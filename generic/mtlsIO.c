/*
 * mtlsIO.c --
 *
 *      This file implements channel IO functions
 *
 * Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "mtlsIO.h"
#include "mtls.h"
#include <errno.h>

static int mtls_CloseProc(ClientData instanceData, Tcl_Interp *interp) {
    mtls_ctx *ctx = (mtls_ctx *)instanceData;
    UNUSED(interp);
    ENTER(CloseProc, ctx->interp);

    mtls_ctx_close(ctx);
    Tcl_EventuallyFree((ClientData)ctx, mtls_ctx_free);

    RETURN(0);
}

static int mtls_Close2Proc(ClientData instanceData, Tcl_Interp *interp,
    int flags)
{
    mtls_ctx *ctx = (mtls_ctx *)instanceData;
    UNUSED(interp);
    ENTER(Close2Proc, ctx->interp);

    if ((flags & (TCL_CLOSE_READ|TCL_CLOSE_WRITE)) == 0) {
        int ret = mtls_CloseProc(instanceData, interp);
        RETURN(INT, ret);
    }

    RETURN(POSIX_EINVAL);
}

static int mtls_InputProc(ClientData instanceData, char *buf, int bufSize,
    int *errorCodePtr)
{
    mtls_ctx *ctx = (mtls_ctx *)instanceData;
    ENTER(InputProc, ctx->interp);

    // Tcl stack for reading:
    //    Tcl_ReadObjCmd()
    //    Tcl_ReadChars() (returns result of DoReadChars())
    //    DoReadChars() (returns number of read bytes, or 0 if channel
    //                   blocked, or -1 if channel is not blocked and error)
    //    GetInput() (returns 0 as successful, or errorCode)
    //    ChanRead() (returns number of read bytes)

    // Tcl core has the following logic in ChanRead() when processing
    // the results of this function:
    //
    //     if result > 0:
    //         if result < bufSize: set CHANNEL_BLOCKED
    //     if result = 0:
    //         assume that the EOF has reached (set CHANNEL_EOF flag)
    //     if result < 0:
    //         if errorCode = EWOULDBLOCK or EAGAIN:
    //             set CHANNEL_BLOCKED
    //             set errorCode as EAGAIN
    //         if errorCode anything else:
    //             use that errorCode as IO error
    //
    // In GetInput()
    //
    //     if nread < 0
    //         return errorCode
    //     if nread = 0 or > 0
    //         return 0 and add nread to buffer
    //
    // In DoReadChars()
    //
    //     if result = 0
    //         continue to read until all buffer read
    //     if result != 0 and CHANNEL_BLOCKED
    //         return 0
    //     if result != 0 and not CHANNEL_BLOCKED
    //         return -1
    //
    // In Tcl_ReadObjCmd()
    //
    //     if result < 0
    //         report about error
    //     if result = 0 or result > 0
    //         return read bytes

again:

    DBG("[ssl] want to read [%d] bytes", bufSize);
    *errorCodePtr = 0;
    int read = 0;

    int conn = mtls_ctx_connect(ctx);

    if (conn == TCL_CONTINUE) {
        // Handshake is not completed. Let's try again later.
        DBG("handshake is incomplete, let's try it later");
        read = -1;
        *errorCodePtr = EAGAIN;
    } else if (conn == TCL_OK) {
        read = mtls_backend_ctx_read(&ctx->backend, buf, bufSize,
            errorCodePtr);
    } else {
        // We are not connected
        read = -1;
        *errorCodePtr = EPROTO;
    }

    // If we are in blocking mode and we get EAGAIN, then block until
    // we actually get something.
    if (!(ctx->flags & MTLS_CTX_FLAG_ASYNC) && read == -1 &&
        *errorCodePtr == EAGAIN)
    {
        TRC("continue reading in blocking mode");
        goto again;
    }

    // Consider only EAGAIN as a temporary failure. All other errors
    // must be critical.
    if ((read >= 0 && *errorCodePtr == 0) || *errorCodePtr == EAGAIN) {
        DBG("[ssl] read [%d] bytes, posix code: %s (%d)", read,
            Tcl_ErrnoMsg(*errorCodePtr), *errorCodePtr);
    } else {
        DBG("set an error mode for the channel");
        ctx->state = MTLS_CTX_STATE_ERROR;

        Tcl_Obj *errObj = Tcl_NewListObj(0, NULL);
        Tcl_ListObjAppendElement(NULL, errObj,
            Tcl_NewStringObj("-errorcode", -1));
        Tcl_ListObjAppendElement(NULL, errObj, Tcl_NewIntObj(read));
        Tcl_ListObjAppendElement(NULL, errObj,
            Tcl_NewStringObj(mtls_ctx_error_get(ctx), -1));
        Tcl_SetChannelError(ctx->self_chan, errObj);
    }

    if (*errorCodePtr != 0) {
        DBG("return posix code: %s (%d)", Tcl_ErrnoMsg(*errorCodePtr),
            *errorCodePtr);
    }

    RETURN(INT, read);
}

static int mtls_OutputProc(ClientData instanceData, const char *buf,
    int bufSize, int *errorCodePtr)
{
    mtls_ctx *ctx = (mtls_ctx *)instanceData;
    ENTER(OutputProc, ctx->interp);

    DBG("[ssl] want to write [%d] bytes", bufSize);
    *errorCodePtr = 0;
    int written = 0;

    int conn = mtls_ctx_connect(ctx);

    if (conn == TCL_CONTINUE) {
        // Handshake is not completed. Let's try again later.
        DBG("handshake is incomplete, let's try it later");
        written = -1;
        *errorCodePtr = EAGAIN;
        RETURN(INT, written);
    } else if (conn == TCL_OK) {
        written = mtls_backend_ctx_write(&ctx->backend, buf, bufSize, errorCodePtr);
    } else {
        // We are not connected
        written = -1;
        *errorCodePtr = EPROTO;
    }

    // Consider only EAGAIN as a temporary failure. All other errors
    // must be critical.
    if ((written >= 0 && *errorCodePtr == 0) || *errorCodePtr == EAGAIN) {
        DBG("[ssl] wrote [%d] bytes, posix code: %s (%d)", written,
            Tcl_ErrnoMsg(*errorCodePtr), *errorCodePtr);
    } else {
        DBG("set an error mode for the channel");
        ctx->state = MTLS_CTX_STATE_ERROR;

        Tcl_Obj *errObj = Tcl_NewListObj(0, NULL);
        Tcl_ListObjAppendElement(NULL, errObj,
            Tcl_NewStringObj("-errorcode", -1));
        Tcl_ListObjAppendElement(NULL, errObj, Tcl_NewIntObj(written));
        Tcl_ListObjAppendElement(NULL, errObj,
            Tcl_NewStringObj(mtls_ctx_error_get(ctx), -1));
        Tcl_SetChannelError(ctx->self_chan, errObj);
    }

    if (*errorCodePtr != 0) {
        DBG("return posix code: %s (%d)", Tcl_ErrnoMsg(*errorCodePtr),
            *errorCodePtr);
    }

    RETURN(INT, written);
}

static int mtls_SetOptionProc(ClientData instanceData, Tcl_Interp *interp,
    const char *optionName, const char *optionValue)
{
    mtls_ctx *ctx = (mtls_ctx *)instanceData;
    // interp in function args can be NULL
    ENTER(SetOptionProc, ctx->interp);

    int ret;

    Tcl_DriverSetOptionProc *setOptionProc;
    setOptionProc = Tcl_ChannelSetOptionProc(
        Tcl_GetChannelType(ctx->upstream_chan));

    if (setOptionProc != NULL) {
        TRC("call upstream setOption with optionName[%s]"
            " = optionValue[%s]", optionName, optionValue);
        ret = (*setOptionProc)(Tcl_GetChannelInstanceData(ctx->upstream_chan),
            interp, optionName, optionValue);
    } else {
        ERR("unknown option specified [%s] = [%s]", optionName, optionValue);
        ret = Tcl_BadChannelOption(interp, optionName, "");
    }

    RETURN(INT, ret);
}

static int mtls_GetOptionProc(ClientData instanceData, Tcl_Interp *interp,
    const char *optionName, Tcl_DString *optionValue)
{
    mtls_ctx *ctx = (mtls_ctx *)instanceData;
    // interp in function args can be NULL
    ENTER(GetOptionProc, ctx->interp);

    int ret;

    Tcl_DriverGetOptionProc *getOptionProc;
    getOptionProc = Tcl_ChannelGetOptionProc(
        Tcl_GetChannelType(ctx->upstream_chan));

    if (getOptionProc != NULL) {
        TRC("call upstream getOption with optionName[%s]", optionName);
        ret = (*getOptionProc)(Tcl_GetChannelInstanceData(ctx->upstream_chan),
            interp, optionName, optionValue);
    } else if (optionName == NULL) {
        TRC("request is query for all options");
        // Request is query for all options, this is ok.
        ret = TCL_OK;
    } else {
        ret = Tcl_BadChannelOption(interp, optionName, "");
    }

    RETURN(INT, ret);
}

static int mtls_GetHandleProc(ClientData instanceData, int direction,
   ClientData *handlePtr)
{
    mtls_ctx *ctx = (mtls_ctx *)instanceData;
    ENTER(GetHandleProc, ctx->interp);

    int ret = Tcl_GetChannelHandle(ctx->upstream_chan, direction,
        handlePtr);

    RETURN(INT, ret);
}

static int mtls_BlockModeProc(ClientData instanceData, int mode) {
    mtls_ctx *ctx = (mtls_ctx *)instanceData;
    ENTER(BlockModeProc, ctx->interp);

    if (mode == TCL_MODE_NONBLOCKING) {
        INF("set NON blocking mode");
        ctx->flags |= MTLS_CTX_FLAG_ASYNC;
    } else {
        INF("set blocking mode");
        ctx->flags &= ~(MTLS_CTX_FLAG_ASYNC);
    }

    RETURN(0);
}

static void mtls_TimerHandlerProc(ClientData clientData) {
    mtls_ctx *ctx = (mtls_ctx *)clientData;
    ENTER(TimerHandlerProc, ctx->interp);

    // Save what events we have coming up
    int mask = ctx->watchMask;

    // Cleanup event queue
    ctx->timer = NULL;
    ctx->watchMask = 0;

    if (mask & TCL_READABLE) {
       DBG("(%s) trigger the READABLE event",
           Tcl_GetChannelName(ctx->self_chan));
    }
    if (mask & TCL_WRITABLE) {
       DBG("(%s) trigger the WRITABLE event",
           Tcl_GetChannelName(ctx->self_chan));
    }

    Tcl_NotifyChannel(ctx->self_chan, mask);

    RETURN();
}

static int mtls_HandlerProc(ClientData instanceData, int mask) {
    mtls_ctx *ctx = (mtls_ctx *)instanceData;
    ENTER(HandlerProc, ctx->interp);

    if (mask & TCL_READABLE) {
       DBG("(%s) got the READABLE event",
           Tcl_GetChannelName(ctx->self_chan));
    }
    if (mask & TCL_WRITABLE) {
       DBG("(%s) got the WRITABLE event",
           Tcl_GetChannelName(ctx->self_chan));
    }

    if (ctx->watchMask & mask) {
        DBG("the event is already queued, ignore it");
        RETURN(0);
    }

    ctx->watchMask |= mask;

    DBG("queue up event in our channel");
    // Delete the previous event if it exists
    if (ctx->timer != NULL) {
        Tcl_DeleteTimerHandler(ctx->timer);
        ctx->timer = NULL;
    }

    ctx->timer = Tcl_CreateTimerHandler(1, mtls_TimerHandlerProc,
        instanceData);

    // The handler must return zero to stop processing the event.
    RETURN(0);
}

static void mtls_WatchProc(ClientData instanceData, int mask) {
    mtls_ctx *ctx = (mtls_ctx *)instanceData;
    ENTER(WatchProc, ctx->interp);

    TRC("watch mask [%d]", mask);

    if (mask & TCL_READABLE) {
        DBG("(%s) want to watch READABLE event",
            Tcl_GetChannelName(ctx->self_chan));
    }
    if (mask & TCL_WRITABLE) {
       DBG("(%s) want to watch WRITABLE event",
           Tcl_GetChannelName(ctx->self_chan));
    }

    Tcl_DriverWatchProc *watchProc = Tcl_ChannelWatchProc(Tcl_GetChannelType(
        ctx->upstream_chan));

    TRC("register watch callback on the upstream channel (%p)",
        ctx->upstream_chan);
    watchProc(Tcl_GetChannelInstanceData(ctx->upstream_chan), mask);

    RETURN();
}

static const Tcl_ChannelType mtlsChannelType = {
    "mtls",                /* Type name */
    TCL_CHANNEL_VERSION_5, /* v5 channel */
    mtls_CloseProc,        /* Close proc */
    mtls_InputProc,        /* Input proc */
    mtls_OutputProc,       /* Output proc */
    NULL,                  /* Seek proc */
    mtls_SetOptionProc,    /* Set option proc */
    mtls_GetOptionProc,    /* Get option proc */
    mtls_WatchProc,        /* Initialize notifier */
    mtls_GetHandleProc,    /* Get OS handles out of channel */
    mtls_Close2Proc,       /* close2proc */
    mtls_BlockModeProc,    /* Set blocking/nonblocking mode*/
    NULL,                  /* Flush proc */
    mtls_HandlerProc,      /* Handling of events bubbling up */
    NULL,                  /* Wide seek proc */
    NULL,                  /* Thread action */
    NULL                   /* Truncate */
};

const Tcl_ChannelType *mtls_ChannelType(void) {
    return &mtlsChannelType;
}
