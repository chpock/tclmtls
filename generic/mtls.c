/*
 * mtls.c --
 *
 *      This file implements TLS functions
 *
 * Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "mtls.h"
#include <errno.h>

static const char *error_out_of_memory = "Could not initialize error buffer due to out of memory";
static const char *error_unknown = "Unknown error";

static int mtls_ctx_error_init(mtls_ctx *ctx, mtls_ctx_error_type type) {
    if (ctx->error[type] == NULL || ctx->error[type] == error_out_of_memory
        || ctx->error[type] == error_unknown)
    {
        ctx->error[type] = (char *)ckalloc(MTLS_CTX_ERROR_BUFF_SIZE);
    }
    if (ctx->error[type] == NULL) {
        ctx->error[type] = (char *)error_out_of_memory;
        return 0;
    }
    return 1;
}

static void mtls_ctx_error_free(mtls_ctx *ctx, mtls_ctx_error_type type) {
    if (ctx->error[type] != NULL && ctx->error[type] != error_out_of_memory
        && ctx->error[type] != error_unknown)
    {
        ckfree(ctx->error[type]);
        ctx->error[type] = NULL;
    }
    return;
}

void mtls_ctx_error_set(mtls_ctx *ctx, mtls_ctx_error_type type,
    const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    mtls_ctx_error_vset(ctx, type, fmt, args);
    va_end(args);
}

void mtls_ctx_error_vset(mtls_ctx *ctx, mtls_ctx_error_type type,
    const char *fmt, va_list args)
{
    if (!mtls_ctx_error_init(ctx, type)) {
        return;
    }
    vsnprintf(ctx->error[type], MTLS_CTX_ERROR_BUFF_SIZE, fmt, args);
}

const char *mtls_ctx_error_get(mtls_ctx *ctx) {
    if (ctx->error[MTLS_CTX_ERROR_GENERAL] == NULL &&
        ctx->error[MTLS_CTX_ERROR_BACKEND] == NULL &&
        ctx->error[MTLS_CTX_ERROR_IO] == NULL)
    {
        return error_unknown;
    }
    if (ctx->error[MTLS_CTX_ERROR_GENERAL] == NULL) {
        // The general error is not defined.

        // If only one of the IO/backend errors is defined, then return it.
        if (ctx->error[MTLS_CTX_ERROR_BACKEND] == NULL) {
            return ctx->error[MTLS_CTX_ERROR_IO];
        } else if (ctx->error[MTLS_CTX_ERROR_IO] == NULL) {
            return ctx->error[MTLS_CTX_ERROR_BACKEND];
        }
        // If both are defined, try combining them.
        if (!mtls_ctx_error_init(ctx, MTLS_CTX_ERROR)) {
            return ctx->error[MTLS_CTX_ERROR];
        }
        snprintf(ctx->error[MTLS_CTX_ERROR], MTLS_CTX_ERROR_BUFF_SIZE,
            "%s. IO error: %s", ctx->error[MTLS_CTX_ERROR_BACKEND],
            ctx->error[MTLS_CTX_ERROR_IO]);
    } else {
        // The general error is defined.

        // If no IO and backend errors are defined, return only
        // a generic error.
        if (ctx->error[MTLS_CTX_ERROR_BACKEND] == NULL &&
            ctx->error[MTLS_CTX_ERROR_IO] == NULL)
        {
            return ctx->error[MTLS_CTX_ERROR_GENERAL];
        }
        // Try to combine different error types.
        if (!mtls_ctx_error_init(ctx, MTLS_CTX_ERROR)) {
            return ctx->error[MTLS_CTX_ERROR];
        }
        // If only one of the IO/backend errors is defined, then combine it
        // with the general error.
        if (ctx->error[MTLS_CTX_ERROR_BACKEND] == NULL) {
            snprintf(ctx->error[MTLS_CTX_ERROR], MTLS_CTX_ERROR_BUFF_SIZE,
                "%s. IO error: %s", ctx->error[MTLS_CTX_ERROR_GENERAL],
                ctx->error[MTLS_CTX_ERROR_IO]);
        } else if (ctx->error[MTLS_CTX_ERROR_IO] == NULL) {
            snprintf(ctx->error[MTLS_CTX_ERROR], MTLS_CTX_ERROR_BUFF_SIZE,
                "%s. SSL backend: %s", ctx->error[MTLS_CTX_ERROR_GENERAL],
                ctx->error[MTLS_CTX_ERROR_BACKEND]);
        } else {
            snprintf(ctx->error[MTLS_CTX_ERROR], MTLS_CTX_ERROR_BUFF_SIZE,
                "%s. IO error: %s. SSL backend: %s",
                ctx->error[MTLS_CTX_ERROR_GENERAL],
                ctx->error[MTLS_CTX_ERROR_IO],
                ctx->error[MTLS_CTX_ERROR_BACKEND]);
        }
    }
    // If we are here, the error message is in initialized
    // ctx->error[MTLS_CTX_ERROR]. In all other cases, we returned
    // the string earlier.
    return ctx->error[MTLS_CTX_ERROR];
}

int __debug_nested = -1;

void mtls_debug(unsigned char level, int clevel, Tcl_Interp *interp,
    const char *fmt, ...)
{
    va_list args;
    Tcl_Obj *obj;

    // Do nothing if interp is not set
    if (interp == NULL) {
        return;
    }

    // if the current level is -1, then it is undefined. We should try to get it
    // from interp.
    if (clevel == -1) {
        clevel = 0;
        obj = Tcl_GetVar2Ex(interp, __debug[1], NULL, TCL_GLOBAL_ONLY);
        if (obj != NULL) {
            if (Tcl_GetIntFromObj(interp, obj, &clevel) == TCL_ERROR) {
                Tcl_ResetResult(interp);
            }
        }
    }

    // Check the message source. The first 3 bits are for package messages.
    if (level & 7) {
        level = level & 7;
        // The message source is package. Let's check current log level for
        // package messages.
        if (level > (clevel & 7)) {
            // The current log level is not hight enough
            return;
        }
    } else {
        level = (level >> 3) & 7;
        // The message source is backend. Let's check current backend log level.
        if (level > ((clevel >> 3) & 7)) {
            // The current log level for backend messages is not hight enough
            return;
        }
    }
    // Make sure the message level is less than or equal to the maximum level.
    if (level > 5) {
        level = 5;
    }

    // Let's generate the message. Unfortunatelly, Tcl doesn't expose
    // the function AppendPrintfToObjVA. Thus, we have to use standard printf.
    char msg[256];
    va_start(args, fmt);
    vsnprintf(msg, sizeof(msg), fmt, args);
    va_end(args);

    // Below we will execute scripts in interp. Let's preserve it and its state.
    Tcl_InterpState sr = Tcl_SaveInterpState(interp, 0);
    Tcl_Preserve(interp);

    // Check if we have debug callback
    obj = Tcl_GetVar2Ex(interp, "::mtls::debug_callback", NULL,
        TCL_GLOBAL_ONLY);
    if (obj != NULL) {
       // We have a debug callback. Let's construct a command.
       obj = Tcl_DuplicateObj(obj);
       Tcl_ListObjAppendElement(interp, obj, Tcl_NewStringObj(
           "::mtls::debug_callback", -1));
       Tcl_ListObjAppendElement(interp, obj, Tcl_NewIntObj(level));
       Tcl_ListObjAppendElement(interp, obj, Tcl_NewStringObj(msg, -1));

       // The command is ready. Execute it in interp.
       Tcl_IncrRefCount(obj);
       int res = Tcl_EvalObjEx(interp, obj, TCL_EVAL_GLOBAL | TCL_EVAL_DIRECT);
       Tcl_DecrRefCount(obj);

       if (res != TCL_ERROR) {
           // The message was successfully processed by the specified callback.
           // Release the interp and return.
           Tcl_RestoreInterpState(interp, sr);
           Tcl_Release(interp);
           return;
       } else {
           // The callback failed. Let user know about this failure.
           Tcl_BackgroundError(interp);
           // Below we will use the default message handler.
       }

    }

    if (fmt[1] == '>') {
        __debug_nested++;
    }

    char pad[21]; /* max length 20 */
    memset(pad, '.', sizeof(pad));
    pad[(__debug_nested < 0 ? 0 :
        (__debug_nested > 20 ? 20 : __debug_nested))] = 0;

    // We are here because the custom debug message callback is undefined or
    // failed. Let's create a new message with the prefix as the package name
    // and the message's log level.

    // Labels for the debugging level. Since we only use 3 bits for a level,
    // the maximum number of levels is 7. But above we made sure that the level
    // is less than or equal to 5.
    const char *levels[] = { "NON", "ERR", "WRN", "INF", "DBG", "TRC" };
    char pmsg[sizeof(msg)+11+20]; /* '[mtls ...] ' + pad */
    snprintf(pmsg, sizeof(pmsg), "[mtls %s] %s%s", levels[level], pad, msg);

    if (fmt[0] == '<') {
        __debug_nested--;
    }

    // Now we are ready to print the message. Let's use Tcl's printing
    // procedure.
    obj = Tcl_NewObj();
    Tcl_ListObjAppendElement(interp, obj, Tcl_NewStringObj("puts", -1));
    Tcl_ListObjAppendElement(interp, obj, Tcl_NewStringObj("stderr", -1));
    Tcl_ListObjAppendElement(interp, obj, Tcl_NewStringObj(pmsg, -1));

    Tcl_IncrRefCount(obj);
    int res = Tcl_EvalObjEx(interp, obj, TCL_EVAL_GLOBAL | TCL_EVAL_DIRECT);
    Tcl_DecrRefCount(obj);

    if (res != TCL_OK) {
        // Oops. Something is wrong here. Let's report about this background
        // error and cleanup interp state.
        Tcl_BackgroundError(interp);
    }

    // We did the best we could. Release the interp, and hopefully the user
    // got the message.

    Tcl_RestoreInterpState(interp, sr);
    Tcl_Release(interp);
    return;
}

int mtls_bio_write(void *bio, const unsigned char *buf, size_t blen) {
    mtls_ctx *ctx = (mtls_ctx *)bio;
    ENTER(bio_write, ctx->interp);

    Tcl_Channel chan = ctx->upstream_chan;
    Tcl_Size written;
    int eof, err;

    INF("[upstream] want to write [%lu] bytes", blen);

    written = Tcl_WriteRaw(chan, (const char *)buf, (Tcl_Size)blen);

    if ((unsigned int)written == blen) {
        INF("[upstream] successfully wrote [%d] bytes",
            written);
    } else {
        // something wrong

        eof = Tcl_Eof(chan);
        err = Tcl_GetErrno();

        // Do not register this failure if the POSIX error is not set or
        // it is EAGAIN/EWOULDBLOCK (which basically means to try again later).
        if (err != 0 && err != EAGAIN && err != EWOULDBLOCK) {
            mtls_ctx_error_set(ctx, MTLS_CTX_ERROR_IO,
                "unsuccessful write: %s (%d)", Tcl_ErrnoMsg(err), err);
        }

        WRN("[upstream] wrote [%d] bytes [eof:%d err=%s(%d)]",
            written, eof, Tcl_ErrnoMsg(err), err);

        // ask the backend what to do, and return what it decides to return
        written = ctx->backend.bio_error_callback(&ctx->backend,
            MTLS_BIO_WRITE, err, eof, blen, written);
    }

    RETURN(INT, written);
}

int mtls_bio_read(void *bio, unsigned char *buf, size_t blen) {
    mtls_ctx *ctx = (mtls_ctx *)bio;
    ENTER(bio_read, ctx->interp);

    Tcl_Channel chan = ctx->upstream_chan;
    Tcl_Size read;
    int eof, err;

    INF("[upstream] want to read [%lu] bytes", blen);

    if (buf == NULL) {
        WRN("buffer is NULL");
        RETURN(0);
    }

    read = Tcl_ReadRaw(chan, (char *)buf, (Tcl_Size)blen);

    if (read > 0) {
        INF("[upstream] successfully read [%d] bytes", read);
    } else {
        // something wrong

        eof = Tcl_Eof(chan);
        err = Tcl_GetErrno();

        // Do not register this failure if the POSIX error is not set or
        // it is EAGAIN/EWOULDBLOCK (which basically means to try again later).
        if (err != 0 && err != EAGAIN && err != EWOULDBLOCK) {
            mtls_ctx_error_set(ctx, MTLS_CTX_ERROR_IO,
                "unsuccessful read: %s (%d)", Tcl_ErrnoMsg(err), err);
        }

        WRN("[upstream] read [%d] bytes [eof:%d err=%s(%d)]",
            read, eof, Tcl_ErrnoMsg(err), err);

        if (eof) {
            INF("got EOF, returning a connection reset error");
            read = 0;
        } else {
            // ask the backend what to do, and return what it decides to return
            read = ctx->backend.bio_error_callback(&ctx->backend,
                MTLS_BIO_READ, err, eof, blen, read);
        }
    }

    RETURN(INT, read);
}

void mtls_free_config(Tcl_Interp *interp, void *conf);

void mtls_ctx_interp_cleanup(ClientData clientData, Tcl_Interp *interp) {
    mtls_ctx *ctx = (mtls_ctx *)clientData;

    if (ctx->callback != NULL) {
        Tcl_DecrRefCount(ctx->callback);
        ctx->callback = NULL;
    }

    if (ctx->tcl_config != NULL) {
        mtls_free_config(interp, ctx->tcl_config);
        ckfree(ctx->tcl_config);
        ctx->tcl_config = NULL;
    }

    ctx->backend.interp = NULL;
    ctx->interp = NULL;
}

int mtls_ctx_init(
    mtls_ctx **pctx,
    Tcl_Interp *interp,
    Tcl_Channel upstream_chan,
    const char *cadir,
    const char *cafile,
    const char *certfile,
    const char *cert,
    const char *ciphers[],
    int cipherslen,
    const char *alpn[],
    int alpnlen,
    Tcl_Obj *command,
    const char *dhparams,
    const char *keyfile,
    const char *key,
    const char *keypassword,
    int request,
    int require,
    int server,
    const char *servername,
    int tls1_1,
    int tls1_2,
    int tls1_3
) {
    ENTER(ctx_init, interp);

    mtls_ctx *ctx;

    DBG("alloc a new context");
    ctx = (mtls_ctx *)ckalloc(sizeof(mtls_ctx));
    *pctx = ctx;

    if (ctx == NULL) {
        RETURN(ERROR);
    }

    ctx->tcl_config = NULL;
    ctx->watchMask = 0;
    ctx->timer = NULL;
    ctx->state = MTLS_CTX_STATE_PREINIT;
    ctx->upstream_chan = upstream_chan;
    ctx->interp = interp;
    ctx->callback = command;
    if (command != NULL) {
        Tcl_IncrRefCount(command);
    }
    for (int i = 0; i < MTLS_CTX_ERROR_SIZE; i++) {
        ctx->error[i] = NULL;
    }

    // Setup the callback for deleted interpreter to correctly clear it
    // from ctx->interp and avoid other callbacks using unavailable interp.
    Tcl_CallWhenDeleted(interp, mtls_ctx_interp_cleanup, (ClientData) ctx);

    if (mtls_backend_ctx_init(
        &ctx->backend,
        interp,
        ctx,
        cadir,
        cafile,
        certfile,
        cert,
        ciphers,
        cipherslen,
        alpn,
        alpnlen,
        dhparams,
        keyfile,
        key,
        keypassword,
        request,
        require,
        server,
        servername,
        tls1_1,
        tls1_2,
        tls1_3
    ) != TCL_OK) {
        mtls_ctx_error_set(ctx, MTLS_CTX_ERROR_GENERAL, "Could not initialize"
            " SSL backend");
        ctx->state = MTLS_CTX_STATE_ERROR;
        RETURN(ERROR);
    }

    ctx->state = MTLS_CTX_STATE_INIT;
    RETURN(OK);
}

int mtls_ctx_connect(mtls_ctx *ctx) {
    ENTER(ctx_connect, ctx->interp);

    switch (ctx->state) {
    case MTLS_CTX_STATE_PREINIT:
        ERR("cannot start handshake in pre-init state");
        RETURN(ERROR);
        break;
    case MTLS_CTX_STATE_HANDSHAKE:
        DBG("handshake mode already started, continue it");
        break;
    case MTLS_CTX_STATE_CONNECTED:
        INF("ctx is in the connected state");
        RETURN(OK);
        break;
    case MTLS_CTX_STATE_ERROR:
        ERR("ctx is in error state");
        RETURN(ERROR);
        break;
    case MTLS_CTX_STATE_INIT:
        // this is the expected state where we can start handshake
        break;
    }

    ctx->state = MTLS_CTX_STATE_HANDSHAKE;

    int res = mtls_backend_ctx_connect(&ctx->backend);

    if (res == TCL_OK) {
        // Everything is ok. Handshake was successful.
        ctx->state = MTLS_CTX_STATE_CONNECTED;
        RETURN(OK);
    } else if (res == TCL_CONTINUE) {
        // Everything is not ok. We need something else to complete
        // the handshake. Let's try it later.
        RETURN(CONTINUE);
    }

    // Something bad happened.

    mtls_ctx_error_set(ctx, MTLS_CTX_ERROR_GENERAL, "Handshake failed");
    ctx->state = MTLS_CTX_STATE_ERROR;
    RETURN(ERROR);
}

int mtls_ctx_close(mtls_ctx *ctx) {
    ENTER(ctx_close, ctx->interp);

    // Gracefully close the channel if it is in a connected state
    if (ctx->state == MTLS_CTX_STATE_CONNECTED) {
        mtls_backend_ctx_close(&ctx->backend);
    }

    if (ctx->timer != NULL) {
        // Remove pending events
        Tcl_DeleteTimerHandler(ctx->timer);
    }
    ctx->timer = NULL;

    ctx->flags |= MTLS_CTX_FLAG_CLOSED;

    RETURN(OK);
}

int mtls_ctx_free(mtls_ctx *ctx) {
    ENTER(ctx_free, ctx->interp);

    if (ctx != NULL) {

        if (!(ctx->flags & MTLS_CTX_FLAG_CLOSED)) {
            mtls_ctx_close(ctx);
        }

        mtls_backend_ctx_free(&ctx->backend);

        if (ctx->interp != NULL) {
            Tcl_DontCallWhenDeleted(ctx->interp, mtls_ctx_interp_cleanup,
                (ClientData) ctx);
            mtls_ctx_interp_cleanup((ClientData) ctx, ctx->interp);
        }

        for (int i = 0; i < MTLS_CTX_ERROR_SIZE; i++) {
            mtls_ctx_error_free(ctx, i);
        }

        ckfree(ctx);
        ctx = NULL;

    }

    RETURN(OK);
}

static void mtls_get_ciphers_push(Tcl_Interp *interp, const char *cipher,
    ClientData obj)
{
    Tcl_ListObjAppendElement(interp, (Tcl_Obj *)obj,
        Tcl_NewStringObj(cipher, -1));
    return;
}

int mtls_get_ciphers(Tcl_Interp *interp, mtls_protocol protocol,
    int verbose, int supported, Tcl_Obj *obj)
{
    ENTER(mtls_get_chiphers, interp);

    if (mtls_backend_get_ciphers(interp, protocol, verbose, supported,
        mtls_get_ciphers_push, (ClientData)obj) != TCL_OK)
    {
        SET_RESULT(FORMAT, "%s: protocol not supported",
            mtls_protocol_strings[protocol]);
        SET_ERROR("WRONGPROTO");
        RETURN(ERROR);
    }

    RETURN(OK);
}

int mtls_get_version(Tcl_Interp *interp, Tcl_Obj *obj) {
    ENTER(mtls_get_version, interp);
    Tcl_SetStringObj(obj, mtls_backend_get_version(interp), -1);
    RETURN(OK);
}

int mtls_ctx_get_status(mtls_ctx *ctx, int is_local, Tcl_Obj *obj) {
    ENTER(mtls_get_status, ctx->interp);

    if (ctx->state != MTLS_CTX_STATE_CONNECTED) {
        TRC("socket is not in connected state, return empty list");
        RETURN(OK);
    }

    UNUSED(is_local);

    Tcl_DictObjPut(ctx->interp, obj, Tcl_NewStringObj("version", -1),
        mtls_backend_ctx_get_status(&ctx->backend,
        MTLS_BACKEND_CTX_PROTOCOL));

    Tcl_DictObjPut(ctx->interp, obj, Tcl_NewStringObj("cipher", -1),
        mtls_backend_ctx_get_status(&ctx->backend,
        MTLS_BACKEND_CTX_CIPHER));

    Tcl_DictObjPut(ctx->interp, obj, Tcl_NewStringObj("sbits", -1),
        mtls_backend_ctx_get_status(&ctx->backend,
        MTLS_BACKEND_CTX_CHIPHER_KEY_BITLEN));

    UNUSED(obj);

    RETURN(OK);
}

int mtls_init(Tcl_Interp *interp) {
    ENTER(mtls_init, interp);

    if (mtls_backend_init(interp) != TCL_OK) {
        RETURN(ERROR);
    }

    RETURN(OK);
}

int mtls_free(Tcl_Interp *interp) {
    ENTER(mtls_free, interp);

    if (mtls_backend_free(interp) != TCL_OK) {
        RETURN(ERROR);
    }

    RETURN(OK);
}
