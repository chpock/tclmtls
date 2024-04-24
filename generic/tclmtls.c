/*
 * tclmtls.c --
 *
 *      This file implements a Tcl interface to the TLS functions
 *      in mtls.c
 *
 * Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "mtlsUuid.h"
#include "tclmtls.h"
#include "mtls.h"
#include "mtlsIO.h"
#include <sys/stat.h> /* for S_ISDIR() in Mtls_Init */

static void mtls_set_config(Tcl_Interp *interp, mtls_config *conf,
    mtls_config_name name, Tcl_Obj *val)
{
    ENTER(set_config, interp);
    if ((*conf)[name] != NULL) {
        Tcl_DecrRefCount((*conf)[name]);
    }
    (*conf)[name] = val;
    if ((*conf)[name] != NULL) {
        TRC("set config element %s to value [%s]",
            mtls_config_strings[name], Tcl_GetString((*conf)[name]));
        Tcl_IncrRefCount((*conf)[name]);
    } else {
        TRC("unset config element %s", mtls_config_strings[name]);
    }
    RETURN();
}

static void mtls_init_config(Tcl_Interp *interp, mtls_config *conf) {
    ENTER(init_config, interp);
    for (unsigned int i = 0; i < MTLS_CONF_SIZE; i++) {
        (*conf)[i] = NULL;
    }
    RETURN();
}

static void mtls_import_config(Tcl_Interp *interp, mtls_config *sconf,
    mtls_config *dconf, mtls_import_config_type type)
{
    ENTER(import_config, interp);
    if (type == MTLS_IMPORT_CONFIG_MERGE) {
        if ((*sconf)[MTLS_CONF_CERTFILE] != NULL ||
            (*sconf)[MTLS_CONF_CERT] != NULL)
        {
            if ((*dconf)[MTLS_CONF_CERTFILE] != NULL) {
                mtls_set_config(interp, dconf, MTLS_CONF_CERTFILE, NULL);
            }
            if ((*dconf)[MTLS_CONF_CERT] != NULL) {
                mtls_set_config(interp, dconf, MTLS_CONF_CERT, NULL);
            }
        }
        if ((*sconf)[MTLS_CONF_KEYFILE] != NULL ||
            (*sconf)[MTLS_CONF_KEY] != NULL)
        {
            if ((*dconf)[MTLS_CONF_KEYFILE] != NULL) {
                mtls_set_config(interp, dconf, MTLS_CONF_KEYFILE, NULL);
            }
            if ((*dconf)[MTLS_CONF_KEY] != NULL) {
                mtls_set_config(interp, dconf, MTLS_CONF_KEY, NULL);
            }
        }
    }
    for (unsigned int i = 0; i < MTLS_CONF_SIZE; i++) {
        if (type == MTLS_IMPORT_CONFIG_UPDATE) {
            if ((*sconf)[i] == NULL || (*dconf)[i] != NULL) {
                continue;
            }
            if ((i == MTLS_CONF_CERTFILE || i == MTLS_CONF_CERT) &&
                (*sconf)[i] != NULL &&
                ((*dconf)[MTLS_CONF_CERTFILE] != NULL || (*dconf)[MTLS_CONF_CERT] != NULL))
            {
                continue;
            }
            if ((i == MTLS_CONF_KEYFILE || i == MTLS_CONF_KEY) &&
                (*sconf)[i] != NULL &&
                ((*dconf)[MTLS_CONF_KEYFILE] != NULL || (*dconf)[MTLS_CONF_KEY] != NULL))
            {
                continue;
            }
        } else if (type == MTLS_IMPORT_CONFIG_MERGE) {
            if ((*sconf)[i] == NULL) {
                continue;
            }
        }
        mtls_set_config(interp, dconf, i, (*sconf)[i]);
    }
    for (unsigned int i = 0; i < MTLS_CONF_SIZE; i++) {
        if ((*dconf)[i] != NULL && Tcl_GetCharLength((*dconf)[i]) == 0) {
            mtls_set_config(interp, dconf, i, NULL);
        }
    }
    RETURN();
}

void mtls_free_config(Tcl_Interp *interp, mtls_config *conf) {
    ENTER(free_config, interp);
    for (unsigned int i = 0; i < MTLS_CONF_SIZE; i++) {
        if ((*conf)[i] != NULL) {
            Tcl_DecrRefCount((*conf)[i]);
            (*conf)[i] = NULL;
        }
    }
    RETURN();
}

#define _PARSE_PARAM(o) \
    TRC("got option %s with value '%s'", mtls_config_strings[o], Tcl_GetString(val)); \
    if (conf != NULL) { \
        local_conf[o] = val; \
        Tcl_IncrRefCount(val); \
    } \
    break

#define PARSE_PARAM_1(o) \
    case o: \
        _PARSE_PARAM(o)

#define PARSE_PARAM_N(t, o) __XCONCAT(PARSE_PARAM, t)(o)

#define PARSE_PARAM_BOOL(o) \
    case o: \
        { int bVal; \
        if (Tcl_GetBooleanFromObj(interp, val, &bVal) != TCL_OK) { \
            Tcl_SetObjResult(interp, Tcl_ObjPrintf("boolean value expected" \
                " for option %s but got '%s' instead", mtls_config_strings[o], \
                Tcl_GetString(val))); \
            Tcl_SetErrorCode(interp, "MTLS", "BADARG", NULL); \
            goto error; \
        } } \
        _PARSE_PARAM(o)

#define PARSE_PARAM_CHAN(o) \
    case o: \
        if (Tcl_GetChannel(interp, Tcl_GetString(val), NULL) == NULL) { \
            Tcl_SetObjResult(interp, Tcl_ObjPrintf("channel name expected" \
               " for option %s but got '%s' instead", mtls_config_strings[o], \
               Tcl_GetString(val))); \
            Tcl_SetErrorCode(interp, "MTLS", "BADARG", NULL); \
            goto error; \
        } \
        _PARSE_PARAM(o)

#define PARSE_PARAM_DEPRECATED(o) \
    case o: \
        WRN("deprecated and unused option %s is specified", mtls_config_strings[o]); \
        break

#define PARSE_PARAM_UNSUPPORTED(o) \
    case o: \
        WRN("unsupported option %s is specified", mtls_config_strings[o]); \
        break

#define PARSE_PARAM(...) __EVAL_CONCAT(PARSE_PARAM, _NARGS(__VA_ARGS__))(__VA_ARGS__)

static int mtls_parse_parameters(Tcl_Interp *interp, int start_index, int objc,
    Tcl_Obj *const objv[], mtls_config *conf, mtls_import_config_type type)
{
    ENTER(parse_parameters, interp);

    TRC("start index:%d, length:%d", start_index, objc);

    mtls_config local_conf;

    // We will only use the local config if storing parsed values is requested,
    // i.e. when conf != NULL
    if (conf != NULL) {
        mtls_init_config(interp, &local_conf);
    }

    int idx;
    for (idx = start_index; idx < objc; idx++) {

        int opt;

        // Check to see if this option is known
        if (Tcl_GetIndexFromObj(interp, objv[idx], mtls_config_strings,
            "option", TCL_EXACT, &opt) != TCL_OK) {
            goto error;
        }
        // Increase the option index and check if the options has a parameter
        if (++idx == objc) {
            SET_RESULT(FORMAT, "missing argument to %s option",
                mtls_config_strings[opt]);
            SET_ERROR("NOVAL");
            goto error;
        }

        Tcl_Obj *val = objv[idx];

        switch ((mtls_config_name) opt) {
            PARSE_PARAM(MTLS_CONF_CADIR);
            PARSE_PARAM(MTLS_CONF_CAFILE);
            PARSE_PARAM(MTLS_CONF_CERTFILE);
            PARSE_PARAM(MTLS_CONF_CERT);
            PARSE_PARAM(MTLS_CONF_CIPHER);
            PARSE_PARAM(MTLS_CONF_ALPN);
            PARSE_PARAM(MTLS_CONF_COMMAND);
            PARSE_PARAM(MTLS_CONF_DHPARAMS);
            PARSE_PARAM(MTLS_CONF_KEYFILE);
            PARSE_PARAM(MTLS_CONF_KEY);
            PARSE_PARAM(CHAN, MTLS_CONF_MODEL);
            PARSE_PARAM(MTLS_CONF_PASSWORD);
            PARSE_PARAM(BOOL, MTLS_CONF_REQUEST);
            PARSE_PARAM(BOOL, MTLS_CONF_REQUIRE);
            PARSE_PARAM(BOOL, MTLS_CONF_SERVER);
            PARSE_PARAM(MTLS_CONF_SERVERNAME);
            PARSE_PARAM(DEPRECATED, MTLS_CONF_SSL2);
            PARSE_PARAM(DEPRECATED, MTLS_CONF_SSL3);
            PARSE_PARAM(DEPRECATED, MTLS_CONF_TLS1);
            PARSE_PARAM(DEPRECATED, MTLS_CONF_CIPHERSUITES);
            PARSE_PARAM(BOOL, MTLS_CONF_TLS1_1);
            PARSE_PARAM(BOOL, MTLS_CONF_TLS1_2);
            PARSE_PARAM(BOOL, MTLS_CONF_TLS1_3);
            PARSE_PARAM(BOOL, MTLS_CONF_AUTOSERVERNAME);
            PARSE_PARAM(UNSUPPORTED, MTLS_CONF_POST_HANDSHAKE);
            PARSE_PARAM(UNSUPPORTED, MTLS_CONF_SECURITYLEVEL);
            PARSE_PARAM(UNSUPPORTED, MTLS_CONF_SESSION_ID);
            PARSE_PARAM(UNSUPPORTED, MTLS_CONF_VALIDATECOMMAND);
            PARSE_PARAM(UNSUPPORTED, MTLS_CONF_VCMD);
            PARSE_PARAM(MTLS_CONF_ACCEPT);
            case MTLS_CONF_SIZE:
                break;
        }

    }

    if (local_conf[MTLS_CONF_CERTFILE] != NULL &&
        local_conf[MTLS_CONF_CERT] != NULL)
    {
        SET_RESULT(FORMAT, "both %s and %s options are specified, but only"
            " one of them is allowed", mtls_config_strings[MTLS_CONF_CERTFILE],
            mtls_config_strings[MTLS_CONF_CERT]);
        SET_ERROR("INVAL");
        goto error;
    }

    if (local_conf[MTLS_CONF_KEYFILE] != NULL &&
        local_conf[MTLS_CONF_KEY] != NULL)
    {
        SET_RESULT(FORMAT, "both %s and %s options are specified, but only"
            " one of them is allowed", mtls_config_strings[MTLS_CONF_KEYFILE],
            mtls_config_strings[MTLS_CONF_KEY]);
        SET_ERROR("INVAL");
        goto error;
    }

    if (conf != NULL) {
        mtls_import_config(interp, &local_conf, conf, type);
        mtls_free_config(interp, &local_conf);
    }

    RETURN(OK);

error:
    if (conf != NULL) {
        mtls_free_config(interp, &local_conf);
    }
    RETURN(ERROR);
}

static int mtls_cmd_version(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    UNUSED(clientData);
    ENTER(cmd_version, interp);

    if (objc != 1) {
        Tcl_WrongNumArgs(interp, 1, objv, "");
        RETURN(ERROR);
    }

    Tcl_Obj *obj = Tcl_NewObj();
    Tcl_IncrRefCount(obj);
    mtls_get_version(interp, obj);
    SET_RESULT(OBJECT, obj);
    Tcl_DecrRefCount(obj);

    RETURN(OK);
}

static int mtls_cmd_protocols(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    UNUSED(clientData);
    ENTER(cmd_protocols, interp);

    if (objc != 1) {
        Tcl_WrongNumArgs(interp, 1, objv, "");
        RETURN(ERROR);
    }

    Tcl_Obj *obj = Tcl_NewListObj(0, NULL);

#ifdef MTLS_BACKEND_FEATURE_SSL2
    Tcl_ListObjAppendElement(interp, obj,
        Tcl_NewStringObj(mtls_protocol_strings[MTLS_PROTO_SSL2], -1));
#endif
#ifdef MTLS_BACKEND_FEATURE_SSL3
    Tcl_ListObjAppendElement(interp, obj,
        Tcl_NewStringObj(mtls_protocol_strings[MTLS_PROTO_SSL3], -1));
#endif
#ifdef MTLS_BACKEND_FEATURE_TLS1
    Tcl_ListObjAppendElement(interp, obj,
        Tcl_NewStringObj(mtls_protocol_strings[MTLS_PROTO_TLS1], -1));
#endif
#ifdef MTLS_BACKEND_FEATURE_TLS1_1
    Tcl_ListObjAppendElement(interp, obj,
        Tcl_NewStringObj(mtls_protocol_strings[MTLS_PROTO_TLS1_1], -1));
#endif
#ifdef MTLS_BACKEND_FEATURE_TLS1_2
    Tcl_ListObjAppendElement(interp, obj,
        Tcl_NewStringObj(mtls_protocol_strings[MTLS_PROTO_TLS1_2], -1));
#endif
#ifdef MTLS_BACKEND_FEATURE_TLS1_3
    Tcl_ListObjAppendElement(interp, obj,
        Tcl_NewStringObj(mtls_protocol_strings[MTLS_PROTO_TLS1_3], -1));
#endif

    SET_RESULT(OBJECT, obj);
    RETURN(OK);
}

static int mtls_cmd_ciphers(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    UNUSED(clientData);
    ENTER(cmd_ciphers, interp);

    if ((objc < 2) || (objc > 4)) {
        Tcl_WrongNumArgs(interp, 1, objv, "protocol ?verbose? ?supported?");
        RETURN(ERROR);
    }

    int protocol;
    if (Tcl_GetIndexFromObj(interp, objv[1], mtls_protocol_strings, "protocol",
        0, &protocol) != TCL_OK)
    {
        SET_RESULT(FORMAT, "unknown protocol \"%s\" specified",
            Tcl_GetString(objv[1]));
        SET_ERROR("WRONGARGS");
        RETURN(ERROR);
    }

    int verbose = 0;
    if ((objc > 2) && Tcl_GetBooleanFromObj(interp, objv[2], &verbose) != TCL_OK) {
        SET_RESULT(FORMAT, "expected boolean value for verbose arg but"
            " got \"%s\"", Tcl_GetString(objv[2]));
        SET_ERROR("WRONGARGS");
        RETURN(ERROR);
    }

    int supported = 0;
    if ((objc > 3) && Tcl_GetBooleanFromObj(interp, objv[3],
        &supported) != TCL_OK)
    {
        SET_RESULT(FORMAT, "expected boolean value for supported arg but"
            " got \"%s\"", Tcl_GetString(objv[3]));
        SET_ERROR("WRONGARGS");
        RETURN(ERROR);
    }

    Tcl_Obj *obj = Tcl_NewObj();
    Tcl_IncrRefCount(obj);
    if (mtls_get_ciphers(interp, (mtls_protocol)protocol, verbose,
        supported, obj) != TCL_OK)
    {
        // We have an error message in the Tcl interp from the above func
        Tcl_DecrRefCount(obj);
        RETURN(ERROR);
    }

    SET_RESULT(OBJECT, obj);
    Tcl_DecrRefCount(obj);
    RETURN(OK);
}

static int mtls_cmd_debug(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    UNUSED(clientData);
    ENTER(cmd_debug, interp);

    int level = 0;
    int backend_level = 0;
    Tcl_Obj *obj;

    static const char *const levels[] = {
        "none", "error", "warning", "info", "debug", "trace", NULL
    };

    if (objc > 3) {
        Tcl_WrongNumArgs(interp, 1, objv, "?level? ?backend_level?");
        RETURN(ERROR);
    }

    // Set the package error level
    if (objc > 1) {

        if (Tcl_GetIntFromObj(interp, objv[1], &level) == TCL_OK) {
            // The first argument is a number. Let's check if we have
            // 2nd argument. This sould be considered as an error.
            if (objc > 2) {
                SET_RESULT(FORMAT, "backend_level should not be specified"
                    " if the first parameter is an integer");
                SET_ERROR("WRONGARGS");
                RETURN(ERROR);
            }
        } else {
            if (Tcl_GetIndexFromObj(interp, objv[1], levels, "level", 0,
                &level) != TCL_OK) {
                RETURN(ERROR);
            }
        }

        // Set the backend error level
        if (objc > 2) {
            if (Tcl_GetIndexFromObj(interp, objv[1], levels, "backend_level", 0,
                &backend_level) != TCL_OK) {
                RETURN(ERROR);
            }
            level += backend_level << 3;
        }

        // Set the level to the Tcl variable
        Tcl_SetVar2Ex(interp, __debug[1], NULL, Tcl_NewIntObj(level),
            TCL_GLOBAL_ONLY);

    } else {

        // We have no args. Try to get the level from the Tcl variable.
        obj = Tcl_GetVar2Ex(interp, __debug[1], NULL, TCL_GLOBAL_ONLY);
        if (obj != NULL) {
            // The variable was found. Let's try to convert it to integer
            // value.
            if (Tcl_GetIntFromObj(interp, obj, &level) == TCL_ERROR) {
                // Could not convert it and this is ok. Let's use the
                // default value and reset interp's error state.
                Tcl_ResetResult(interp);
            }
        }

    }

    // Return current debug level
    SET_RESULT(INT, level);
    RETURN(OK);

}

static int mtls_cmd_status(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    UNUSED(clientData);
    ENTER(cmd_status, interp);

    if (objc < 2 || objc > 3) {
        Tcl_WrongNumArgs(interp, 1, objv, "?-local? channel");
        RETURN(ERROR);
    }

    int is_local = 0;
    if (objc == 3) {
        if (strcmp(Tcl_GetString(objv[1]), "-local") != 0) {
            SET_RESULT(FORMAT, "unknown flag \"%s\" is specified where"
                " \"-local\" is expected", Tcl_GetString(objv[1]));
            SET_ERROR("WRONGARGS");
            RETURN(ERROR);
        }
        is_local = 1;
    }

    Tcl_Channel chan = Tcl_GetChannel(interp, Tcl_GetString(objv[objc-1]), NULL);
    if (chan == NULL) {
        SET_RESULT(FORMAT, "invalid channel \"%s\" specified",
            Tcl_GetString(objv[objc-1]));
        SET_ERROR("WRONGARGS");
        RETURN(ERROR);
    }

    chan = Tcl_GetTopChannel(chan);
    if (Tcl_GetChannelType(chan) != mtls_ChannelType()) {
        SET_RESULT(FORMAT, "bad channel \"%s\": not a TLS channel",
            Tcl_GetString(objv[objc-1]));
        SET_ERROR("WRONGARGS");
        RETURN(ERROR);
    }

    mtls_ctx *ctx = (mtls_ctx *)Tcl_GetChannelInstanceData(chan);

    Tcl_Obj *obj = Tcl_NewDictObj();
    Tcl_IncrRefCount(obj);
    mtls_ctx_get_status(ctx, is_local, obj);
    SET_RESULT(OBJECT, obj);
    Tcl_DecrRefCount(obj);

    RETURN(OK);
}


static int mtls_cmd_unimport(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    UNUSED(clientData);
    ENTER(cmd_unimport, interp);

    if (objc != 2) {
        Tcl_WrongNumArgs(interp, 1, objv, "channel");
        RETURN(ERROR);
    }

    Tcl_Channel chan = Tcl_GetChannel(interp, Tcl_GetString(objv[1]), NULL);
    if (chan == NULL) {
        SET_RESULT(FORMAT, "can not find channel named \"%s\"",
            Tcl_GetString(objv[1]));
        SET_ERROR("WRONGARGS");
        RETURN(ERROR);
    }

    chan = Tcl_GetTopChannel(chan);
    if (Tcl_GetChannelType(chan) != mtls_ChannelType()) {
        SET_RESULT(FORMAT, "bad channel \"%s\": not a TLS channel",
            Tcl_GetString(objv[1]));
        SET_ERROR("WRONGARGS");
        RETURN(ERROR);
    }

    if (Tcl_UnstackChannel(interp, chan) == TCL_ERROR) {
        RETURN(ERROR);
    }

    RETURN(OK);
}


static int mtls_cmd_handshake(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    UNUSED(clientData);
    ENTER(cmd_handshake, interp);

    if (objc != 2) {
        Tcl_WrongNumArgs(interp, 1, objv, "channel");
        RETURN(ERROR);
    }

    Tcl_Channel chan = Tcl_GetChannel(interp, Tcl_GetString(objv[1]), NULL);
    if (chan == NULL) {
        SET_RESULT(FORMAT, "invalid channel \"%s\" specified",
            Tcl_GetString(objv[1]));
        SET_ERROR("WRONGARGS");
        RETURN(ERROR);
    }

    chan = Tcl_GetTopChannel(chan);
    if (Tcl_GetChannelType(chan) != mtls_ChannelType()) {
        SET_RESULT(FORMAT, "bad channel \"%s\": not a TLS channel",
            Tcl_GetString(objv[1]));
        SET_ERROR("WRONGARGS");
        RETURN(ERROR);
    }

    mtls_ctx *ctx = (mtls_ctx *)Tcl_GetChannelInstanceData(chan);

    // Start the handshake if the ctx is in an initialized state, but
    // the handshake process has not yet started.
    if (ctx->state == MTLS_CTX_STATE_INIT ||
        ctx->state == MTLS_CTX_STATE_HANDSHAKE)
    {
        TRC("socket is not connected, try to connect");
        if (mtls_ctx_connect(ctx) == TCL_ERROR) {
            // The handshake was unsuccessful. Return the error.
            SET_RESULT(STRING, mtls_ctx_error_get(ctx));
            SET_ERROR("INTERNAL");
            RETURN(ERROR);
        }
        TRC("got non-error connection, state is: %d", ctx->state);
    }

    switch (ctx->state) {
    case MTLS_CTX_STATE_PREINIT:
        SET_RESULT(STRING, "channel TLS context is in pre-init state");
        SET_ERROR("WRONGSTATE");
        RETURN(ERROR);
        break;
    case MTLS_CTX_STATE_HANDSHAKE:
        // The handshake has begun but is not yet completed.
        SET_RESULT(INT, 0);
        RETURN(OK);
        break;
    case MTLS_CTX_STATE_CONNECTED:
        // The handshake was successful, ctx is in the connected state.
        SET_RESULT(INT, 1);
        RETURN(OK);
        break;
    case MTLS_CTX_STATE_ERROR:
        // ctx is in error state
        SET_RESULT(STRING, mtls_ctx_error_get(ctx));
        SET_ERROR("INTERNAL");
        RETURN(ERROR);
        break;
    case MTLS_CTX_STATE_INIT:
        // This is an unexpected state since we have already started
        // the handshake above.
        SET_RESULT(STRING, "the channel is in an unexpected state");
        SET_ERROR("INTERNAL");
        RETURN(ERROR);
        break;
    }

    // All cases are handled by the switch above. Let's add a return here to
    // avoid the compiler warning.
    return 0;
}

static int mtls_cmd_init(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    ENTER(cmd_init, interp);
    mtls_config *conf = (mtls_config *)clientData;

    // objv[0] is this command name. Skip it by setting the start index to 1.
    if (mtls_parse_parameters(interp, 1, objc, objv, conf,
        MTLS_IMPORT_CONFIG_OVERRIDE) != TCL_OK)
    {
        // We have an error message in the Tcl interp from the above func
        RETURN(ERROR);
    }

    // Check if we got the -model/-server/-servername parameters.
    // In the default configuration, they should be ignored.
    const mtls_config_name config_name[3] = {
        MTLS_CONF_MODEL, MTLS_CONF_SERVER, MTLS_CONF_SERVERNAME
    };
    for (unsigned int i = 0; i < 3; i++) {
        if ((*conf)[config_name[i]] != NULL) {
            WRN("got the %s option, which is ignored in this function",
                mtls_config_strings[config_name[i]]);
            Tcl_DecrRefCount((*conf)[config_name[i]]);
            (*conf)[config_name[i]] = NULL;
        }
    }

    // Create a dict for all defined parameters as the return result.
    Tcl_Obj *result = Tcl_NewDictObj();
    for (unsigned int i = 0; i < MTLS_CONF_SIZE; i++) {
        if ((*conf)[i] != NULL) {
            Tcl_DictObjPut(interp, result,
                Tcl_NewStringObj(mtls_config_strings[i], -1), (*conf)[i]);
        }
    }
    SET_RESULT(OBJECT, result);

    RETURN(OK);
}

static int mtls_cmd_socket(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    ENTER(cmd_socket, interp);
    mtls_config *conf = (mtls_config *)clientData;

    TRC("arguments count: %d", objc);

    static const char *const options[] = {
        "-server", "-myaddr", "-myport", "-async", NULL
    };
    enum options {
        OPT_SERVER, OPT_MYADDR, OPT_MYPORT, OPT_ASYNC
    };

    // Initialize ::socket command

    Tcl_Obj *socketCmd = Tcl_NewObj();
    Tcl_IncrRefCount(socketCmd);
    Tcl_ListObjAppendElement(interp, socketCmd, Tcl_NewStringObj(
        "::socket", -1));

    // Initialize ::mtls::import command

    Tcl_Obj *importCmd = Tcl_NewObj();
    Tcl_IncrRefCount(importCmd);
    Tcl_ListObjAppendElement(interp, importCmd, Tcl_NewStringObj(
        "::mtls::import", -1));

    // This is object that contains the parameter autoservername. First we
    // initialize it as an object from the default configuration. If this
    // parameter is found in the current arguments, then this object will
    // be replaced with the current value.
    Tcl_Obj *autoservernameObj = (*conf)[MTLS_CONF_AUTOSERVERNAME];

    // Let's parse arguments and construct ::socket and ::mtls::import
    // commands.

    int is_server = 0;
    int idx;
    for (idx = 1; idx < objc; idx++) {

        int opt;

        // Check to see if this option is known to ::socket
        if (Tcl_GetIndexFromObj(interp, objv[idx], options, "option",
            TCL_EXACT, &opt) == TCL_OK)
        {
            TRC("argument for socket cmd detected [%s]",
                Tcl_GetString(objv[idx]));
            // Yes. It is known. Add it to ::socket command if it is not
            // -server arg with a value.
            if (opt == OPT_SERVER && idx < (objc - 1)) {
                TRC("set the server mode");
                // Set the appropriate argument to ::mtls::import
                Tcl_ListObjAppendElement(interp, importCmd, Tcl_NewStringObj(
                    "-server", -1));
                Tcl_ListObjAppendElement(interp, importCmd, Tcl_NewBooleanObj(
                    1));
                // Append also the callback as -accept arg for ::mtls::import
                Tcl_ListObjAppendElement(interp, importCmd, Tcl_NewStringObj(
                    "-accept", -1));
                Tcl_ListObjAppendElement(interp, importCmd, objv[++idx]);
                is_server = 1;
            } else {
                Tcl_ListObjAppendElement(interp, socketCmd, objv[idx]);
                // If the option is not -async, assume it has a value. But also
                // check to see if we have arguments available.
                if (opt != OPT_ASYNC && idx < (objc - 1)) {
                    Tcl_ListObjAppendElement(interp, socketCmd, objv[++idx]);
                    TRC("add a value for the argument [%s]",
                        Tcl_GetString(objv[idx]));
                }
            }
            // Let's check the next option
            continue;
        }

        // The current argument is unknown for ::socket. If it starts with '-',
        // consider it an argument to ::mtls::import. If it is not, stop
        // processing the arguments.
        const char *opt_string = Tcl_GetString(objv[idx]);
        if (opt_string[0] != '-') {
            TRC("the end of options detected on argument #%d", idx);
            break;
        }

        // We have an option for ::mtls::import. Let's push it to this command
        // queue. If we have a possible value for the argument, let's push
        // it also.
        TRC("argument for import cmd detected [%s]", opt_string);
        Tcl_ListObjAppendElement(interp, importCmd, objv[idx]);
        if (idx < (objc - 1)) {
            Tcl_ListObjAppendElement(interp, importCmd, objv[++idx]);
            TRC("add a value for the argument [%s]",
                Tcl_GetString(objv[idx]));
            // If the current option is -autoservername, replace
            // the appropriate object.
            if (strcmp(opt_string, "-autoservername") == 0) {
                autoservernameObj = objv[idx];
            }
        }

    }

#ifndef MTLS_ENABLE_SERVER
    if (is_server) {
        Tcl_DecrRefCount(importCmd);
        Tcl_DecrRefCount(socketCmd);
        SET_RESULT(STRING, PACKAGE_NAME " was build without SSL server support");
        SET_ERROR("UNSUP");
        RETURN(ERROR);
    }
#endif
#ifndef MTLS_ENABLE_CLIENT
    if (!is_server) {
        Tcl_DecrRefCount(importCmd);
        Tcl_DecrRefCount(socketCmd);
        SET_RESULT(STRING, PACKAGE_NAME " was build without SSL client support");
        SET_ERROR("UNSUP");
        RETURN(ERROR);
    }
#endif

    // If we are in client mode, let's try to get the server name from argument
    // number end-1.
    if (!is_server && objc >= 3) {
        // The default value is true
        int autoservername = 1;
        // Check if the parameter -autoservername exists.
        // An error of boolean convertion can be silently ignored here.
        // It will be reported by ::mtls::import below below when it checks
        // its arguments.
        if (autoservernameObj != NULL) {
            if (Tcl_GetBooleanFromObj(interp, autoservernameObj,
                &autoservername) != TCL_OK)
            {
                // Reset error if any
                Tcl_ResetResult(interp);
            }
        }
        // Check the parameter -autoservername if we need to automatically
        // detect the server name.
        if (autoservername) {
            Tcl_ListObjAppendElement(interp, importCmd, Tcl_NewStringObj(
                "-servername", -1));
            Tcl_ListObjAppendElement(interp, importCmd, objv[objc-2]);
            TRC("the server name is set automatically to [%s]",
                Tcl_GetString(objv[objc-2]));
        }
    }

    // Check if the server mode is requested. Set ::mtls::import with
    // -accept arg as a callback in this case.
    if (is_server) {
        // ::mtls::import requires a channel name as the first arg.
        // But if it is called as a callback, the channel name will be added
        // to the end of the options. Let's add special stub 'accept' as
        // a channel name.
        Tcl_Obj *socket_name[1];
        socket_name[0] = Tcl_NewStringObj("accept", -1);
        Tcl_ListObjReplace(interp, importCmd, 1, 0, 1, socket_name);
        // Now, let's add the built ::mtls::import command as a callback.
        Tcl_ListObjAppendElement(interp, socketCmd, Tcl_NewStringObj(
            "-server", -1));
        Tcl_ListObjAppendElement(interp, socketCmd, importCmd);
        // Import cmd is not needed anymore
        Tcl_DecrRefCount(importCmd);
    }

    // Push the rest of arguments to the ::socket command queue.
    for (; idx < objc; idx++) {
        TRC("add unknown argument to socket cmd: [%s]",
                Tcl_GetString(objv[idx]));
        Tcl_ListObjAppendElement(interp, socketCmd, objv[idx]);
    }

    int ret;

    // Try to execute the ::socket cmd
    INF("execute the socket cmd");
    ret = Tcl_EvalObjEx(interp, socketCmd, TCL_EVAL_GLOBAL | TCL_EVAL_DIRECT);
    // socketCmd is no longer needed
    Tcl_DecrRefCount(socketCmd);

    // If we got an error, return it
    if (ret != TCL_OK) {
        ERR("the socket cmd failed");
        if (!is_server) {
            Tcl_DecrRefCount(importCmd);
        }
        RETURN(ERROR);
    }

    // If we are in server mode, just return the result from ::socket.
    // The cmd import has already been released above.
    if (is_server) {
        RETURN(OK);
    }

    // The ::socket command was executed successfully. Let's get its return value.
    // It should be the socket name.
    Tcl_Obj *socket_name[1];
    socket_name[0] = Tcl_GetObjResult(interp);
    INF("got socket name [%s]", Tcl_GetString(socket_name[0]));
    // Let's preserve the socket_name object. We will use it to close the socket
    // in case ::mtls::import fails.
    Tcl_IncrRefCount(socket_name[0]);
    // Insert the socket name for ::mtls::import
    Tcl_ListObjReplace(interp, importCmd, 1, 0, 1, socket_name);

    // Try to execute the ::mtls::import cmd
    INF("execute the import cmd");
    ret = Tcl_EvalObjEx(interp, importCmd, TCL_EVAL_GLOBAL | TCL_EVAL_DIRECT);
    // importCmd is no longer needed
    Tcl_DecrRefCount(importCmd);

    // If we got an error, let's close the socket and return the error.
    if (ret != TCL_OK) {
        ERR("the import cmd failed");
        // We have to close the original channel
        Tcl_Obj *closeCmd = Tcl_NewObj();
        Tcl_IncrRefCount(closeCmd);
        Tcl_ListObjAppendElement(interp, closeCmd, Tcl_NewStringObj(
            "::close", -1));
        Tcl_ListObjAppendElement(interp, closeCmd, socket_name[0]);
        // We want to return the error from ::mtls::import, but not
        // from ::close. Thus, let's preserver interp's state.
        Tcl_InterpState sr = Tcl_SaveInterpState(interp, 0);
        INF("execute the close cmd");
        Tcl_EvalObjEx(interp, closeCmd, TCL_EVAL_GLOBAL | TCL_EVAL_DIRECT);
        Tcl_RestoreInterpState(interp, sr);
        Tcl_DecrRefCount(closeCmd);
        Tcl_DecrRefCount(socket_name[0]);
        RETURN(ERROR);
    }

    // socket_name object is not needed anymore
    Tcl_DecrRefCount(socket_name[0]);

    RETURN(OK);
}

#define CONF2VAR_BOOL(v, o, d) \
    int o = d; \
    if ((*conf)[v] == NULL) { \
        TRC("boolean config param %s is not defined, using default: %d", #o, o); \
    } else { \
        if (Tcl_GetBooleanFromObj(interp, (*conf)[v], &o) != TCL_OK) { \
            ERR("failed to convert param %s to boolean", #o); \
        } else { \
            TRC("set config param %s as bool[%d]", #o, o); \
        } \
    }

#define CONF2VAR_OBJ(v, o) \
    Tcl_Obj *o = NULL; \
    if ((*conf)[v] == NULL) { \
        TRC("set config param %s as obj[%s]", #o, "NULL"); \
    } else { \
        o = (*conf)[v]; \
        TRC("set config param %s as obj[%p]", #o, o); \
    }

#define CONF2VAR_CHAR(v, o) \
    const char *o; \
    if ((*conf)[v] == NULL || Tcl_GetCharLength((*conf)[v]) == 0) { \
        o = NULL; \
        TRC("set config param %s as *char[%s]", #o, "NULL"); \
    } else { \
        o = Tcl_GetString((*conf)[v]); \
        TRC("set config param %s as *char[%s]", #o, o); \
    }

#define CONF2VAR(t, ...) __XCONCAT(CONF2VAR, t)(__VA_ARGS__)

static int mtls_cmd_import(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    mtls_config *global_conf = (mtls_config *)clientData;
    ENTER(cmd_import, interp);

    if (objc < 2) {
        Tcl_WrongNumArgs(interp, 1, objv, "channel ?options?");
        RETURN(ERROR);
    }

    Tcl_Channel upstream_chan;
    int is_accept = 0;
    int maxopts = objc;
    // If the first arg is "accept" then we are called as a callback
    // in "socket -server ...". In this case, we have 3 additional
    // arguments after options: socket_name, ipaddr and port.
    // Also in this case, we should expect objc to be 5 or higher:
    // "cmdname <accept> socket ipaddr port"
    if (strcmp(Tcl_GetString(objv[1]), "accept") == 0 && objc > 4) {
        upstream_chan = Tcl_GetChannel(interp, Tcl_GetString(objv[objc-3]),
            NULL);
        maxopts -= 3;
        is_accept = 1;
        TRC("detected call as a callback");
    } else {
        upstream_chan = Tcl_GetChannel(interp, Tcl_GetString(objv[1]), NULL);
    }

    if (upstream_chan == NULL) {
        SET_RESULT(FORMAT, "invalid channel \"%s\" specified",
            Tcl_GetString(objv[1]));
        SET_ERROR("WRONGARGS");
        RETURN(ERROR);
    }

    upstream_chan = Tcl_GetTopChannel(upstream_chan);

    DBG("initialize mtls config");
    mtls_config *conf = (mtls_config *)ckalloc(sizeof(mtls_config));
    if (conf == NULL) {
        SET_RESULT(STRING, "Unable to initialize mtls config");
        SET_ERROR("OUTOFMEM");
        RETURN(ERROR);
    }
    mtls_init_config(interp, conf);
    // objv[0] is this command name and objv[1] is a channel name. Skip these
    // by setting the start index to 2.
    if (mtls_parse_parameters(interp, 2, maxopts, objv, conf,
        MTLS_IMPORT_CONFIG_MERGE) != TCL_OK)
    {
        // We have an error message in the Tcl interp from the above func
        mtls_free_config(interp, conf);
        ckfree(conf);
        RETURN(ERROR);
    }

    // Base config is the global config by default.
    mtls_config *base_conf = global_conf;

    if ((*conf)[MTLS_CONF_MODEL] != NULL) {
        const char *model_chan_name = Tcl_GetString((*conf)[MTLS_CONF_MODEL]);

        DBG("try to use model channel [%s] for default config",
            model_chan_name);

        Tcl_Channel model_chan = Tcl_GetChannel(interp,
            model_chan_name, NULL);
        if (model_chan == NULL) {
            mtls_free_config(interp, conf);
            ckfree(conf);
            SET_RESULT(FORMAT, "can not find model channel named \"%s\"",
                model_chan_name);
            SET_ERROR("WRONGARGS");
            RETURN(ERROR);
        }

        model_chan = Tcl_GetTopChannel(model_chan);
        if (Tcl_GetChannelType(model_chan) != mtls_ChannelType()) {
            mtls_free_config(interp, conf);
            ckfree(conf);
            SET_RESULT(FORMAT, "bad model channel \"%s\": not a TLS channel",
                model_chan_name);
            SET_ERROR("WRONGARGS");
            RETURN(ERROR);
        }

        mtls_ctx *model_ctx = (mtls_ctx *)Tcl_GetChannelInstanceData(model_chan);

        base_conf = model_ctx->tcl_config;
    }

    // Add the parameters from the base config
    mtls_import_config(interp, base_conf, conf, MTLS_IMPORT_CONFIG_UPDATE);

    CONF2VAR(BOOL, MTLS_CONF_SERVER, server, 0);

#ifndef MTLS_ENABLE_SERVER
    if (server) {
        mtls_free_config(interp, conf);
        ckfree(conf);
        SET_RESULT(STRING, PACKAGE_NAME " was build without SSL server support");
        SET_ERROR("UNSUP");
        RETURN(ERROR);
    }
#endif
#ifndef MTLS_ENABLE_CLIENT
    if (!server) {
        mtls_free_config(interp, conf);
        ckfree(conf);
        SET_RESULT(STRING, PACKAGE_NAME " was build without SSL client support");
        SET_ERROR("UNSUP");
        RETURN(ERROR);
    }
#endif

    // Set the default value for the -require/-request args based on
    // the socket type (client/server)
    if ((*conf)[MTLS_CONF_REQUIRE] == NULL) {
        TRC("set the default -request/-require args based on socket type");
        mtls_set_config(interp, conf, MTLS_CONF_REQUEST,
            Tcl_NewBooleanObj(server ? 0 : 1));
        mtls_set_config(interp, conf, MTLS_CONF_REQUIRE,
            Tcl_NewBooleanObj(server ? 0 : 1));
    }

    // Set key/keyfile args to cert/certfile if they are undefined
    if ((*conf)[MTLS_CONF_KEYFILE] == NULL && (*conf)[MTLS_CONF_KEY] == NULL) {
        if ((*conf)[MTLS_CONF_CERTFILE] != NULL) {
            TRC("set -keyfile arg value to -certfile arg value");
            mtls_set_config(interp, conf, MTLS_CONF_KEYFILE,
                (*conf)[MTLS_CONF_CERTFILE]);
        } else if ((*conf)[MTLS_CONF_CERT] != NULL) {
            TRC("set -key arg value to -cert arg value");
            mtls_set_config(interp, conf, MTLS_CONF_KEY,
                (*conf)[MTLS_CONF_CERT]);
        }
    }

    // Convert config to variables
    CONF2VAR(CHAR, MTLS_CONF_CADIR, cadir);
    CONF2VAR(CHAR, MTLS_CONF_CAFILE, cafile);
    CONF2VAR(CHAR, MTLS_CONF_CERTFILE, certfile);
    CONF2VAR(CHAR, MTLS_CONF_CERT, cert);
    CONF2VAR(OBJ, MTLS_CONF_COMMAND, command);
    CONF2VAR(CHAR, MTLS_CONF_DHPARAMS, dhparams);
    CONF2VAR(CHAR, MTLS_CONF_KEYFILE, keyfile);
    CONF2VAR(CHAR, MTLS_CONF_KEY, key);
    CONF2VAR(OBJ, MTLS_CONF_PASSWORD, password);
    CONF2VAR(BOOL, MTLS_CONF_REQUEST, request, 1);
    CONF2VAR(BOOL, MTLS_CONF_REQUIRE, require, 1);
    CONF2VAR(CHAR, MTLS_CONF_SERVERNAME, servername);
#ifdef MTLS_BACKEND_FEATURE_TLS1_1
    CONF2VAR(BOOL, MTLS_CONF_TLS1_1, tls1_1, 1);
#else
    CONF2VAR(BOOL, MTLS_CONF_TLS1_1, tls1_1, 0);
#endif
#ifdef MTLS_BACKEND_FEATURE_TLS1_2
    CONF2VAR(BOOL, MTLS_CONF_TLS1_2, tls1_2, 1);
#else
    CONF2VAR(BOOL, MTLS_CONF_TLS1_2, tls1_2, 0);
#endif
#ifdef MTLS_BACKEND_FEATURE_TLS1_3
    CONF2VAR(BOOL, MTLS_CONF_TLS1_3, tls1_3, 1);
#else
    CONF2VAR(BOOL, MTLS_CONF_TLS1_3, tls1_3, 0);
#endif
    CONF2VAR(OBJ, MTLS_CONF_ACCEPT, accept);

    // Preserve the accept object. We will need it later.
    if (accept != NULL && is_accept) {
        accept = Tcl_DuplicateObj(accept);
        Tcl_IncrRefCount(accept);
    } else {
        // Set the accept variable to NULL. Below we will check this variable
        // to determine if we are in server callback mode.
        accept = NULL;
    }

    // Handle the cipher arg in special way. Convert Tcl list to *char[].
    const char **ciphers = NULL;
    int cipherslen = 0;
    if ((*conf)[MTLS_CONF_CIPHER] == NULL) {
        TRC("set config param cipher as *char[NULL]");
    } else {
        if (Tcl_ListObjLength(interp, (*conf)[MTLS_CONF_CIPHER],
            &cipherslen) != TCL_OK)
        {
            if (accept != NULL) {
                Tcl_DecrRefCount(accept);
            }
            mtls_free_config(interp, conf);
            ckfree(conf);
            SET_RESULT(FORMAT, "cipher arg is not valid list: %s",
                Tcl_GetString((*conf)[MTLS_CONF_CIPHER]));
            SET_ERROR("WRONGARGS");
            RETURN(ERROR);
        }
        ciphers = (const char **)ckalloc(sizeof(*ciphers) * (cipherslen + 1));
        if (ciphers == NULL) {
            if (accept != NULL) {
                Tcl_DecrRefCount(accept);
            }
            mtls_free_config(interp, conf);
            ckfree(conf);
            SET_RESULT(FORMAT, "failed to allocale memory for chipher list");
            SET_ERROR("OUTOFMEM");
            RETURN(ERROR);
        }
        for (int i = 0; i < cipherslen; i++) {
            Tcl_Obj *cipherObj;
            Tcl_ListObjIndex(interp, (*conf)[MTLS_CONF_CIPHER], i, &cipherObj);
            ciphers[i] = Tcl_GetString(cipherObj);
            TRC("add cipher[%s]", ciphers[i]);
        }
        ciphers[cipherslen] = NULL;
    }

    // Handle the alpn arg in special way. Convert Tcl list to *char[].
    const char **alpn = NULL;
    int alpnlen = 0;
    if ((*conf)[MTLS_CONF_ALPN] == NULL) {
        TRC("set config param alpn as *char[NULL]");
    } else {
        if (Tcl_ListObjLength(interp, (*conf)[MTLS_CONF_ALPN],
            &alpnlen) != TCL_OK)
        {
            if (accept != NULL) {
                Tcl_DecrRefCount(accept);
            }
            mtls_free_config(interp, conf);
            ckfree(conf);
            SET_RESULT(FORMAT, "alpn arg is not valid list: %s",
                Tcl_GetString((*conf)[MTLS_CONF_ALPN]));
            SET_ERROR("WRONGARGS");
            RETURN(ERROR);
        }
        alpn = (const char **)ckalloc(sizeof(*alpn) * (alpnlen + 1));
        if (alpn == NULL) {
            if (accept != NULL) {
                Tcl_DecrRefCount(accept);
            }
            mtls_free_config(interp, conf);
            ckfree(conf);
            SET_RESULT(FORMAT, "failed to allocale memory for alpn list");
            SET_ERROR("OUTOFMEM");
            RETURN(ERROR);
        }
        for (int i = 0; i < alpnlen; i++) {
            Tcl_Obj *alpnObj;
            Tcl_ListObjIndex(interp, (*conf)[MTLS_CONF_ALPN], i, &alpnObj);
            alpn[i] = Tcl_GetString(alpnObj);
            TRC("add alpn[%s]", alpn[i]);
        }
        alpn[alpnlen] = NULL;
    }

    // Try to get private key from Tcl callback
    // Initialize a temporary DString that will store the password
    Tcl_DString keypasswordDS;
    Tcl_DStringInit(&keypasswordDS);
    const char *keypassword = NULL;
    if (password != NULL) {
        INF("try using a callback to get the private key password");
        // Save interp state
        Tcl_InterpState sr = Tcl_SaveInterpState(interp, 0);
        Tcl_Preserve(interp);
        int res = Tcl_EvalObjEx(interp, password,
            TCL_EVAL_GLOBAL | TCL_EVAL_DIRECT);
        if (res == TCL_ERROR) {
            // Report about possible failure
            Tcl_BackgroundError(interp);
        } else {
            // All ok. Let's store the result.
            Tcl_DStringGetResult(interp, &keypasswordDS);
            keypassword = Tcl_DStringValue(&keypasswordDS);
            INF("successfully got private key password from callback");
        }
        // Restore interp state
        Tcl_RestoreInterpState(interp, sr);
        Tcl_Release(interp);
        if (res == TCL_ERROR) {
            if (accept != NULL) {
                Tcl_DecrRefCount(accept);
            }
            Tcl_DStringFree(&keypasswordDS);
            if (ciphers != NULL) {
                ckfree(ciphers);
            }
            if (alpn != NULL) {
                ckfree(alpn);
            }
            mtls_free_config(interp, conf);
            ckfree(conf);
            SET_RESULT(FORMAT, "password callback failed with error");
            SET_ERROR("CALLBACK");
            RETURN(ERROR);
        }
    }

    mtls_ctx *ctx;
    if (mtls_ctx_init(
        &ctx,
        interp,
        upstream_chan,
        cadir,
        cafile,
        certfile,
        cert,
        ciphers,
        cipherslen,
        alpn,
        alpnlen,
        command,
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
        if (accept != NULL) {
            Tcl_DecrRefCount(accept);
        }
        Tcl_DStringFree(&keypasswordDS);
        if (ciphers != NULL) {
            ckfree(ciphers);
        }
        if (alpn != NULL) {
            ckfree(alpn);
        }
        mtls_free_config(interp, conf);
        ckfree(conf);
        if (ctx == NULL) {
            SET_RESULT(FORMAT, "failed to allocate context");
            SET_ERROR("OUTOFMEM");
            RETURN(ERROR);
        }
        Tcl_SetObjResult(interp, Tcl_NewStringObj(
            mtls_ctx_error_get(ctx), -1));
        Tcl_SetErrorCode(interp, "MTLS", "INTERNAL", NULL);
        mtls_ctx_free(ctx);
        RETURN(ERROR);
    }

    // Release config and memory allocations that are no longer needed
    Tcl_DStringFree(&keypasswordDS);
    if (alpn != NULL) {
        ckfree(alpn);
    }
    if (ciphers != NULL) {
        ckfree(ciphers);
    }
    // Assigning a configuration to a variable in mtls context. It will be
    // freed in the future as part of that context.
    ctx->tcl_config = conf;

    Tcl_DString saveTranslation, saveBlocking, saveEncoding, saveEOFChar;
    Tcl_DStringInit(&saveTranslation);
    Tcl_DStringInit(&saveBlocking);
    Tcl_DStringInit(&saveEOFChar);
    Tcl_DStringInit(&saveEncoding);
    Tcl_GetChannelOption(interp, upstream_chan, "-eofchar", &saveEOFChar);
    Tcl_GetChannelOption(interp, upstream_chan, "-encoding", &saveEncoding);
    Tcl_GetChannelOption(interp, upstream_chan, "-translation", &saveTranslation);
    Tcl_GetChannelOption(interp, upstream_chan, "-blocking", &saveBlocking);
    Tcl_SetChannelOption(interp, upstream_chan, "-translation", "binary");
    Tcl_SetChannelOption(interp, upstream_chan, "-blocking", "true");

    INF("import channel: %s (0x%x)", Tcl_GetChannelName(upstream_chan), upstream_chan);

    Tcl_Channel self_chan = Tcl_StackChannel(interp, mtls_ChannelType(), (ClientData)ctx,
        (TCL_READABLE | TCL_WRITABLE), upstream_chan);
    if (self_chan == NULL) {
        if (accept != NULL) {
            Tcl_DecrRefCount(accept);
        }
        Tcl_DStringFree(&saveTranslation);
        Tcl_DStringFree(&saveEncoding);
        Tcl_DStringFree(&saveEOFChar);
        Tcl_DStringFree(&saveBlocking);
        mtls_ctx_free(ctx);
        // we already have an error message from Tcl_StackChannel in interp's result
        APPEND_RESULT(". Could not create Tcl channel");
        SET_ERROR("IO");
        RETURN(ERROR);
    }

    // ctx has been initialized without self_chan. Creating self_chan requires
    // specifying ctx as client data. This is why we initialized ctx before
    // self_chan.
    ctx->self_chan = self_chan;

    INF("self channel: %s (0x%x)", Tcl_GetChannelName(self_chan), self_chan);
    Tcl_SetChannelOption(interp, self_chan, "-translation", Tcl_DStringValue(&saveTranslation));
    Tcl_SetChannelOption(interp, self_chan, "-encoding", Tcl_DStringValue(&saveEncoding));
    Tcl_SetChannelOption(interp, self_chan, "-eofchar", Tcl_DStringValue(&saveEOFChar));
    Tcl_SetChannelOption(interp, self_chan, "-blocking", Tcl_DStringValue(&saveBlocking));
    Tcl_DStringFree(&saveTranslation);
    Tcl_DStringFree(&saveEncoding);
    Tcl_DStringFree(&saveEOFChar);
    Tcl_DStringFree(&saveBlocking);

    // Check if we are not in "socket -server ..." callback mode.
    if (accept == NULL) {
        INF("return channel name: %s", Tcl_GetChannelName(self_chan));
        SET_RESULT(STRING, Tcl_GetChannelName(self_chan));
        RETURN(OK);
    }

    // We are in "socket -server ..." callback mode. The object accept contains
    // user-defined socket callback.

    INF("run user-defined callback for server socket");

    // Add our socket name and callback parameters.
    Tcl_ListObjAppendElement(interp, accept, Tcl_NewStringObj(
        Tcl_GetChannelName(self_chan), -1));
    Tcl_ListObjAppendElement(interp, accept, objv[objc-2]);
    Tcl_ListObjAppendElement(interp, accept, objv[objc-1]);

    Tcl_Preserve(interp);
    int res = Tcl_EvalObjEx(interp, accept, TCL_EVAL_GLOBAL | TCL_EVAL_DIRECT);
    Tcl_Release(interp);

    Tcl_IncrRefCount(accept);

    // Return anything returned by the custom callback.
    RETURN(INT, res);
}

#ifndef STRINGIFY
#  define STRINGIFY(x) STRINGIFY1(x)
#  define STRINGIFY1(x) #x
#endif

#if TCL_MAJOR_VERSION > 8
#define MIN_VERSION "9.0"
#else
#define MIN_VERSION "8.6"
#endif

void mtls_interp_shutdown(ClientData clientData, Tcl_Interp *interp) {
    ENTER(interp_shutdown, interp);
    mtls_free(interp);
    mtls_free_config(interp, clientData);
    ckfree(clientData);
    RETURN();
}

#ifdef __cplusplus
extern "C" {
#endif  /* __cplusplus */
DLLEXPORT int Mtls_Init(Tcl_Interp* interp) {

    Tcl_CmdInfo info;

    if (Tcl_InitStubs(interp, MIN_VERSION, 0) == NULL) {
        Tcl_SetObjResult(interp, Tcl_NewStringObj(
                "Unable to initialize Tcl stubs", -1));
        return TCL_ERROR;
    }

    ENTER(Mtls_Init, interp);

    if (mtls_init(interp) != TCL_OK) {
        SET_RESULT(STRING, "Unable to initialize mtls");
        SET_ERROR("INTERNAL");
        RETURN(ERROR);
    }

    // Initialize the default configuration for sockets
    DBG("initialize the default configuration");
    mtls_config *conf = (mtls_config *)ckalloc(sizeof(mtls_config));
    if (conf == NULL) {
        SET_RESULT(STRING, "Unable to initialize the default config");
        SET_ERROR("INTERNAL");
        RETURN(ERROR);
    }
    mtls_init_config(interp, conf);

    // Create a callback to correctly clean up the config when the interpreter
    // is removed
    Tcl_CallWhenDeleted(interp, mtls_interp_shutdown, conf);

    if (Tcl_GetCommandInfo(interp, "::tcl::build-info", &info)) {
        Tcl_CreateObjCommand(interp, "::mtls::build-info",
            info.objProc, (void *)(
                PACKAGE_VERSION "+" STRINGIFY(SAMPLE_VERSION_UUID)
#if defined(__clang__) && defined(__clang_major__)
                ".clang-" STRINGIFY(__clang_major__)
#if __clang_minor__ < 10
                "0"
#endif
                STRINGIFY(__clang_minor__)
#endif
#if defined(__cplusplus) && !defined(__OBJC__)
                ".cplusplus"
#endif
#ifndef NDEBUG
                ".debug"
#endif
#if !defined(__clang__) && !defined(__INTEL_COMPILER) && defined(__GNUC__)
                ".gcc-" STRINGIFY(__GNUC__)
#if __GNUC_MINOR__ < 10
                "0"
#endif
                STRINGIFY(__GNUC_MINOR__)
#endif
#ifdef __INTEL_COMPILER
                ".icc-" STRINGIFY(__INTEL_COMPILER)
#endif
#ifdef TCL_MEM_DEBUG
                ".memdebug"
#endif
#if defined(_MSC_VER)
                ".msvc-" STRINGIFY(_MSC_VER)
#endif
#ifdef USE_NMAKE
                ".nmake"
#endif
#ifndef TCL_CFG_OPTIMIZED
                ".no-optimize"
#endif
#ifdef __OBJC__
                ".objective-c"
#if defined(__cplusplus)
                "plusplus"
#endif
#endif
#ifdef TCL_CFG_PROFILED
                ".profile"
#endif
#ifdef PURIFY
                ".purify"
#endif
#ifdef STATIC_BUILD
                ".static"
#endif
            ), NULL
        );
    }

    DBG("register package config");
    Tcl_RegisterConfig(interp, PACKAGE_NAME, pkgconfig, "iso8859-1");

    /* Provide the current package */
    if (Tcl_PkgProvide(interp, PACKAGE_NAME, PACKAGE_VERSION) != TCL_OK) {
        SET_RESULT(STRING, "Unable to provide package");
        SET_ERROR("INTERNAL");
        RETURN(ERROR);
    }

    Tcl_CreateObjCommand(interp, "::mtls::socket",
        (Tcl_ObjCmdProc *)mtls_cmd_socket, conf, NULL);
    Tcl_CreateObjCommand(interp, "::mtls::init",
        (Tcl_ObjCmdProc *)mtls_cmd_init, conf, NULL);
    Tcl_CreateObjCommand(interp, "::mtls::import",
        (Tcl_ObjCmdProc *)mtls_cmd_import, conf, NULL);
    Tcl_CreateObjCommand(interp, "::mtls::handshake",
        (Tcl_ObjCmdProc *)mtls_cmd_handshake, conf, NULL);
    Tcl_CreateObjCommand(interp, "::mtls::debug",
        (Tcl_ObjCmdProc *)mtls_cmd_debug, conf, NULL);
    Tcl_CreateObjCommand(interp, "::mtls::ciphers",
        (Tcl_ObjCmdProc *)mtls_cmd_ciphers, conf, NULL);
    Tcl_CreateObjCommand(interp, "::mtls::protocols",
        (Tcl_ObjCmdProc *)mtls_cmd_protocols, conf, NULL);
    Tcl_CreateObjCommand(interp, "::mtls::version",
        (Tcl_ObjCmdProc *)mtls_cmd_version, conf, NULL);
    Tcl_CreateObjCommand(interp, "::mtls::unimport",
        (Tcl_ObjCmdProc *)mtls_cmd_unimport, conf, NULL);
    Tcl_CreateObjCommand(interp, "::mtls::status",
        (Tcl_ObjCmdProc *)mtls_cmd_status, conf, NULL);

    RETURN(OK);
}
#ifdef __cplusplus
}
#endif  /* __cplusplus */
