/*
 * mtlsInt.h --
 *
 *      This header file contains internal declarations needed for
 *      all of the source files in this package.
 *
 * Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifndef _MTLSINT_H
#define _MTLSINT_H

#include <tcl.h>
#include <string.h>

#if defined(MTLS_DEBUG_ALL)
#define MTLS_DEBUG_LEVEL 4
#elif defined(MTLS_DEBUG_INFO)
#define MTLS_DEBUG_LEVEL 3
#elif defined(MTLS_DEBUG_WARN)
#define MTLS_DEBUG_LEVEL 2
#elif defined(MTLS_DEBUG_ERR)
#define MTLS_DEBUG_LEVEL 1
#else
#define MTLS_DEBUG_LEVEL 0
#endif

#if MTLS_DEBUG_LEVEL >= 4
#include <stdio.h>
#define DBG(...) _MSG(DBG, __VA_ARGS__)
#define TRC(...) _MSG(TRC, __VA_ARGS__)
#define DLog(...) {fprintf(stderr,"[mTLS DEBUG] "); fprintf(stderr,__VA_ARGS__); fprintf(stderr,"\r\n"); fflush(stderr);}
#define DLogNoNL(...) {fprintf(stderr,"[mTLS DEBUG] "); fprintf(stderr,__VA_ARGS__); fflush(stderr);}
#else
#define DBG(...) {}
#define TRC(...) {}
#define DLog(...) {}
#define DLogNoNL(...) {}
#endif

#if MTLS_DEBUG_LEVEL >= 3
#define INF(...) _MSG(INF, __VA_ARGS__)
#else
#define INF(...) {}
#endif

#if MTLS_DEBUG_LEVEL >= 2
#define WRN(...) _MSG(WRN, __VA_ARGS__)
#else
#define WRN(...) {}
#endif

#if MTLS_DEBUG_LEVEL >= 1
#define ERR(...) _MSG(ERR, __VA_ARGS__)
#else
#define ERR(...) {}
#endif

#define UNUSED(expr) do { (void)(expr); } while (0)

/*
 * Backwards compatibility for size type change
 */
#if TCL_MAJOR_VERSION < 9 && TCL_MINOR_VERSION < 7
    #ifndef Tcl_Size
        typedef int Tcl_Size;
    #endif

    #define TCL_SIZE_MODIFIER ""
#endif

typedef enum {
    MTLS_BIO_READ,
    MTLS_BIO_WRITE
} mtls_bio_operation;

typedef enum {
    MTLS_PROTO_SSL2,
    MTLS_PROTO_SSL3,
    MTLS_PROTO_TLS1,
    MTLS_PROTO_TLS1_1,
    MTLS_PROTO_TLS1_2,
    MTLS_PROTO_TLS1_3
} mtls_protocol;

#ifdef _TCLMTLS_H

const char *mtls_protocol_strings[] = {
    "ssl2", "ssl3", "tls1", "tls1.1", "tls1.2", "tls1.3", NULL
};

const char *__debug[] = {
    /*  0 - func name             */ "mtls_",
    /*  1 -                       */ "::mtls::debug_level",
    /*  2 -                       */ "",
    /*  3 -                       */ "",
    /*  4 -                       */ "",
    /*  5 -                       */ "",
    /*  6 - enter - format        */ "=> %s%s",
    /*  7 -                       */ "",
    /*  8 -                       */ "",
    /*  9 -                       */ "",
    /* 10 - return - void         */ "<= %s%s (void)",
    /* 11 - return - string       */ "<= %s%s (return: %s)",
    /* 12 - return - integer      */ "<= %s%s (return: %i)",
    /* 13 - return - ptr          */ "<= %s%s (return: %p)",
    /* 14 -                       */ "",
    /* 15 -                       */ "",
    /* 16 -                       */ "",
    /* 17 -                       */ "",
    /* 18 -                       */ "",
    /* 19 -                       */ "",
    /* 20 - return - TCL_OK       */ "TCL_OK",
    /* 21 - return - TCL_ERROR    */ "TCL_ERROR",
    /* 22 - return - TCL_CONTINUE */ "TCL_CONTINUE",
    /* 23 -                       */ "",
    /* 24 -                       */ "",
    /* 25 -                       */ "",
    /* 26 -                       */ "",
    /* 27 -                       */ "",
    /* 28 -                       */ "",
    /* 29 -                       */ "",
    /* 30 - return - EINVAL       */ "EINVAL",
    /* 31 - return - EAGAIN       */ "EAGAIN"
};

#else
extern const char *__debug[];
extern const char *mtls_protocol_strings[];
extern const char *mtls_protocol_strings2[];
#endif /* _TCLMTLS_H */

#define MTLS_LOG_LEVEL_NON 0
#define MTLS_LOG_LEVEL_ERR 1
#define MTLS_LOG_LEVEL_WRN 2
#define MTLS_LOG_LEVEL_INF 3
#define MTLS_LOG_LEVEL_DBG 4
#define MTLS_LOG_LEVEL_TRC 5

#define MTLS_LOG_BACKEND_LEVEL_NON (MTLS_LOG_LEVEL_NON << 3)
#define MTLS_LOG_BACKEND_LEVEL_ERR (MTLS_LOG_LEVEL_ERR << 3)
#define MTLS_LOG_BACKEND_LEVEL_WRN (MTLS_LOG_LEVEL_WRN << 3)
#define MTLS_LOG_BACKEND_LEVEL_INF (MTLS_LOG_LEVEL_INF << 3)
#define MTLS_LOG_BACKEND_LEVEL_DBG (MTLS_LOG_LEVEL_DBG << 3)
#define MTLS_LOG_BACKEND_LEVEL_TRC (MTLS_LOG_LEVEL_TRC << 3)

#define __XCONCAT(a, b) a##_##b
#define __EVAL_CONCAT(a, b) __XCONCAT(a, b)

#define _MSG(l, ...) \
    mtls_debug(MTLS_LOG_LEVEL_##l, __current_debug_level, __current_interp, __VA_ARGS__)


#define ENTER(func, interp) \
    int __current_debug_level = 0; \
    Tcl_Interp *__current_interp = ((interp) == NULL || Tcl_InterpDeleted((interp))) ? NULL : (interp); \
    if (__current_interp != NULL) { \
        Tcl_Obj *__current_debug_level_obj = Tcl_GetVar2Ex(__current_interp, \
            __debug[1], NULL, TCL_GLOBAL_ONLY); \
        if (__current_debug_level_obj != NULL) { \
            if (Tcl_GetIntFromObj(__current_interp, __current_debug_level_obj, \
                &__current_debug_level) == TCL_ERROR) { \
                    Tcl_ResetResult(__current_interp); \
            } \
        } \
    } \
    const char *__current_func = #func; \
    INF(__debug[6], __debug[0], __current_func)

#define __GET_ARG20(_1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, ...) _20
#define __IS_ONE_TWO_MANY(...) __GET_ARG20(__VA_ARGS__, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, N, 0, 1)
#define __IS_EXACT_NARGS(...) __GET_ARG20(__VA_ARGS__, N, 9, N, 8, N, 7, N, 6, N, 5, N, 4, N, 3, N, 2, 0, 1)
#define ___IS_EMPTY() ,
#define __IS_EMPTY(...) ___IS_EMPTY __VA_ARGS__ ()
#define _NARGS(...) __IS_ONE_TWO_MANY(__IS_EMPTY(__VA_ARGS__), __VA_ARGS__)
#define _NARGS_EXACT(...) __IS_EXACT_NARGS(__IS_EMPTY(__VA_ARGS__), __VA_ARGS__)

#define SET_RESULT(t, ...) __XCONCAT(SET_RESULT, t)(__VA_ARGS__)

#define SET_RESULT_OBJECT(v) \
    Tcl_SetObjResult(__current_interp, (v))

#define SET_RESULT_INT(v) SET_RESULT_OBJECT(Tcl_NewIntObj(v))

#define SET_RESULT_STRING(v) SET_RESULT_OBJECT(Tcl_NewStringObj((v), -1))

#define SET_RESULT_FORMAT(...) SET_RESULT_OBJECT(Tcl_ObjPrintf(__VA_ARGS__))

#define APPEND_RESULT(...) \
    Tcl_AppendResult(__current_interp, __VA_ARGS__, (char *)NULL)

#define SET_ERROR(...) \
    Tcl_SetErrorCode(__current_interp, "MTLS", __VA_ARGS__, NULL)

#define RETURN_0() \
    INF(__debug[10], __debug[0], __current_func); \
    UNUSED(__current_interp); \
    UNUSED(__current_func); \
    return

#define RETURN_1(r) __XCONCAT(RETURN_1, r)

#define RETURN_1_0 RETURN_N_INT(0)

#define RETURN_1_1 RETURN_N_INT(1)

#define RETURN_1_OK RETURN_N_TCL_OK()

#define RETURN_1_ERROR RETURN_N_TCL_ERROR()

#define RETURN_1_CONTINUE RETURN_N_TCL_CONTINUE()

#define RETURN_1_POSIX_EINVAL \
    INF(__debug[11], __debug[0], __current_func, __debug[30]); \
    UNUSED(__current_interp); \
    UNUSED(__current_func); \
    return EINVAL

#define RETURN_N(t, ...) __XCONCAT(RETURN_N, t)(__VA_ARGS__)

#define RETURN_N_PTR(v) \
    INF(__debug[13], __debug[0], __current_func, (v)); \
    UNUSED(__current_interp); \
    UNUSED(__current_func); \
    return (v)

#define RETURN_N_INT(v) \
    INF(__debug[12], __debug[0], __current_func, (v)); \
    UNUSED(__current_interp); \
    UNUSED(__current_func); \
    return (v)

#define RETURN_N_TCL_OK() \
    INF(__debug[11], __debug[0], __current_func, __debug[20]); \
    UNUSED(__current_interp); \
    UNUSED(__current_func); \
    return TCL_OK

#define RETURN_N_TCL_ERROR() \
    INF(__debug[11], __debug[0], __current_func, __debug[21]); \
    UNUSED(__current_interp); \
    UNUSED(__current_func); \
    return TCL_ERROR

#define RETURN_N_TCL_CONTINUE() \
    INF(__debug[11], __debug[0], __current_func, __debug[22]); \
    UNUSED(__current_interp); \
    UNUSED(__current_func); \
    return TCL_CONTINUE

#define RETURN(...) __EVAL_CONCAT(RETURN, _NARGS(__VA_ARGS__))(__VA_ARGS__)

#ifdef USE_MBEDTLS
#include "backend-mbedtls.h"
#endif

#endif /* _MTLSINT_H */