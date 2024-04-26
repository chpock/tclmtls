/*
 * mtls.h --
 *
 *      This header file contains general function declarations.
 *
 * Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifndef _MTLS_H
#define _MTLS_H

#include "mtlsInt.h"

typedef enum {
    MTLS_CTX_ERROR,
    MTLS_CTX_ERROR_GENERAL,
    MTLS_CTX_ERROR_BACKEND,
    MTLS_CTX_ERROR_IO,
    MTLS_CTX_ERROR_SIZE
} mtls_ctx_error_type;

typedef char *mtls_ctx_error[MTLS_CTX_ERROR_SIZE];

#define MTLS_CTX_ERROR_BUFF_SIZE 512

typedef enum {
    MTLS_CTX_STATE_PREINIT,      /* ctx has just been created and has not yet */
                                 /* been initialized */
    MTLS_CTX_STATE_INIT,         /* ctx is initialized but is not connected */
    MTLS_CTX_STATE_HANDSHAKE,    /* handshake initiated but not completed */
    MTLS_CTX_STATE_CONNECTED,    /* ctx is in normal and connected state */
    MTLS_CTX_STATE_ERROR         /* ctx is in error mode */
} mtls_ctx_state;

typedef struct mtls_ctx {
    void *tcl_config;

    Tcl_Channel upstream_chan;
    Tcl_Channel self_chan;

    int watchMask;
    Tcl_TimerToken timer;

    int flags;
    mtls_ctx_state state;
    mtls_ctx_error error;

    Tcl_Interp *interp;
    Tcl_Obj *callback;

    mtls_backend_ctx backend;

} mtls_ctx;

#define MTLS_CTX_FLAG_ASYNC    (1<<0) /* non-blocking mode */
#define MTLS_CTX_FLAG_CLOSED   (1<<1) /* contex is closed */

#if MTLS_DEBUG_LEVEL >= 1
void mtls_debug(unsigned char level, int clevel, Tcl_Interp *interp, const char *fmt, ...);
#endif /* MTLS_DEBUG_LEVEL */

int mtls_init(Tcl_Interp *interp);
int mtls_free(Tcl_Interp *interp);
int mtls_get_ciphers(Tcl_Interp *interp, mtls_protocol protocol,
    int verbose, int supported, Tcl_Obj *obj);
int mtls_get_version(Tcl_Interp *interp, Tcl_Obj *obj);

int mtls_bio_write(void *bio, const unsigned char *buf, size_t blen);
int mtls_bio_read(void *bio, unsigned char *buf, size_t blen);

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
);
int mtls_ctx_connect(mtls_ctx *ctx);
int mtls_ctx_close(mtls_ctx *ctx);
int mtls_ctx_free(mtls_ctx *ctx);
int mtls_ctx_get_status(mtls_ctx *ctx, int is_local, Tcl_Obj *obj);

void mtls_ctx_error_set(mtls_ctx *ctx, mtls_ctx_error_type type,
    const char *fmt, ...);
void mtls_ctx_error_vset(mtls_ctx *ctx, mtls_ctx_error_type type,
    const char *fmt, va_list args);
const char *mtls_ctx_error_get(mtls_ctx *ctx);

#endif /* _MTLS_H */
