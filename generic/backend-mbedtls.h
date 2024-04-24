/*
 * backend-mbedtls.h --
 *
 *      This file defines mbed-TLS backend headers.
 *
 * Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifndef _BACKEND_MBEDTLS_H
#define _BACKEND_MBEDTLS_H

#include <mbedtls/version.h>
#include <mbedtls/net_sockets.h>
#include <mbedtls/entropy.h>
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/error.h>

#ifdef MTLS_DEBUG_ALL
#include <mbedtls/debug.h>
#endif /* MTLS_DEBUG */

#define MTLS_BACKEND_TYPE \
    "mbedtls"
#define MTLS_BACKEND_VERSION \
    MBEDTLS_VERSION_STRING
#define MTLS_BACKEND_NAME \
    "mbedTLS " MBEDTLS_VERSION_STRING
#define MTLS_BACKEND_FEATURE_TLS1_2
#define MTLS_BACKEND_FEATURE_TLS1_3

typedef enum {
    MTLS_BACKEND_CTX_CIPHER,
    MTLS_BACKEND_CTX_PROTOCOL,
    MTLS_BACKEND_CTX_CHIPHER_KEY_BITLEN
} mtls_backend_ctx_status_type;

typedef struct mtls_ctx mtls_ctx;
typedef struct mtls_backend_ctx mtls_backend_ctx;

typedef struct mtls_backend_ctx {

    int (*bio_error_callback)(mtls_backend_ctx *ctx, mtls_bio_operation io,
        int err, int eof, int want, int actual);

    Tcl_Interp *interp;

    mtls_ctx *parent_ctx;

    mbedtls_ctr_drbg_context ctr_drbg;
    mbedtls_ssl_context ssl;
    mbedtls_ssl_config config;
    mbedtls_x509_crt cacert;
    mbedtls_x509_crt clicert;
    mbedtls_x509_crl crl;
    mbedtls_pk_context pk;
#ifdef MTLS_ENABLE_SERVER
    mbedtls_dhm_context dhm;
#endif
#ifndef TCL_THREADS
    mbedtls_entropy_context entropy;
#endif
    int *ciphers;
    const char **alpn;
    int alpnlen;

} mtls_backend_ctx;

int mtls_backend_init(Tcl_Interp *interp);
int mtls_backend_free(Tcl_Interp *interp);

int mtls_backend_get_ciphers(Tcl_Interp *interp, mtls_protocol protocol,
    int verbose, int supported,
    void (*push_callback)(Tcl_Interp *, const char *, ClientData),
    ClientData clientdata);

const char *mtls_backend_get_version(Tcl_Interp *interp);

int mtls_backend_ctx_init(
    mtls_backend_ctx *ctx,
    Tcl_Interp *interp,
    mtls_ctx *parent_ctx,
    const char *cadir,
    const char *cafile,
    const char *certfile,
    const char *cert,
    const char *ciphers[],
    int cipherslen,
    const char *alpn[],
    int alpnlen,
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
int mtls_backend_ctx_connect(mtls_backend_ctx *ctx);
int mtls_backend_ctx_close(mtls_backend_ctx *ctx);
int mtls_backend_ctx_free(mtls_backend_ctx *ctx);
int mtls_backend_ctx_read(mtls_backend_ctx *ctx, char *buf, int bufSize,
    int *errorCodePtr);
int mtls_backend_ctx_write(mtls_backend_ctx *ctx, const char *buf,
    int bufSize, int *errorCodePtr);
Tcl_Obj *mtls_backend_ctx_get_status(mtls_backend_ctx *ctx,
    mtls_backend_ctx_status_type type);

#endif /* _BACKEND_MBEDTLS_H */