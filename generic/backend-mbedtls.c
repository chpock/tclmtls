/*
 * backend-mbedtls.c --
 *
 *      This file implements mbed-TLS backend.
 *
 * Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "mtlsInt.h"
#include "mtls.h"
#include <errno.h>

#if defined(__APPLE__)

#include <Security/Security.h>

static void mbedtls_set_system_ca_crl(mtls_backend_ctx *ctx) {
    ENTER(mbedtls_set_default_ca, ctx->interp);

    SecTrustSettingsDomain domains[] = { kSecTrustSettingsDomainUser,
        kSecTrustSettingsDomainAdmin, kSecTrustSettingsDomainSystem };

    for (unsigned int domain = 0; domain < 3; domain++) {

        INF("start adding certs from domain#%u", domain);
        CFArrayRef certs = NULL;
        OSStatus status = SecTrustSettingsCopyCertificates(domains[domain],
            &certs);

        if (status == errSecNoTrustSettings) {
            INF("no trust settings");
            continue;
        } else if (status != errSecSuccess) {
            WRN("failed to get trust settings");
            continue;
        }

        int added = 0;
        int failed = 0;
        int error = 0;
        int count = CFArrayGetCount(certs);
        for (int i = 0; i < count; i++) {
            SecCertificateRef cert = (void *)CFArrayGetValueAtIndex(certs, i);
            CFDataRef der;
            status = SecItemExport(cert, kSecFormatX509Cert, 0,
                NULL, &der);
            if (status != errSecSuccess) {
                error++;
            } else {
                int err = mbedtls_x509_crt_parse(&ctx->cacert,
                     CFDataGetBytePtr(der), CFDataGetLength(der));
                if (err) {
                    failed++;
                } else {
                    added++;
                }
            }
            CFRelease(der);
        }
        CFRelease(certs);

        INF("CA result - added:%d failed:%d error:%d", added, failed, error);

    }

    RETURN();
}


#elif defined(_WIN32)

#define WIN32_LEAN_AND_MEAN
#ifndef STRICT
#define STRICT // See MSDN Article Q83456
#endif
#include <windows.h>
#undef WIN32_LEAN_AND_MEAN

// TODO: function mbedtls_set_default_ca_crl should cache system CA/CRL and
// share pre-loaded certificates between all connection and threads/interps

#include <wincrypt.h>

static void mbedtls_set_system_ca_crl(mtls_backend_ctx *ctx) {
    ENTER(mbedtls_set_default_ca, ctx->interp);

    static const char* store_names[] = { "ROOT", "CA" };

    for (unsigned int store_name = 0; store_name < 2; store_name++) {

        INF("start adding certs from store [%s]", store_names[store_name]);
        HCERTSTORE store = CertOpenStore(CERT_STORE_PROV_SYSTEM_A, 0, 0,
            CERT_STORE_OPEN_EXISTING_FLAG | CERT_STORE_READONLY_FLAG |
            CERT_STORE_SHARE_CONTEXT_FLAG | CERT_SYSTEM_STORE_CURRENT_USER,
            store_names[store_name]);

        if (store == NULL) {
            WRN("failed to open the store");
            continue;
        }

        int added = 0;
        int failed = 0;
        int unknown = 0;
        const CERT_CONTEXT *cert;

        for (cert = CertEnumCertificatesInStore(store, NULL); cert != NULL;
            cert = CertEnumCertificatesInStore(store, cert))
        {
            if (cert->dwCertEncodingType == X509_ASN_ENCODING) {
                int err = mbedtls_x509_crt_parse(&ctx->cacert,
                    cert->pbCertEncoded, cert->cbCertEncoded);
                if (err) {
                    failed++;
                } else {
                    added++;
                }
            } else {
                unknown++;
            }
        }

        INF("CA result - added:%d failed:%d unknown:%d", added, failed,
            unknown);

        added = 0;
        failed = 0;
        unknown = 0;
        const CRL_CONTEXT *crl;

        for (crl = CertEnumCRLsInStore(store, NULL); crl != NULL;
            crl = CertEnumCRLsInStore(store, crl))
        {
            if (crl->dwCertEncodingType == X509_ASN_ENCODING) {
                int err = mbedtls_x509_crl_parse(&ctx->crl,
                    crl->pbCrlEncoded, crl->cbCrlEncoded);
                if (err) {
                    failed++;
                } else {
                    added++;
                }
            } else {
                unknown++;
            }
        }

        INF("CRL result - added:%d failed:%d unknown:%d", added, failed,
            unknown);

        CertCloseStore(store, 0);

    }

    RETURN();
}

#else

static void mbedtls_set_system_ca_crl(mtls_backend_ctx *ctx) {
    ENTER(mbedtls_set_default_ca, ctx->interp);

#if defined(ANDROID) || defined(__ANDROID__)
    const char *default_ca_path = "/system/etc/security/cacerts";
#else
    const char *default_ca_path = "/etc/ssl/certs";
#endif  /* ANDROID */
    INF("load default CA certs from [%s]", default_ca_path);
    int err = mbedtls_x509_crt_parse_path(&ctx->cacert, default_ca_path);
    if (err < 0) {
        char buff[128];
        mbedtls_strerror(err, buff, 128);
        WRN("error while loading: %s", buff);
    } else if (err > 0) {
        WRN("failed to load %d certificate(s)", err);
    } else {
        INF("CA certs successfully loaded");
    }

    RETURN();
}

#endif /* _WIN32 */

#ifdef TCL_THREADS
static mbedtls_entropy_context ts_entropy;

static int entropy_func(void *data, unsigned char *output, size_t len);
#endif /* TCL_THREADS */

#define MBEDTLS_ERROR_BUFF_SIZE 256

static void mbedtls_ctx_set_error_string(mtls_backend_ctx *ctx,
    const char *fmt, ...)
{
    va_list args;
    va_start(args, fmt);
    mtls_ctx_error_vset(ctx->parent_ctx, MTLS_CTX_ERROR_BACKEND, fmt, args);
    va_end(args);
}

static void mbedtls_ctx_set_error_string_code(mtls_backend_ctx *ctx,
    int code, const char *fmt, ...)
{
    char buff[512];
    va_list args;
    va_start(args, fmt);
    vsnprintf(buff, 512, fmt, args);
    va_end(args);
    int errlen = strlen(buff);
    if (errlen < (512 - 1)) {
        mbedtls_strerror(code, &buff[errlen], 512 - errlen);
    }
    mtls_ctx_error_set(ctx->parent_ctx, MTLS_CTX_ERROR_BACKEND, "%s", buff);
}

static void mbedtls_ctx_set_error_code(mtls_backend_ctx *ctx, int code) {
    char buff[512];
    mbedtls_strerror(code, buff, 512);
    mtls_ctx_error_set(ctx->parent_ctx, MTLS_CTX_ERROR_BACKEND, "%s", buff);
}

#ifdef MTLS_DEBUG_ALL
static void mbedtls_debug_callback(void *context, int mbedtls_level, const char *f_name, int line_nb, const char *line) {
    unsigned char level;
    // mbedtls levels are:
    // 0:No debug 1:Error 2:State change 3:Info 4:Verbose
    switch (mbedtls_level) {
        case 0:
            level = MTLS_LOG_BACKEND_LEVEL_NON;
            break;
        case 1:
            level = MTLS_LOG_BACKEND_LEVEL_ERR;
            break;
        case 2:
            level = MTLS_LOG_BACKEND_LEVEL_INF;
            break;
        case 3:
            level = MTLS_LOG_BACKEND_LEVEL_DBG;
            break;
        default:
            level = MTLS_LOG_BACKEND_LEVEL_TRC;
            break;
    }
    // mbedtls adds '\n\0' to its message lines. Let's make a copy without \n.
    int len = strlen(line);
    char *local_line = ckalloc(len+1);
    if (local_line == NULL) {
        // Failed to allocate memory for our line. Let's panic.
        mtls_debug(level, -1, (Tcl_Interp *)context, "%s", "failed to alloc memory for message");
    } else {
        strcpy(local_line, line);
        // Truncate by 1 byte (new line char)
        local_line[len-1] = '\0';
        mtls_debug(level, -1, (Tcl_Interp *)context, "[%s:%04d] %s", f_name, line_nb, local_line);
        ckfree(local_line);
    }
}
#endif /* MTLS_DEBUG_ALL */

static int mtls_backend_bio_error_callback(mtls_backend_ctx *ctx,
    mtls_bio_operation io, int err, int eof, int want, int actual)
{
    ENTER(backend_bio_error_callback, ctx->interp);

    int ret = actual;

    if (io == MTLS_BIO_WRITE) {
        TRC("cought write IO issue; err:%d eof:%d want:%d actual:%d", err,
            eof, want, actual);
        // We got an error while writing to the upstream channel.
        // If the error is EAGAIN, inform mbedtls to try later.
        if (actual < 0 && err == EAGAIN) {
            ret = MBEDTLS_ERR_SSL_WANT_WRITE;
            TRC("instruct mbedtls to try again later");
        }
    } else {
        TRC("cought read IO issue; err:%d eof:%d want:%d actual:%d", err,
            eof, want, actual);
        // We got an error while reading from the upstream channel.
        // EOF case should be handled by mtls_bio_read. Here we should decide what to do
        // with actual == 0 and actual < 0
        if (actual < 0) {
            // We got some kind of IO error. If it is EAGAIN, then retry.
            if (err == EAGAIN) {
                ret = MBEDTLS_ERR_SSL_WANT_READ;
                TRC("instruct mbedtls to try again later");
            } else {
                // something else. Close the connection.
                ret = 0;
            }
        } else if (actual == 0) {
            // we have not read anything, but EOF has not been reached.
            // Let's try again.
            ret = MBEDTLS_ERR_SSL_WANT_READ;
            TRC("instruct mbedtls to try again later");
        }
    }

    UNUSED(eof);
    UNUSED(want);
    RETURN(INT, ret);
    UNUSED(ctx);
}

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
) {
    ENTER(backend_ctx_init, interp);

    int err;

    // The request param is ignored as for now
    UNUSED(request);

    DBG("setup backend context");
    ctx->bio_error_callback = mtls_backend_bio_error_callback;
    ctx->parent_ctx = parent_ctx;
    ctx->interp = interp;
    ctx->ciphers = NULL;
    ctx->alpnlen = 0;

    // Make sure memory references are valid in case we exit early
#ifndef TCL_THREADS
    // Entropy
    mbedtls_entropy_init(&ctx->entropy);
#endif /* TCL_THREADS */
    // Random number generator
    mbedtls_ctr_drbg_init(&ctx->ctr_drbg);
    // Trusted CA
    mbedtls_x509_crt_init(&ctx->cacert);
    // Client certificate
    mbedtls_x509_crt_init(&ctx->clicert);
    // Private key
    mbedtls_pk_init(&ctx->pk);
    // CRL
    mbedtls_x509_crl_init(&ctx->crl);
    // Configuration to use within TLS
    mbedtls_ssl_config_init(&ctx->config);
    // TLS context
    mbedtls_ssl_init(&ctx->ssl);
#ifdef MTLS_ENABLE_SERVER
    // Diffie-Hellman-Merkle key exchange
    mbedtls_dhm_init(&ctx->dhm);
#endif /* MTLS_ENABLE_SERVER */

#ifdef MTLS_DEBUG_ALL
    INF("setup debug callback");
    mbedtls_ssl_conf_dbg(&ctx->config, mbedtls_debug_callback, ctx->interp);
    mbedtls_debug_set_threshold(4);
#endif /* MTLS_DEBUG_ALL */

    INF("seed the radom number generator");
#ifdef TCL_THREADS
    err = mbedtls_ctr_drbg_seed(&ctx->ctr_drbg, entropy_func, &ts_entropy, NULL, 0);
#else /* TCL_THREADS */
    err = mbedtls_ctr_drbg_seed(&ctx->ctr_drbg, mbedtls_entropy_func, &ctx->entropy, NULL, 0);
#endif /* TCL_THREADS */
    if (err) {
        mbedtls_ctx_set_error_code(ctx, err);
        RETURN(ERROR);
    }

    /* Load the trusted CA from file */
    if (cafile != NULL) {
        INF("load the trusted CA from file [%s]", cafile);
        err = mbedtls_x509_crt_parse_file(&ctx->cacert, cafile);
        if (err < 0) {
            mbedtls_ctx_set_error_code(ctx, err);
            RETURN(ERROR);
        } else if (err > 0) {
            mbedtls_ctx_set_error_string(ctx, "Failed to parse %d CA"
                " certificate(s) from the file: %s", err, cafile);
            RETURN(ERROR);
        }
    } else {
        INF("no trusted CA file specified");
    }

    /* Load the trusted CA from directory */
    if (cadir != NULL) {
        INF("load the trusted CA from directory [%s]", cadir);
        err = mbedtls_x509_crt_parse_path(&ctx->cacert, cadir);
        if (err < 0) {
            mbedtls_ctx_set_error_code(ctx, err);
            RETURN(ERROR);
        } else if (err > 0) {
            mbedtls_ctx_set_error_string(ctx, "Failed to parse %d CA"
                " certificate(s) from the directory: %s", err, cadir);
            RETURN(ERROR);
        }
    } else {
        INF("no trusted CA directory specified");
    }

    // TODO: add here CRL handling. Call mbedtls_set_system_ca_crl below only
    // if CRL is not specified.

    if (cafile == NULL && cadir == NULL) {
        mbedtls_set_system_ca_crl(ctx);
    }

    INF("add CA/crl to config");
    mbedtls_ssl_conf_ca_chain(&ctx->config, &ctx->cacert, &ctx->crl);

    /* Load the client certificate */
    if (cert != NULL) {
        INF("load the client certificate");
        err = mbedtls_x509_crt_parse(&ctx->clicert,
            (const unsigned char *)cert, strlen(cert) + 1);
        if (err < 0) {
            mbedtls_ctx_set_error_string_code(ctx, err, "Failed to load"
                "specified client certificate: ");
            RETURN(ERROR);
        } else if (err > 0) {
            mbedtls_ctx_set_error_string(ctx, "Failed to parse %d client"
                " certificate(s)", err);
            RETURN(ERROR);
        }
    } else {
        INF("no client certificate specified");
    }

    /* Load the client certificate from file */
    if (certfile != NULL) {
        INF("load the client certificate from file [%s]", certfile);
        err = mbedtls_x509_crt_parse_file(&ctx->clicert, certfile);
        if (err < 0) {
            mbedtls_ctx_set_error_string_code(ctx, err, "Failed to load"
                " the client certificate from the file \"%s\": ", certfile);
            RETURN(ERROR);
        } else if (err > 0) {
            mbedtls_ctx_set_error_string(ctx, "Failed to parse %d client"
                " certificate(s) from the file: %s", err, certfile);
            RETURN(ERROR);
        }
        if (cafile != NULL) {
            /*
               Add the trusted CA to client certificate chain, as SSL servers
               should provide as much information their certificate's chain
               as possible. See:
               https://github.com/Mbed-TLS/mbedtls/issues/5

               Also, tcltls behaves in the same way:
               https://github.com/chpock/tcltls/blob/5075dde49b132919f1eb4912bf2ccabb974b6d45/generic/tls.c#L2128-L2131

            */
            INF("load the trusted CA as client cert");
            mbedtls_x509_crt *cert_cur = &ctx->cacert;
            int i = 0;
            while (cert_cur != NULL) {
                if (cert_cur->raw.p != NULL) {
                    INF("add CA cert #%d to the client cert chain", i++);
                    mbedtls_x509_crt_parse_der(&ctx->clicert, cert_cur->raw.p,
                        cert_cur->raw.len);
                }
                cert_cur = cert_cur->next;
            }
            UNUSED(i);
        }
    } else {
        INF("no client certificate file specified");
    }

    if (keyfile != NULL || key != NULL) {
        if (keyfile != NULL) {
            INF("load the private key from file [%s]", keyfile);
            err = mbedtls_pk_parse_keyfile(&ctx->pk, keyfile,
                keypassword, mbedtls_ctr_drbg_random,
                &ctx->ctr_drbg);
        } else {
            INF("load the private key");
            err = mbedtls_pk_parse_key(&ctx->pk, (const unsigned char *)key,
                strlen(key) + 1, (const unsigned char *)keypassword,
                (keypassword == NULL ? 0 : strlen(keypassword)),
                mbedtls_ctr_drbg_random, &ctx->ctr_drbg);
        }
        if (err) {
            mbedtls_ctx_set_error_code(ctx, err);
            RETURN(ERROR);
        }
        if (!(mbedtls_pk_can_do(&ctx->pk, MBEDTLS_PK_RSA) ||
            mbedtls_pk_can_do(&ctx->pk, MBEDTLS_PK_ECKEY)))
        {
            mbedtls_ctx_set_error_code(ctx, MBEDTLS_ERR_PK_TYPE_MISMATCH);
            RETURN(ERROR);
        }
    }

    if (cert != NULL || certfile != NULL) {
        INF("add client certificate/private key to config");
        mbedtls_ssl_conf_own_cert(&ctx->config, &ctx->clicert, &ctx->pk);
    }

    int endpoint;
    if (server) {
        INF("endpoint is: %s", "SERVER");
        endpoint = MBEDTLS_SSL_IS_SERVER;
    } else {
        INF("endpoint is: %s", "CLIENT");
        endpoint = MBEDTLS_SSL_IS_CLIENT;
    }
    err = mbedtls_ssl_config_defaults(&ctx->config,
        endpoint, MBEDTLS_SSL_TRANSPORT_STREAM,
        MBEDTLS_SSL_PRESET_DEFAULT);
    if (err) {
        mbedtls_ctx_set_error_code(ctx, err);
        RETURN(ERROR);
    }

    /* Set TLS version */
    // Validate enabled TLS version. mbedTLS supports only TLS 1.2/1.3.
    if (tls1_1) {
        WRN("TLS 1.1 is not supported by mbedTLS");
    }
    if (!tls1_2 && !tls1_3) {
        mbedtls_ctx_set_error_string(ctx, "both TLS1.2 and TLS1.3 protocols"
            " are disabled", err);
        RETURN(ERROR);
    }
    if (!tls1_2) {
        INF("%s TLS1.2, %s TLS1.3", "disable", "enable");
        mbedtls_ssl_conf_min_tls_version(&ctx->config,
            MBEDTLS_SSL_VERSION_TLS1_3);
        mbedtls_ssl_conf_max_tls_version(&ctx->config,
            MBEDTLS_SSL_VERSION_TLS1_3);
    } else if (!tls1_3) {
        INF("%s TLS1.2, %s TLS1.3", "enable", "disable");
        mbedtls_ssl_conf_min_tls_version(&ctx->config,
            MBEDTLS_SSL_VERSION_TLS1_2);
        mbedtls_ssl_conf_max_tls_version(&ctx->config,
            MBEDTLS_SSL_VERSION_TLS1_2);
    } else {
        INF("%s TLS1.2, %s TLS1.3", "enable", "enable");
        mbedtls_ssl_conf_min_tls_version(&ctx->config,
            MBEDTLS_SSL_VERSION_TLS1_2);
        mbedtls_ssl_conf_max_tls_version(&ctx->config,
            MBEDTLS_SSL_VERSION_TLS1_3);
    }

    /* Set validation mode */
    if (require) {
        INF("set SSL verify mode: %s", "REQUIRED");
        mbedtls_ssl_conf_authmode(&ctx->config, MBEDTLS_SSL_VERIFY_REQUIRED);
    } else {
        INF("set SSL verify mode: %s", "NONE");
        mbedtls_ssl_conf_authmode(&ctx->config, MBEDTLS_SSL_VERIFY_NONE);
    }

    /* Assign the random number generator to the TLS config. */
    mbedtls_ssl_conf_rng(&ctx->config, mbedtls_ctr_drbg_random, &ctx->ctr_drbg);

    INF("assign config to ssl");
    /* Assign the TLS config to the TLS context. */
    err = mbedtls_ssl_setup(&ctx->ssl, &ctx->config);
    if (err) {
        mbedtls_ctx_set_error_code(ctx, err);
        RETURN(ERROR);
    }

    if (cipherslen) {
        INF("add specified ciphers");
        ctx->ciphers = (int *)ckalloc(cipherslen * sizeof(int));
        if (ctx->ciphers == NULL) {
            mbedtls_ctx_set_error_string(ctx, "failed to alloc chiphers");
            RETURN(ERROR);
        }
        for (int i = 0; i < cipherslen; i++) {
            int cipher_id = mbedtls_ssl_get_ciphersuite_id(ciphers[i]);
            if (cipher_id == 0) {
                mbedtls_ctx_set_error_string(ctx, "invalid cipher \"%s\""
                    " specified", ciphers[i]);
                RETURN(ERROR);
            }
            TRC("add cipher [%s] with id [%d]", ciphers[i], cipher_id);
            ctx->ciphers[i] = cipher_id;
        }
        mbedtls_ssl_conf_ciphersuites(&ctx->config, ctx->ciphers);
    } else {
        INF("add all supported siphers");
        mbedtls_ssl_conf_ciphersuites(&ctx->config,
            mbedtls_ssl_list_ciphersuites());
    }

#ifdef MTLS_ENABLE_SERVER
    if (dhparams != NULL) {
        INF("load the DH parameters from file [%s]", dhparams);
        err = mbedtls_dhm_parse_dhmfile(&ctx->dhm, dhparams);
        if (err) {
            mbedtls_ctx_set_error_code(ctx, err);
            RETURN(ERROR);
        }
        err = mbedtls_ssl_conf_dh_param_ctx(&ctx->config, &ctx->dhm);
        if (err) {
            mbedtls_ctx_set_error_code(ctx, err);
            RETURN(ERROR);
        }
    } else {
        INF("no DH parameters file specified");
    }
#else
    UNUSED(dhparams);
#endif /* MTLS_ENABLE_SERVER */

    INF("set renegotiation: %s", "ENABLED");
    mbedtls_ssl_conf_renegotiation(&ctx->config,
        MBEDTLS_SSL_RENEGOTIATION_ENABLED);

#ifdef MTLS_ENABLE_CLIENT
    INF("set session tickets: %s", "DISABLED");
    mbedtls_ssl_conf_session_tickets(&ctx->config,
        MBEDTLS_SSL_SESSION_TICKETS_DISABLED);
#endif /* MTLS_ENABLE_CLIENT */

    // TODO: add here session handling

    /*
        Warning: If the hostname is not set with this function, Mbed TLS will
        silently skip certificate verification entirely. Always set
        the hostname with this function - even when not using SNI!
    */
    if (servername != NULL) {
        INF("set hostname [%s]", servername);
        err = mbedtls_ssl_set_hostname(&ctx->ssl, servername);
        if (err) {
            mbedtls_ctx_set_error_code(ctx, err);
            RETURN(ERROR);
        }
    } else {
        WRN("no hostname specified, certificate verification will be skipped");
    }

    if (alpnlen) {
        INF("add specified alpn");
        ctx->alpn = (const char **)ckalloc((alpnlen + 1) * sizeof(char *));
        if (ctx->alpn == NULL) {
            mbedtls_ctx_set_error_string(ctx, "failed to alloc alpn");
            RETURN(ERROR);
        }
        int i;
        for (i = 0; i <= alpnlen; i++) {
            ctx->alpn[i] = NULL;
        }
        ctx->alpnlen = alpnlen;
        for (i = 0; i < alpnlen; i++) {
            int buffsize = strlen(alpn[i]) + 1;
            ctx->alpn[i] = (char *)ckalloc(buffsize);
            if (ctx->alpn[i] == NULL) {
                mbedtls_ctx_set_error_string(ctx, "failed to alloc alpn");
                RETURN(ERROR);
            }
            memcpy((char *)ctx->alpn[i], alpn[i], buffsize);
            TRC("add alpn [%s]", ctx->alpn[i]);
        }
        err = mbedtls_ssl_conf_alpn_protocols(&ctx->config, ctx->alpn);
        if (err) {
            mbedtls_ctx_set_error_code(ctx, err);
            RETURN(ERROR);
        }
    } else {
        INF("no alpn specified");
    }

    INF("set BIO for mbedtls");
    mbedtls_ssl_set_bio(&ctx->ssl, ctx->parent_ctx, mtls_bio_write, mtls_bio_read, NULL);

    RETURN(OK);

}

int mtls_backend_ctx_connect(mtls_backend_ctx *ctx) {
    ENTER(backend_ctx_connect, ctx->interp);

    int ret;

    INF("try handshake...");

    ret = mbedtls_ssl_handshake(&ctx->ssl);

    if (ret == 0) {
        // all good
        RETURN(OK);
    }

    // something not so good happened

    if (ret == MBEDTLS_ERR_SSL_WANT_READ) {
        DBG("returned: WANT_READ");
        RETURN(CONTINUE);
    }

    if (ret == MBEDTLS_ERR_SSL_WANT_WRITE) {
        DBG("returned: WANT_WRITE");
        RETURN(CONTINUE);
    }

    if (ret == MBEDTLS_ERR_SSL_ASYNC_IN_PROGRESS) {
        DBG("returned: ASYNC_IN_PROGRESS");
        RETURN(CONTINUE);
    }

    if (ret == MBEDTLS_ERR_SSL_CRYPTO_IN_PROGRESS) {
        DBG("returned: CRYPTO_IN_PROGRESS");
        RETURN(CONTINUE);
    }

    // something really bad happened

    uint32_t flags;
    if (ret == MBEDTLS_ERR_SSL_HELLO_VERIFY_REQUIRED) {
        ERR("unexpected HELLO_VERIFY_REQUIRED");
        mbedtls_ctx_set_error_string(ctx, "unexpected HELLO_VERIFY_REQUIRED");
    } else if (ret == MBEDTLS_ERR_X509_CERT_VERIFY_FAILED &&
        (flags = mbedtls_ssl_get_verify_result(&ctx->ssl)) != 0)
    {
        // Try to show the exact certificate verification result rather than
        // general error:
        // "X509 - Certificate verification failed, e.g. CRL, CA or signature
        // check failed"
        char verify_buf[256];
        mbedtls_x509_crt_verify_info(verify_buf, sizeof(verify_buf),
            " ", flags);
        // mbedTLS possibly has a few failures when validating a certificate.
        // All of them will be separated by newline character. And also,
        // newline character will be at the end of the string.
        // Let's remove the newline character at the end of the string and
        // replace the newline characters in the middle of the string
        // with ';'.
        unsigned int len = strlen(verify_buf);
        if (len > 0) {
            if (len >= sizeof(verify_buf)) {
                len = sizeof(verify_buf) - 1;
            }
            // Remove trailing newline
            verify_buf[--len] = '\0';
            // Replace '\n' by ';'
            for (unsigned int i = 0; i < len; i++) {
                if (verify_buf[i] == '\n') {
                    verify_buf[i] = ';';
                }
            }
            mbedtls_ctx_set_error_string(ctx, "Certificate verification"
                " failed:%s", verify_buf);
        }
    } else {
        ERR("mtls_backend_ctx_connect: somethig wrong [%d]", ret);
        mbedtls_ctx_set_error_code(ctx, ret);
    }

    RETURN(INT, ret);
}

int mtls_backend_ctx_write(mtls_backend_ctx *ctx, const char *buf,
    int bufSize, int *errorCodePtr)
{
    ENTER(backend_ctx_write, ctx->interp);

    DBG("want to write [%d] bytes", bufSize);

    int written = mbedtls_ssl_write(&ctx->ssl, (const unsigned char *)buf,
        (size_t)bufSize);

    if (written >= 0) {
        *errorCodePtr = 0;
        DBG("wrote [%d] bytes", written);
        RETURN(INT, written);
    }

    switch (written) {
    case MBEDTLS_ERR_SSL_WANT_WRITE:
        INF("got MBEDTLS_ERR_SSL_WANT_WRITE, return EAGAIN");
        *errorCodePtr = EAGAIN;
        RETURN(INT, -1);
        break;
    case MBEDTLS_ERR_SSL_WANT_READ:
        INF("got MBEDTLS_ERR_SSL_WANT_READ, return EAGAIN");
        *errorCodePtr = EAGAIN;
        RETURN(INT, -1);
        break;
    case MBEDTLS_ERR_SSL_ASYNC_IN_PROGRESS:
        INF("got MBEDTLS_ERR_SSL_ASYNC_IN_PROGRESS, return EAGAIN");
        *errorCodePtr = EAGAIN;
        RETURN(INT, -1);
        break;
    case MBEDTLS_ERR_SSL_CRYPTO_IN_PROGRESS:
        INF("got MBEDTLS_ERR_SSL_CRYPTO_IN_PROGRESS, return EAGAIN");
        *errorCodePtr = EAGAIN;
        RETURN(INT, -1);
        break;
    case MBEDTLS_ERR_SSL_RECEIVED_EARLY_DATA:
        INF("got MBEDTLS_ERR_SSL_RECEIVED_EARLY_DATA, return EAGAIN");
        *errorCodePtr = EAGAIN;
        RETURN(INT, -1);
        break;
    default:
        ERR("got custom error code: [%d]", written);
        *errorCodePtr = ECONNRESET;
        mbedtls_ctx_set_error_code(ctx, written);
        RETURN(INT, -1);
        break;
    }
}

int mtls_backend_ctx_read(mtls_backend_ctx *ctx, char *buf, int bufSize,
    int *errorCodePtr)
{
    ENTER(backend_ctx_read, ctx->interp);

    DBG("want to read [%d] bytes", bufSize);

again: ; // empty statement

    int read = mbedtls_ssl_read(&ctx->ssl, (unsigned char *)buf,
        (size_t)bufSize);

    if (read >= 0) {
        *errorCodePtr = 0;
        DBG("read [%d] bytes", read);
        RETURN(INT, read);
    }

    switch (read) {
    case MBEDTLS_ERR_SSL_CLIENT_RECONNECT:
        INF("got MBEDTLS_ERR_SSL_CLIENT_RECONNECT, return ECONNRESET");
        *errorCodePtr = ECONNRESET;
        mbedtls_ctx_set_error_string(ctx, "The client initiated a reconnect"
            " from the same port");
        RETURN(INT, -1);
        break;
    case MBEDTLS_ERR_SSL_WANT_READ:
        INF("got MBEDTLS_ERR_SSL_WANT_READ, retry reading");
        *errorCodePtr = EAGAIN;
        RETURN(INT, -1);
        break;
    case MBEDTLS_ERR_SSL_WANT_WRITE:
        INF("got MBEDTLS_ERR_SSL_WANT_WRITE, return EAGAIN");
        *errorCodePtr = EAGAIN;
        RETURN(INT, -1);
        break;
    case MBEDTLS_ERR_SSL_ASYNC_IN_PROGRESS:
        INF("got MBEDTLS_ERR_SSL_ASYNC_IN_PROGRESS, return EAGAIN");
        *errorCodePtr = EAGAIN;
        RETURN(INT, -1);
        break;
    case MBEDTLS_ERR_SSL_CRYPTO_IN_PROGRESS:
        INF("got MBEDTLS_ERR_SSL_CRYPTO_IN_PROGRESS, return EAGAIN");
        *errorCodePtr = EAGAIN;
        RETURN(INT, -1);
        break;
    case MBEDTLS_ERR_SSL_RECEIVED_EARLY_DATA:
        INF("got MBEDTLS_ERR_SSL_RECEIVED_EARLY_DATA, return EAGAIN");
        *errorCodePtr = EAGAIN;
        RETURN(INT, -1);
        break;
    case MBEDTLS_ERR_SSL_RECEIVED_NEW_SESSION_TICKET:
        // accodring to https://github.com/espressif/esp-idf/issues/13494#issuecomment-2031069784
        // we should continue to read after this error
        INF("got MBEDTLS_ERR_SSL_RECEIVED_NEW_SESSION_TICKET, retry reading");
        goto again;
        break;
    case MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY:
        INF("got MBEDTLS_ERR_SSL_PEER_CLOSE_NOTIFY, return EOF");
        *errorCodePtr = 0;
        // zero read bytes means EOF in Tcl core
        RETURN(INT, 0);
        break;
    default:
        ERR("got custom error code: [%d], set ECONNRESET", read);
        *errorCodePtr = ECONNRESET;
        mbedtls_ctx_set_error_code(ctx, read);
        RETURN(INT, -1);
        break;
    }

}

Tcl_Obj *mtls_backend_ctx_get_status(mtls_backend_ctx *ctx,
    mtls_backend_ctx_status_type type)
{
    ENTER(backend_ctx_get_status, ctx->interp);

    Tcl_Obj *obj = NULL;

    switch (type) {
    case MTLS_BACKEND_CTX_CIPHER:
        obj = Tcl_NewStringObj(mbedtls_ssl_get_ciphersuite(&ctx->ssl), -1);
        break;
    case MTLS_BACKEND_CTX_PROTOCOL:
        obj = Tcl_NewStringObj(mbedtls_ssl_get_version(&ctx->ssl), -1);
        break;
    case MTLS_BACKEND_CTX_CHIPHER_KEY_BITLEN: ; // empty statement
        int suite_id = mbedtls_ssl_get_ciphersuite_id_from_ssl(&ctx->ssl);
        const mbedtls_ssl_ciphersuite_t *ciphersuite_info;
        ciphersuite_info = mbedtls_ssl_ciphersuite_from_id(suite_id);
        int sbits = mbedtls_ssl_ciphersuite_get_cipher_key_bitlen(ciphersuite_info);
        obj = Tcl_NewSizeIntFromObj(sbits);
        break;
    default:
        DBG("try to get peer certificate");
        const mbedtls_x509_crt *cert = mbedtls_ssl_get_peer_cert(&ctx->ssl);
        DBG("got peer cert: %p", (void *)cert);
        if (cert == NULL) {
            goto no_cert_avail;
        }
        switch (type) {
        case MTLS_BACKEND_CTX_SERIAL:
            break;
        case MTLS_BACKEND_CTX_FINGERPRINT_SHA1: ; // empty statement
            char hash_sha1[20];
            mbedtls_sha1(cert->raw.p, cert->raw.len, (unsigned char *)hash_sha1);
            obj = Tcl_NewObj();
            mtls_string_to_hex(hash_sha1, 20, obj);
            break;
        case MTLS_BACKEND_CTX_FINGERPRINT_SHA256: ; // empty statement
            char hash_sha256[32];
            mbedtls_sha256(cert->raw.p, cert->raw.len, (unsigned char *)hash_sha256, 0);
            obj = Tcl_NewObj();
            mtls_string_to_hex(hash_sha256, 32, obj);
            break;
        default:
            break;
        }
        break;
    }

no_cert_avail:

    RETURN(PTR, obj);
}

int mtls_backend_ctx_close(mtls_backend_ctx *ctx) {
    ENTER(backend_ctx_close, ctx->interp);
    mbedtls_ssl_close_notify(&ctx->ssl);
    RETURN(OK);
}

int mtls_backend_ctx_free(mtls_backend_ctx *ctx) {
    ENTER(backend_ctx_free, ctx->interp);

    INF("free mbedtls structures");
    mbedtls_ssl_free(&ctx->ssl);
    mbedtls_ssl_config_free(&ctx->config);
    mbedtls_ctr_drbg_free(&ctx->ctr_drbg);
#ifndef TCL_THREADS
    mbedtls_entropy_free(&ctx->entropy);
#endif /* TCL_THREADS */
    mbedtls_x509_crt_free(&ctx->cacert);
    mbedtls_x509_crt_free(&ctx->clicert);
    mbedtls_pk_free(&ctx->pk);
    mbedtls_x509_crl_free(&ctx->crl);
#ifdef MTLS_ENABLE_SERVER
    mbedtls_dhm_free(&ctx->dhm);
#endif /* MTLS_ENABLE_SERVER */

    if (ctx->ciphers != NULL) {
        ckfree(ctx->ciphers);
    }

    if (ctx->alpnlen) {
        while (ctx->alpnlen) {
            ctx->alpnlen--;
            if (ctx->alpn[ctx->alpnlen] != NULL) {
                ckfree((void *)ctx->alpn[ctx->alpnlen]);
            }
        }
        ckfree(ctx->alpn);
    }

    RETURN(OK);
}

const char *mtls_backend_version = "mbedTLS " MBEDTLS_VERSION_STRING;

const char *mtls_backend_get_version(Tcl_Interp *interp)
{
    ENTER(backend_get_version, interp);
    RETURN(PTR, mtls_backend_version);
    UNUSED(interp);
}

int mtls_backend_get_ciphers(Tcl_Interp *interp, mtls_protocol protocol,
    int verbose, int supported,
    void (*push_callback)(Tcl_Interp *, const char *, ClientData),
    ClientData clientdata)
{
    ENTER(backend_get_ciphers, interp);

    TRC("check protocol [%s], verbose:%d supported:%d",
        mtls_protocol_strings[protocol], verbose, supported);

    // verbose and supported flags don't work in mbedTLS backend
    UNUSED(verbose);
    UNUSED(supported);

    // mbedTLS supports only TLS 1.2 and TLS 1.3
    if ((protocol != MTLS_PROTO_TLS1_2) && (protocol != MTLS_PROTO_TLS1_3)) {
        RETURN(ERROR);
    }

    const int *list = mbedtls_ssl_list_ciphersuites();
    while (*list) {

        const mbedtls_ssl_ciphersuite_t *cs =
            mbedtls_ssl_ciphersuite_from_id(*list);

        // We only want to get TLS 1.2 ciphers, but the minimum TLS version
        // for the current cipher is 1.3, then skip this cipher.
        if (protocol == MTLS_PROTO_TLS1_2 && cs->private_min_tls_version
            >= MBEDTLS_SSL_VERSION_TLS1_3)
        {
            goto next;
        }

        // We only want to get TLS 1.3 ciphers, but the maximum TLS version
        // for the current cipher is 1.2, then skip this cipher.
        if (protocol == MTLS_PROTO_TLS1_3 && cs->private_max_tls_version
            <= MBEDTLS_SSL_VERSION_TLS1_2)
        {
            goto next;
        }

        push_callback(interp, mbedtls_ssl_get_ciphersuite_name(*list),
            clientdata);

next:

        list++;

    }

    RETURN(OK);
}

#ifdef TCL_THREADS

static int entropy_initialized = 0;
static Tcl_Mutex entropy_mx[2];

static int entropy_func(void *data, unsigned char *output, size_t len) {
  int ret;
  Tcl_MutexLock(&entropy_mx[1]);
  ret = mbedtls_entropy_func(data, output, len);
  Tcl_MutexUnlock(&entropy_mx[1]);
  return ret;
}

#endif /* TCL_THREADS */

int mtls_backend_init(Tcl_Interp *interp) {
    ENTER(backend_init, interp);

#ifdef TCL_THREADS
    INF("threaded mode detected");
    Tcl_MutexLock(&entropy_mx[0]);
    if (!entropy_initialized) {
        INF("initialize thread-shared entropy");
        mbedtls_entropy_init(&ts_entropy);
    } else {
        INF("thread-shared entropy has already been initialized");
    }
    entropy_initialized++;
    Tcl_MutexUnlock(&entropy_mx[0]);
#else
    INF("non-threaded mode detected");
#endif /* TCL_THREADS */

#ifdef MBEDTLS_SSL_PROTO_TLS1_3
    INF("init psa crypto");
    int err = psa_crypto_init();
    if (err != PSA_SUCCESS) {
        Tcl_SetObjResult(interp, Tcl_ObjPrintf("failed to initialize"
            " PSA Crypto: %d", err));
        RETURN(ERROR);
    }
#else
    INF("psa crypto is not needed");
#endif /* MBEDTLS_SSL_PROTO_TLS1_3 */

    RETURN(OK);
}

int mtls_backend_free(Tcl_Interp *interp) {
    ENTER(backend_init, interp);

#ifdef MBEDTLS_SSL_PROTO_TLS1_3
    mbedtls_psa_crypto_free();
#endif /* MBEDTLS_SSL_PROTO_TLS1_3 */

#ifdef TCL_THREADS
    INF("threaded mode detected");
    Tcl_MutexLock(&entropy_mx[0]);
    if (entropy_initialized) {
        if (--entropy_initialized) {
            INF("thread-shared entropy is in use by other interp");
        } else {
            INF("free thread-shared entropy");
            mbedtls_entropy_free(&ts_entropy);
        }
    } else {
        INF("thread-shared entropy has already been freed");
    }
    Tcl_MutexUnlock(&entropy_mx[0]);
#endif /* TCL_THREADS */

    RETURN(OK);
    UNUSED(interp);
}