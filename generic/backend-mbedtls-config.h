/*
 * backend-mbedtls-config.h --
 *
 * Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#define MBEDTLS_DEPRECATED_REMOVED
#define MBEDTLS_CHECK_RETURN_WARNING
#undef MBEDTLS_SSL_CONTEXT_SERIALIZATION
#undef MBEDTLS_SSL_PROTO_DTLS
#define MBEDTLS_X509_TRUSTED_CERTIFICATE_CALLBACK
#undef MBEDTLS_NET_C
#undef MBEDTLS_X509_CSR_PARSE_C
#undef MBEDTLS_X509_CREATE_C
#undef MBEDTLS_X509_CRT_WRITE_C
#undef MBEDTLS_X509_CSR_WRITE_C
#define MBEDTLS_VERSION_C
#define MBEDTLS_VERSION_FEATURES

#ifdef MTLS_ENABLE_CLIENT
#define MBEDTLS_SSL_CLI_C
#else
#undef MBEDTLS_SSL_CLI_C
#endif

#ifdef MTLS_ENABLE_SERVER
#define MBEDTLS_SSL_SRV_C
#else
#undef MBEDTLS_SSL_SRV_C
#endif

#ifdef MTLS_DEBUG_ALL

#define MBEDTLS_DEBUG_C
#define MBEDTLS_SSL_DEBUG_ALL

#else

#undef MBEDTLS_DEBUG_C
#undef MBEDTLS_SSL_DEBUG_ALL

#endif /* MTLS_DEBUG_ALL */