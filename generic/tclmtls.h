/*
 * tclmtls.h --
 *
 *      This header file contains general function declarations.
 *
 * Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifndef _TCLMTLS_H
#define _TCLMTLS_H

#include "mtlsInt.h"

// Import mode for mtls_import_config
//
// MTLS_IMPORT_CONFIG_OVERRIDE: complete override of destination by source,
// including NULL values.
//
// source   | destination | result in destination
// NULL     | NULL        | NULL
// A        | NULL        | A
// A        | B           | A
// NULL     | B           | NULL
//
// MTLS_IMPORT_CONFIG_MERGE: override only the defined source values, but leave
// the undefined source values as they were at the destination.
//
// source   | destination | result in destination
// NULL     | NULL        | NULL
// A        | NULL        | A
// A        | B           | A
// NULL     | B           | B
//
// MTLS_IMPOTT_CONFIG_UPDATE: override only those values that are
// not defined in the destination.
//
// source   | destination | result in destination
// NULL     | NULL        | NULL
// A        | NULL        | A
// A        | B           | B
// NULL     | B           | B

typedef enum {
    MTLS_IMPORT_CONFIG_OVERRIDE,
    MTLS_IMPORT_CONFIG_MERGE,
    MTLS_IMPORT_CONFIG_UPDATE
} mtls_import_config_type;

typedef enum {
    MTLS_CONF_CADIR,
    MTLS_CONF_CAFILE,
    MTLS_CONF_CERTFILE,
    MTLS_CONF_CERT,
    MTLS_CONF_CIPHER,
    MTLS_CONF_COMMAND,
    MTLS_CONF_DHPARAMS,
    MTLS_CONF_KEYFILE,
    MTLS_CONF_KEY,
    MTLS_CONF_MODEL,
    MTLS_CONF_PASSWORD,
    MTLS_CONF_REQUEST,
    MTLS_CONF_REQUIRE,
    MTLS_CONF_SERVER,
    MTLS_CONF_SERVERNAME,
    MTLS_CONF_SSL2,
    MTLS_CONF_SSL3,
    MTLS_CONF_TLS1,
    MTLS_CONF_TLS1_1,
    MTLS_CONF_TLS1_2,
    MTLS_CONF_TLS1_3,
    MTLS_CONF_ALPN,
    MTLS_CONF_AUTOSERVERNAME,
    MTLS_CONF_CIPHERSUITES,
    MTLS_CONF_POST_HANDSHAKE,
    MTLS_CONF_SECURITYLEVEL,
    MTLS_CONF_SESSION_ID,
    MTLS_CONF_VALIDATECOMMAND,
    MTLS_CONF_VCMD,
    MTLS_CONF_ACCEPT,
    MTLS_CONF_SIZE
} mtls_config_name;

typedef Tcl_Obj *mtls_config[MTLS_CONF_SIZE];

static const char *const mtls_config_strings[] = {
    "-cadir", "-cafile", "-certfile", "-cert", "-cipher",
    "-command", "-dhparams", "-keyfile", "-key", "-model",
    "-password", "-request", "-require", "-server", "-servername",
    "-ssl2", "-ssl3", "-tls1", "-tls1.1", "-tls1.2", "-tls1.3",
    "-alpn", "-autoservername",
    "-ciphersuites", "-post_handshake", "-security_level",
    "-session_id", "-validatecommand",
    "-vcmd",
    "-accept",
    NULL
};

#ifdef MTLS_BACKEND_FEATURE_SSL2
#define MTLS_PKGCONFIG_PROTOCOL_SSL2 "1"
#else
#define MTLS_PKGCONFIG_PROTOCOL_SSL2 "0"
#endif

#ifdef MTLS_BACKEND_FEATURE_SSL3
#define MTLS_PKGCONFIG_PROTOCOL_SSL3 "1"
#else
#define MTLS_PKGCONFIG_PROTOCOL_SSL3 "0"
#endif

#ifdef MTLS_BACKEND_FEATURE_TLS1
#define MTLS_PKGCONFIG_PROTOCOL_TLS1 "1"
#else
#define MTLS_PKGCONFIG_PROTOCOL_TLS1 "0"
#endif

#ifdef MTLS_BACKEND_FEATURE_TLS1_1
#define MTLS_PKGCONFIG_PROTOCOL_TLS1_1 "1"
#else
#define MTLS_PKGCONFIG_PROTOCOL_TLS1_1 "0"
#endif

#ifdef MTLS_BACKEND_FEATURE_TLS1_2
#define MTLS_PKGCONFIG_PROTOCOL_TLS1_2 "1"
#else
#define MTLS_PKGCONFIG_PROTOCOL_TLS1_2 "0"
#endif

#ifdef MTLS_BACKEND_FEATURE_TLS1_3
#define MTLS_PKGCONFIG_PROTOCOL_TLS1_3 "1"
#else
#define MTLS_PKGCONFIG_PROTOCOL_TLS1_3 "0"
#endif

#ifdef MTLS_ENABLE_CLIENT
#define MTLS_PKGCONFIG_CLIENT "1"
#else
#define MTLS_PKGCONFIG_CLIENT "0"
#endif

#ifdef MTLS_ENABLE_SERVER
#define MTLS_PKGCONFIG_SERVER "1"
#else
#define MTLS_PKGCONFIG_SERVER "0"
#endif

static Tcl_Config const pkgconfig[] = {
    {"version",         PACKAGE_VERSION},
    {"client",          MTLS_PKGCONFIG_CLIENT},
    {"server",          MTLS_PKGCONFIG_SERVER},
    {"ssl2,protocol",   MTLS_PKGCONFIG_PROTOCOL_SSL2},
    {"ssl3,protocol",   MTLS_PKGCONFIG_PROTOCOL_SSL3},
    {"tls1,protocol",   MTLS_PKGCONFIG_PROTOCOL_TLS1},
    {"tls1.1,protocol", MTLS_PKGCONFIG_PROTOCOL_TLS1_1},
    {"tls1.2,protocol", MTLS_PKGCONFIG_PROTOCOL_TLS1_2},
    {"tls1.3,protocol", MTLS_PKGCONFIG_PROTOCOL_TLS1_3},
    {"version,backend", MTLS_BACKEND_VERSION},
    {"name,backend",    MTLS_BACKEND_NAME},
    {"type,backend",    MTLS_BACKEND_TYPE},
    {NULL, NULL}
};

/*
 * For C++ compilers, use extern "C"
 */

#ifdef __cplusplus
extern "C" {
#endif

/*
 * Only the _Init function is exported.
 */

extern DLLEXPORT int Mtls_Init(Tcl_Interp * interp);

/*
 * end block for C++
 */

#ifdef __cplusplus
}
#endif

#endif /* _TCLMTLS_H */
