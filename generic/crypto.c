/*
 * crypto.c --
 *
 *      Generic-purpose Tcl commands (::mtls::randombytes, ::mtls::aesgcm-
 *      encrypt, ::mtls::aesgcm-decrypt) built on the mbedtls this package
 *      already links for TLS. Not tied to a socket/connection, so they get
 *      their own lazily-seeded CTR_DRBG rather than reusing backend-mbedtls.c's
 *      connection-scoped entropy plumbing.
 *
 * See the file "license.terms" for information on usage and redistribution
 * of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#include "mtlsInt.h"
#include "mtls.h"
#include <mbedtls/gcm.h>
#include <mbedtls/ctr_drbg.h>
#include <mbedtls/entropy.h>

#define MTLS_CRYPTO_KEY_LEN     32
#define MTLS_CRYPTO_NONCE_LEN   12
#define MTLS_CRYPTO_TAG_LEN     16
#define MTLS_CRYPTO_MAX_RANDOM  (1024 * 1024)

static mbedtls_entropy_context crypto_entropy;
static mbedtls_ctr_drbg_context crypto_ctr_drbg;
static int crypto_rng_initialized = 0;
#ifdef TCL_THREADS
static Tcl_Mutex crypto_rng_mx;
#endif /* TCL_THREADS */

/* Lazily seed the shared DRBG on first use. Safe from any thread - a seeded
 * mbedtls_ctr_drbg_context is no different from any other shared mbedtls
 * context this package already hands across threads. */
static int crypto_rng_ensure_seeded(Tcl_Interp *interp) {
    int ret = 0;
#ifdef TCL_THREADS
    Tcl_MutexLock(&crypto_rng_mx);
#endif /* TCL_THREADS */
    if (!crypto_rng_initialized) {
        mbedtls_entropy_init(&crypto_entropy);
        mbedtls_ctr_drbg_init(&crypto_ctr_drbg);
        ret = mbedtls_ctr_drbg_seed(&crypto_ctr_drbg, mbedtls_entropy_func,
            &crypto_entropy, NULL, 0);
        if (ret == 0) {
            crypto_rng_initialized = 1;
        }
    }
#ifdef TCL_THREADS
    Tcl_MutexUnlock(&crypto_rng_mx);
#endif /* TCL_THREADS */
    if (ret != 0) {
        Tcl_SetObjResult(interp, Tcl_ObjPrintf(
            "failed to seed random number generator: -0x%04x", -ret));
        Tcl_SetErrorCode(interp, "MTLS", "CRYPTO", "RNG", NULL);
        return TCL_ERROR;
    }
    return TCL_OK;
}

/* ::mtls::randombytes count */
static int mtls_cmd_randombytes(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    UNUSED(clientData);
    ENTER(cmd_randombytes, interp);

    if (objc != 2) {
        Tcl_WrongNumArgs(interp, 1, objv, "count");
        RETURN(ERROR);
    }

    Tcl_WideInt count;
    if (Tcl_GetWideIntFromObj(interp, objv[1], &count) != TCL_OK) {
        RETURN(ERROR);
    }
    if (count < 0 || count > MTLS_CRYPTO_MAX_RANDOM) {
        SET_RESULT(FORMAT, "count must be between 0 and %d",
            MTLS_CRYPTO_MAX_RANDOM);
        SET_ERROR("CRYPTO", "RANGE");
        RETURN(ERROR);
    }

    if (crypto_rng_ensure_seeded(interp) != TCL_OK) {
        RETURN(ERROR);
    }

    unsigned char *buf = (unsigned char *)Tcl_Alloc((unsigned int)count);
    int ret = mbedtls_ctr_drbg_random(&crypto_ctr_drbg, buf, (size_t)count);
    if (ret != 0) {
        Tcl_Free((char *)buf);
        Tcl_SetObjResult(interp, Tcl_ObjPrintf(
            "failed to generate random bytes: -0x%04x", -ret));
        SET_ERROR("CRYPTO", "RNG");
        RETURN(ERROR);
    }

    Tcl_SetObjResult(interp, Tcl_NewByteArrayObj(buf, (Tcl_Size)count));
    Tcl_Free((char *)buf);

    RETURN(OK);
}

/* ::mtls::aesgcm-encrypt key plaintext
 * key must be 32 bytes (AES-256). Returns nonce(12) || ciphertext || tag(16),
 * with a fresh random nonce generated internally each call. */
static int mtls_cmd_aesgcm_encrypt(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    UNUSED(clientData);
    ENTER(cmd_aesgcm_encrypt, interp);

    if (objc != 3) {
        Tcl_WrongNumArgs(interp, 1, objv, "key plaintext");
        RETURN(ERROR);
    }

    Tcl_Size keyLen;
    unsigned char *key = Tcl_GetBytesFromObj(interp, objv[1], &keyLen);
    if (key == NULL) {
        RETURN(ERROR);
    }
    if (keyLen != MTLS_CRYPTO_KEY_LEN) {
        SET_RESULT(FORMAT, "key must be %d bytes", MTLS_CRYPTO_KEY_LEN);
        SET_ERROR("CRYPTO", "KEYLEN");
        RETURN(ERROR);
    }

    Tcl_Size ptLen;
    unsigned char *pt = Tcl_GetBytesFromObj(interp, objv[2], &ptLen);
    if (pt == NULL) {
        RETURN(ERROR);
    }

    if (crypto_rng_ensure_seeded(interp) != TCL_OK) {
        RETURN(ERROR);
    }

    Tcl_Size outLen = MTLS_CRYPTO_NONCE_LEN + ptLen + MTLS_CRYPTO_TAG_LEN;
    unsigned char *out = (unsigned char *)Tcl_Alloc((unsigned int)outLen);
    unsigned char *nonce = out;
    unsigned char *ct    = out + MTLS_CRYPTO_NONCE_LEN;
    unsigned char *tag   = out + MTLS_CRYPTO_NONCE_LEN + ptLen;

    int ret = mbedtls_ctr_drbg_random(&crypto_ctr_drbg, nonce,
        MTLS_CRYPTO_NONCE_LEN);

    mbedtls_gcm_context gcm;
    mbedtls_gcm_init(&gcm);
    if (ret == 0) {
        ret = mbedtls_gcm_setkey(&gcm, MBEDTLS_CIPHER_ID_AES, key,
            MTLS_CRYPTO_KEY_LEN * 8);
    }
    if (ret == 0) {
        ret = mbedtls_gcm_crypt_and_tag(&gcm, MBEDTLS_GCM_ENCRYPT,
            (size_t)ptLen, nonce, MTLS_CRYPTO_NONCE_LEN, NULL, 0, pt, ct,
            MTLS_CRYPTO_TAG_LEN, tag);
    }
    mbedtls_gcm_free(&gcm);

    if (ret != 0) {
        Tcl_Free((char *)out);
        Tcl_SetObjResult(interp, Tcl_ObjPrintf(
            "AES-GCM encryption failed: -0x%04x", -ret));
        SET_ERROR("CRYPTO", "ENCRYPT");
        RETURN(ERROR);
    }

    Tcl_SetObjResult(interp, Tcl_NewByteArrayObj(out, outLen));
    Tcl_Free((char *)out);

    RETURN(OK);
}

/* ::mtls::aesgcm-decrypt key ciphertext
 * ciphertext is nonce(12) || ciphertext || tag(16), as produced by
 * aesgcm-encrypt. Fails (MTLS CRYPTO AUTH) if the tag doesn't verify. */
static int mtls_cmd_aesgcm_decrypt(ClientData clientData, Tcl_Interp *interp,
    int objc, Tcl_Obj *const objv[])
{
    UNUSED(clientData);
    ENTER(cmd_aesgcm_decrypt, interp);

    if (objc != 3) {
        Tcl_WrongNumArgs(interp, 1, objv, "key ciphertext");
        RETURN(ERROR);
    }

    Tcl_Size keyLen;
    unsigned char *key = Tcl_GetBytesFromObj(interp, objv[1], &keyLen);
    if (key == NULL) {
        RETURN(ERROR);
    }
    if (keyLen != MTLS_CRYPTO_KEY_LEN) {
        SET_RESULT(FORMAT, "key must be %d bytes", MTLS_CRYPTO_KEY_LEN);
        SET_ERROR("CRYPTO", "KEYLEN");
        RETURN(ERROR);
    }

    Tcl_Size inLen;
    unsigned char *in = Tcl_GetBytesFromObj(interp, objv[2], &inLen);
    if (in == NULL) {
        RETURN(ERROR);
    }
    if (inLen < MTLS_CRYPTO_NONCE_LEN + MTLS_CRYPTO_TAG_LEN) {
        SET_RESULT(STRING, "ciphertext too short");
        SET_ERROR("CRYPTO", "SHORT");
        RETURN(ERROR);
    }

    const unsigned char *nonce = in;
    const unsigned char *ct    = in + MTLS_CRYPTO_NONCE_LEN;
    Tcl_Size ctLen = inLen - MTLS_CRYPTO_NONCE_LEN - MTLS_CRYPTO_TAG_LEN;
    const unsigned char *tag   = in + MTLS_CRYPTO_NONCE_LEN + ctLen;

    /* avoid Tcl_Alloc(0), not guaranteed non-NULL on all platforms */
    unsigned char *pt = (unsigned char *)Tcl_Alloc(
        (unsigned int)(ctLen > 0 ? ctLen : 1));

    mbedtls_gcm_context gcm;
    mbedtls_gcm_init(&gcm);
    int ret = mbedtls_gcm_setkey(&gcm, MBEDTLS_CIPHER_ID_AES, key,
        MTLS_CRYPTO_KEY_LEN * 8);
    if (ret == 0) {
        ret = mbedtls_gcm_auth_decrypt(&gcm, (size_t)ctLen, nonce,
            MTLS_CRYPTO_NONCE_LEN, NULL, 0, tag, MTLS_CRYPTO_TAG_LEN, ct, pt);
    }
    mbedtls_gcm_free(&gcm);

    if (ret != 0) {
        Tcl_Free((char *)pt);
        SET_RESULT(STRING, "AES-GCM authentication failed");
        SET_ERROR("CRYPTO", "AUTH");
        RETURN(ERROR);
    }

    Tcl_SetObjResult(interp, Tcl_NewByteArrayObj(pt, ctLen));
    Tcl_Free((char *)pt);

    RETURN(OK);
}

void mtls_register_crypto_commands(Tcl_Interp *interp) {
    Tcl_CreateObjCommand(interp, "::mtls::randombytes",
        (Tcl_ObjCmdProc *)mtls_cmd_randombytes, NULL, NULL);
    Tcl_CreateObjCommand(interp, "::mtls::aesgcm-encrypt",
        (Tcl_ObjCmdProc *)mtls_cmd_aesgcm_encrypt, NULL, NULL);
    Tcl_CreateObjCommand(interp, "::mtls::aesgcm-decrypt",
        (Tcl_ObjCmdProc *)mtls_cmd_aesgcm_decrypt, NULL, NULL);
}
