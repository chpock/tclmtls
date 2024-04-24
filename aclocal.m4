#
# Include the TEA standard macro set
#

builtin(include,tclconfig/tcl.m4)

#
# Add here whatever m4 macros you want to define for your package
#

AC_DEFUN([TCLMTLS_SET_DEBUG], [

    AC_ARG_ENABLE(debug,
        AS_HELP_STRING([--enable-debug=<level>],
            [enable debug messages. Possible levels are: none, error, warning, info, on (default: none)]
        ), [
            mtlsdebug="$enableval"
            if test -z "$mtlsdebug"; then
                mtlsdebug="all"
            elif test "all" = "$mtlsdebug"; then
                mtlsdebug="all"
            elif test "trace" = "$mtlsdebug"; then
                mtlsdebug="all"
            elif test "yes" = "$mtlsdebug"; then
                mtlsdebug="all"
            elif test "no" = "$mtlsdebug"; then
                mtlsdebug="none"
            fi
        ], [
            mtlsdebug="none"
        ]
    )

    AC_MSG_CHECKING([for enabled debugging level])
    if test "$mtlsdebug" = "none"; then
        AC_MSG_RESULT([$mtlsdebug])
    elif test "$mtlsdebug" = "all"; then
        AC_MSG_RESULT([$mtlsdebug])
        AC_DEFINE(MTLS_DEBUG_ALL)
        LDFLAGS="$LDFLAGS -g -fno-omit-frame-pointer"
        TEA_ADD_CFLAGS([-g -fno-omit-frame-pointer])
    elif test "$mtlsdebug" = "info"; then
        AC_MSG_RESULT([$mtlsdebug])
        AC_DEFINE(MTLS_DEBUG_INFO)
    elif test "$mtlsdebug" = "warning"; then
        AC_MSG_RESULT([$mtlsdebug])
        AC_DEFINE(MTLS_DEBUG_WARN)
    elif test "$mtlsdebug" = "error"; then
        AC_MSG_RESULT([$mtlsdebug])
        AC_DEFINE(MTLS_DEBUG_ERR)
    else
        AC_MSG_RESULT([$mtlsdebug])
        AC_MSG_ERROR([unknown debug level: '$mtlsdebug'])
    fi

])

AC_DEFUN([TCLMTLS_CHECK_MBEDTLS], [
    AC_MSG_CHECKING([for Mbed-TLS root directory])
    AC_ARG_WITH([mbedtls],
        AS_HELP_STRING([--with-mbedtls=<dir>],
            [path to root directory of Mbed-TLS installation]
        ), [
            mbedtlsdir="$withval"
        ], [
            mbedtlsdir="no"
        ]
    )
    AC_MSG_RESULT([$mbedtlsdir])

    AC_MSG_CHECKING([for Mbed-TLS include directory])
    AC_ARG_WITH([mbedtls-include],
        AS_HELP_STRING([--with-mbedtls-include=<dir>],
            [path to include directory of Mbed-TLS installation]
        ), [
            mbedtlsincdir="$withval"
        ], [
            if test "$mbedtlsdir" != "no"; then
                mbedtlsincdir="$mbedtlsdir/include"
            else
                mbedtlsincdir="no"
            fi
        ]
    )
    AC_MSG_RESULT([$mbedtlsincdir])

    AC_MSG_CHECKING([for mbedtls/ssl.h])
    if test "$mbedtlsincdir" != "no"; then
        if test -f "$mbedtlsincdir/mbedtls/ssl.h"; then
            AC_MSG_RESULT([ok])
        else
            AC_MSG_RESULT([fail])
            AC_MSG_ERROR([Unable to locate mbedtls/ssl.h])
        fi
    else
            AC_MSG_RESULT([skip])
    fi

    AC_MSG_CHECKING([for Mbed-TLS lib directory])
    AC_ARG_WITH([mbedtls-lib],
        AS_HELP_STRING([--with-mbedtls-lib=<dir>],
            [path to lib directory of Mbed-TLS installation]
        ), [
            mbedtlslibdir="$withval"
        ], [
            if test "$mbedtlsdir" != "no"; then
                mbedtlslibdir="$mbedtlsdir/lib"
            else
                mbedtlslibdir="no"
            fi
        ]
    )
    AC_MSG_RESULT([$mbedtlslibdir])

    AC_MSG_CHECKING([for libmbedtls.*])
    if test "$mbedtlslibdir" != "no"; then
        if test -f "$mbedtlslibdir/libmbedtls${SHLIB_SUFFIX}"; then
            AC_MSG_RESULT([ok, shared])
        else
            if test -f "$mbedtlslibdir/libmbedtls.a"; then
                AC_MSG_RESULT([ok, static])
            else
                AC_MSG_RESULT([fail])
                AC_MSG_ERROR([Unable to locate libmbedtls.*])
            fi
        fi
    else
            AC_MSG_RESULT([skip])
    fi

    if test "$mbedtlsincdir" != "no"; then
        TEA_ADD_INCLUDES([-I\"`${CYGPATH} $mbedtlsincdir`\"])
    fi
    if test "$mbedtlslibdir" != "no"; then
        TEA_ADD_LIBS([-L"$mbedtlslibdir"])
    fi

    if test "$mbedtlsincdir" != "no" || test "$mbedtlslibdir" != "no"; then
        if test "${TEA_PLATFORM}" = "windows" ; then
            TEA_ADD_LIBS([-lbcrypt -lws2_32])
        fi
        TEA_ADD_LIBS([-lmbedtls -lmbedx509 -lmbedcrypto])

        TEA_ADD_SOURCES([backend-mbedtls.c])
        AC_DEFINE(USE_MBEDTLS)
        mtlsbackend="mbedtls"
    fi

])

AC_DEFUN([TCLMTLS_ADD_OBJECTS], [
    vars="$@"
    for i in $vars; do
        PKG_OBJECTS="$PKG_OBJECTS $i"
    done
    AC_SUBST(PKG_OBJECTS)
])

AC_DEFUN([TCLMTLS_MBEDTLS_DEFINE], [
    vars="$@"
    for i in $vars; do
        MBEDTLS_CFLAGS="$MBEDTLS_CFLAGS -D$i"
        TEA_ADD_CFLAGS([-D$i])
    done
    AC_SUBST(MBEDTLS_CFLAGS)
])

AC_DEFUN([TCLMTLS_CHECK_DEFAULT_BACKEND], [
    AC_MSG_CHECKING([for the default SSL backend])
    if test -z "$mtlsbackend"; then
        AC_MSG_RESULT([yes])
        if test "${TEA_PLATFORM}" = "windows" ; then
            TEA_ADD_LIBS([-lbcrypt -lws2_32])
        fi
        TEA_ADD_SOURCES([backend-mbedtls.c])
        TEA_ADD_INCLUDES([-I\"`${CYGPATH} ${srcdir}/mbedtls/include`\" -I\"`${CYGPATH} ${srcdir}/generic`\"])
        TCLMTLS_ADD_OBJECTS([
            mbedtls/library/libmbedtls.a
            mbedtls/library/libmbedx509.a
            mbedtls/library/libmbedcrypto.a
        ])
        AC_DEFINE(USE_MBEDTLS)
        mtlsbackend="mbedtls"

        TCLMTLS_MBEDTLS_DEFINE([MBEDTLS_USER_CONFIG_FILE=\\\"backend-mbedtls-config.h\\\"])
        MBEDTLS_CFLAGS="$MBEDTLS_CFLAGS -I\$(srcdir_abs)/generic"
        MBEDTLS_CFLAGS="$MBEDTLS_CFLAGS -fno-asynchronous-unwind-tables"
        MBEDTLS_CFLAGS="$MBEDTLS_CFLAGS -ffunction-sections -fdata-sections"
        MBEDTLS_CFLAGS="$MBEDTLS_CFLAGS -fvisibility=hidden"

        if test "${TEA_PLATFORM}" = "windows" ; then
            MBEDTLS_MAKEFLAGS="WINDOWS=1"
        fi

        AC_MSG_CHECKING([for debugging support in the default SSL backend])
        if test "$mtlsdebug" = "all"; then
            TCLMTLS_MBEDTLS_DEFINE(MTLS_DEBUG_ALL)
            AC_MSG_RESULT([yes])
        else
            AC_MSG_RESULT([no])
        fi

        AC_SUBST(MBEDTLS_CFLAGS)
        AC_SUBST(MBEDTLS_MAKEFLAGS)
    else
        AC_MSG_RESULT([no])
    fi
])

AC_DEFUN([TCLMTLS_SET_LDFLAGS], [

    _LDFLAGS="$LDFLAGS"
    _CFLAGS="$CFLAGS"
    LDFLAGS="-Wl,-Map=conftest.map"
    CFLAGS=
    AC_MSG_CHECKING([whether LD supports -Wl,-Map=])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([])],[
        AC_MSG_RESULT([yes])
        LDFLAGS="$_LDFLAGS -Wl,-Map=\$@.map"
    ],[
        AC_MSG_RESULT([no])
        LDFLAGS="$_LDFLAGS"
    ])
    CFLAGS="$_CFLAGS"

    _LDFLAGS="$LDFLAGS"
    _CFLAGS="$CFLAGS"
    LDFLAGS="-Wl,--gc-sections"
    CFLAGS=
    AC_MSG_CHECKING([whether LD supports -Wl,--gc-sections])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([])],[
        AC_MSG_RESULT([yes])
        LDFLAGS="$_LDFLAGS -Wl,--gc-sections -Wl,--as-needed"
    ],[
        AC_MSG_RESULT([no])
        LDFLAGS="$_LDFLAGS"
    ])
    CFLAGS="$_CFLAGS"

    if test "$mtlsdebug" != "all"; then
        _LDFLAGS="$LDFLAGS"
        _CFLAGS="$CFLAGS"
        LDFLAGS="-Wl,-dead_strip"
        CFLAGS=
        AC_MSG_CHECKING([whether LD supports -Wl,-dead_strip])
        AC_LINK_IFELSE([AC_LANG_PROGRAM([])],[
            AC_MSG_RESULT([yes])
            LDFLAGS="$_LDFLAGS -Wl,-dead_strip"
        ],[
            AC_MSG_RESULT([no])
            LDFLAGS="$_LDFLAGS"
        ])
        CFLAGS="$_CFLAGS"

        _LDFLAGS="$LDFLAGS"
        _CFLAGS="$CFLAGS"
        LDFLAGS="-s"
        CFLAGS=
        AC_MSG_CHECKING([whether LD supports -s])
        AC_LINK_IFELSE([AC_LANG_PROGRAM([])],[
            AC_MSG_RESULT([yes])
            LDFLAGS="$_LDFLAGS -s"
        ],[
            AC_MSG_RESULT([no])
            LDFLAGS="$_LDFLAGS"
        ])
        CFLAGS="$_CFLAGS"
    fi

])

AC_DEFUN([TCLMTLS_CHECK_SANITIZE], [
    AC_ARG_ENABLE(sanitize,
        AS_HELP_STRING([--enable-sanitize],
            [enable sanitizers (default: no)]
        ), [
            mtlssan="$enableval"
        ], [
            mtlssan="no"
        ]
    )

    AC_MSG_CHECKING([for enabled sanitizers])
    if test "$mtlssan" = "yes"; then

        AC_MSG_RESULT([yes])

        TEA_ADD_CFLAGS([-DPURIFY])

        _CFLAGS="$CFLAGS"
        CFLAGS="-fsanitize=address"
        AC_MSG_CHECKING([whether cc supports $CFLAGS])
        AC_LINK_IFELSE([AC_LANG_PROGRAM([])],[
            AC_MSG_RESULT([yes])
            LDFLAGS="$LDFLAGS $CFLAGS"
            CFLAGS="$_CFLAGS $CFLAGS"
        ],[
            AC_MSG_RESULT([no])
            CFLAGS="$_CFLAGS"
        ])

        _CFLAGS="$CFLAGS"
        CFLAGS="-fsanitize=memory -fPIE -pie"
        AC_MSG_CHECKING([whether cc supports $CFLAGS])
        AC_LINK_IFELSE([AC_LANG_PROGRAM([])],[
            AC_MSG_RESULT([yes])
            LDFLAGS="$LDFLAGS $CFLAGS"
            CFLAGS="$_CFLAGS $CFLAGS"
        ],[
            AC_MSG_RESULT([no])
            CFLAGS="$_CFLAGS"
        ])

        _CFLAGS="$CFLAGS"
        CFLAGS="-fsanitize=undefined"
        AC_MSG_CHECKING([whether cc supports $CFLAGS])
        AC_LINK_IFELSE([AC_LANG_PROGRAM([])],[
            AC_MSG_RESULT([yes])
            LDFLAGS="$LDFLAGS $CFLAGS"
            CFLAGS="$_CFLAGS $CFLAGS"
        ],[
            AC_MSG_RESULT([no])
            CFLAGS="$_CFLAGS"
        ])

    else
        AC_MSG_RESULT([no])
    fi
])

AC_DEFUN([TCLMTLS_CHECK_CLIENT_SERVER], [
    AC_ARG_ENABLE(client,
        AS_HELP_STRING([--enable-client],
            [enable SSL client (default: yes)]
        ), [
            mtlsclient="$enableval"
        ], [
            mtlsclient="yes"
        ]
    )

    AC_MSG_CHECKING([for enabled SSL client])
    if test "$mtlsclient" = "yes"; then
        TCLMTLS_MBEDTLS_DEFINE(MTLS_ENABLE_CLIENT)
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi

    AC_ARG_ENABLE(server,
        AS_HELP_STRING([--enable-server],
            [enable SSL server (default: yes)]
        ), [
            mtlsserver="$enableval"
        ], [
            mtlsserver="yes"
        ]
    )

    AC_MSG_CHECKING([for enabled SSL server])
    if test "$mtlsserver" = "yes"; then
        TCLMTLS_MBEDTLS_DEFINE(MTLS_ENABLE_SERVER)
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi

    if test "$mtlsserver" = "no" && test "$mtlsclient" = "no"; then
        AC_MSG_ERROR([ERROR: either an SSL server or SSL client must be enabled])
    fi
])

