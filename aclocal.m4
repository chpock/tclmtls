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
            if test "${CFLAGS_DEFAULT}" = "${CFLAGS_DEBUG}"; then
                mtlsdebug="auto"
            else
                mtlsdebug="none"
            fi
        ]
    )

    AC_MSG_CHECKING([for debugging level])
    if test "$mtlsdebug" = "none"; then
        AC_MSG_RESULT([$mtlsdebug])
    elif test "$mtlsdebug" = "all" || test "$mtlsdebug" = "auto"; then
        AC_MSG_RESULT([$mtlsdebug])
        mtlsdebug="all"
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

        # Clear CFLAGS because it contains variable names
        _CFLAGS="$CFLAGS"
        CFLAGS=

        AC_MSG_CHECKING(for AES instruction set)
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
            #include <wmmintrin.h>
        ]],[])],
            [AC_MSG_RESULT(yes)], [
                AC_MSG_RESULT([no, adding -maes])
                MBEDTLS_CFLAGS="$MBEDTLS_CFLAGS -maes -mpclmul"
            ]
        )

        if test "${TEA_PLATFORM}" = "unix" ; then
            AC_MSG_CHECKING(for required -lrt)
            AC_LINK_IFELSE([AC_LANG_PROGRAM([[
                #include <time.h>
            ]],[[
                struct timespec t;
                clock_gettime(0, &t);
            ]])],
                [AC_MSG_RESULT(no)], [
                    AC_MSG_RESULT([yes])
                    TEA_ADD_LIBS([-lrt])
                ]
            )
        fi

        # Restore CFLAGS
        CFLAGS="$_CFLAGS"

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

        # Test this only on MacOS as GNU ld interprets -dead_strip
        # as '-de'+'ad_strip'
        if test "$SHLIB_SUFFIX" = ".dylib"; then
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
        fi

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

AC_DEFUN([TCLMTLS_PROG_TCLSH], [
    AC_MSG_CHECKING([for correct tclsh])

    if ! test -f "${TCLSH_PROG}" && ! command -v "${TCLSH_PROG}" >/dev/null 2>&1; then
        if test "${TEA_PLATFORM}" = "windows"; then
            base_name="tclsh${TCL_MAJOR_VERSION}${TCL_MINOR_VERSION}"
            name_list="${base_name}${EXEEXT} \
                ${base_name}s${EXEEXT} \
                ${base_name}t${EXEEXT} \
                ${base_name}st${EXEEXT} \
                ${base_name}g${EXEEXT} \
                ${base_name}gs${EXEEXT} \
                ${base_name}gt${EXEEXT} \
                ${base_name}gst${EXEEXT}"
            for i in $name_list; do
                if test -f "${TCL_BIN_DIR}/../bin/$i"; then
                    TCLSH_PROG="`cd "${TCL_BIN_DIR}/../bin"; pwd`/$i"
                    break
                fi
            done
        fi
        if test -f "${TCLSH_PROG}"; then
            AC_MSG_RESULT([${TCLSH_PROG}])
            AC_SUBST(TCLSH_PROG)
        else
            AC_MSG_RESULT([fail])
            AC_MSG_ERROR([ERROR: could not find tclsh binary])
        fi
    else
        AC_MSG_RESULT([ok])
    fi
])
