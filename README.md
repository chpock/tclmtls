
# <a name='Introduction'></a>Introduction

This Tcl package provides an extension which implements Transport
Layer Security (TLS) over Transmission Control Protocol (TCP)
network communication channels.

## <a name='::Introduction-Why_'></a>Why?

Usual Tcl package for SSL/TLS communication channels is
[tcltls](https://core.tcl-lang.org/tcltls). But this package has
a number of disadvantages:

- it uses OpenSSL/LibreSSL libraries. These libraries are huge (see [issue#11308](https://github.com/openssl/openssl/issues/11308)), contain a lot of obsolete and practically useless code/functions
- no support for CA certificates provided by the operating system
- unsafe default parameters


There are also alternatives, but they have their disadvantages:

- Package [twapi](https://www.magicsplat.com/tcl-docs/twapi/tls.html): Windows platform only; comes as part of the entire twapi package (which is very cool, but also critically huge)
- Package [TclCurl](https://github.com/flightaware/tclcurl-fa): depends on the even larger cURL library and therefore has both greater build complexity and size; support for client connections only


[::mtls](README\.html\#::mtls) solves these disadvantages. It has the following features:

- uses the [mbedTLS](https://github.com/Mbed-TLS/mbedtls) library with minimal size
- interface is compatible with tcltls, most of the existing code will work as is, without modifications
- uses CA certificates from the operating system on Linux/Windows/MacOS platforms
- uses only modern TLS1.2/TLS1.3 protocols, which are more than sufficient for successful connections to most services
- certificate and hostname verification, SNI are enabled by default
- multi-platform, Linux/Windows/MacOS supported
- possibility to exclude the client or server part to minimize the size even more
- easy to build, no 3rd-party libraries, everything you need to build is in this repository
- was created as a base for the use of SSL/TLS alternative backends


## <a name='::Introduction-Compatibility'></a>Compatibility

This package requires Tcl 8.6 or later. However it has been tested
with Tcl 8.6.14 and 9.0b3.

## <a name='::Introduction-How_to_build'></a>How to build

This package uses the Tcl Extension Architecture (TEA) to build and
install on Linux, Mac, or Windows platforms. For Windows platform only
building with Mingw-w64 toolchain is supported.

The standard TEA config, make and install process is supported.

```
$ git clone https://github.com/chpock/tclmtls.git
$ cd tclmtls
$ git submodule update --init --recursive
$ mkdir build && cd build
$ ../configure
$ make
$ make test
$ make install
```

The supported configure options include all of the standard TEA
configure script options, plus:

|||
|----|----|
|`--disable-client`|disable SSL client part|
|`--disable-server`|disable SSL server part|


By default, mbedTLS will be built and used. It is possible to use
an already built library, for this it is possible to specify
the following options:

|||
|----|----|
|`--with-mbedtls=<dir>`|path to root directory of Mbed-TLS installation|
|`--with-mbedtls-include=<dir>`|path to include directory of Mbed-TLS installation|
|`--with-mbedtls-lib=<dir>`|path to lib directory of Mbed-TLS installation|


Options that can be used for development:

|||
|----|----|
|`--enable-debug=<level>`|enable debug messages. Possible levels are: `none`, `error`, `warning`, `info` or `on`|


## <a name='::Introduction-Usage'></a>Usage

Since this package intends to be compatible with the original
[tcltls](https://core.tcl-lang.org/tcltls) package, the usual
**tcltls** examples should be correct. You can see these examples on
the Tcl wiki: [https://wiki.tcl-lang.org/page/tls](https://wiki.tcl-lang.org/page/tls)

Minimal examples are available below under [HTTPS examples](README\.html\#::mtls\-HTTPS\_examples).

## <a name='::Introduction-Built_packages_and_sources'></a>Built packages and sources

The source code is available on [Github](https://github.com/chpock/tclmtls).

Built packages are available on the above Github page, under
[Releases](https://github.com/chpock/tclmtls/releases).

There are packages for the following platforms:

- **Windows x86** and **x86\_64**: Windows XP or higher is required. However, they are only tested on Windows 10.
- **Linux x86** and **x86\_64**: built and tested on Cenos6.10. Require glibc v2.12 or higher.
- **MacOS x86** and **x86\_64**: built and tested on MacOS 10.12. However, these packages should be compatible with MacOS as of version 10.6.


RPM spec file for openSUSE is available on
[https://build.opensuse.org/package/show/home:jkandz/tclmtls](https://build.opensuse.org/package/show/home:jkandz/tclmtls).
Binary packages for openSUSE for various platforms are available on
[openSUSE build service](https://software.opensuse.org//download.html?project=home%3Ajkandz&package=tclmtls)

## <a name='::Introduction-Copyrights'></a>Copyrights

Copyright (C) 2024 Konstantin Kushnir <chpock@gmail.com>

## <a name='::Introduction-License'></a>License

This code is licensed under the same terms as the Tcl Core.

This package contains [mbedTLS](https://github.com/Mbed-TLS/mbedtls)
sources which are distributed under a dual [Apache-2.0](https://spdx.org/licenses/Apache-2.0.html)
OR [GPL-2.0-or-later](https://spdx.org/licenses/GPL-2.0-or-later.html)
license.

# <a name='::mtls'></a>::mtls

## <a name='::mtls-Description'></a>Description

This Tcl package provides an extension which implements Transport
Layer Security (TLS) over Transmission Control Protocol (TCP)
network communication channels.

Typically one would use the [socket](README\.html\#::mtls::socket) command which provides
compatibility with the native Tcl [socket](https://www.tcl.tk/man/tcl8.6/TclCmd/socket.htm)
command. In such cases [import](README\.html\#::mtls::import) should not be used directly.

Please note, to ensure seamless use of both the classic tcltls package
and this package, command aliases are created in the `::tls` namespace
when loading mtls. This allows the same code base to be used when using
different packages to support SSL/TLS connections. However, aliases
will not be created if the `::tls` namespace already exists at the time
the mtls package is loaded. This in turn allows both the tcltls and
mtls packages to be loaded at the same time. But the tcltls package
must be loaded first.

## <a name='::mtls-HTTPS_examples'></a>HTTPS examples

### <a name='::mtls-client_example'></a>client example

```
package require http
package require mtls

http::register https 443 ::mtls::socket

set tok [http::geturl https://www.tcl.tk/]

```

### <a name='::mtls-server_example'></a>server example

```
package require mtls

proc readable { sock } {
    # do something
}

proc accept { sock addr port } {
    fileevent $sock readable [list readable $sock]
}

mtls::socket -certfile $serverCert -keyfile $serverKey -server accept 8080

```

## <a name='::mtls-Commands'></a>Commands

#### <a name='::mtls::ciphers'></a>ciphers [[::mtls](README\.html\#::mtls)]

Gets a list of supported ciphers.


> `ciphers` *`protocol`*<br>

##### Parameters

|||
|----|----|
|`protocol`|protocol for which a list of ciphers will be returned. Must be one of `tls1.2` or `tls1.3`.|


##### Description

For compatibility with **tcltls**, this procedure can also accept
`verbose` and `supported` arguments. However, they do not affect
the result and will be ignored.

##### Return value

 A Tcl list of supported ciphers based on the specified
protocol.

#### <a name='::mtls::debug'></a>debug [[::mtls](README\.html\#::mtls)]

Sets the level for debug messages


> `debug` *`?integer?`*<br>
> `debug` *`?level? ?backend_level?`*<br>

##### Parameters

|||
|----|----|
|`backend_level`|sets the desired level for TLS backend debug messages. The format is the same as for the `level` argument.|
|`integer`|integer in this form will represent the debugging level for the packet and TLS backend messages. The first 3 bits refer to the packet layer and the next 3 bits refer to the TLS backend layer. The minimum level is `0`, which means no messages, and the maximum level is `5`, which corresponds to the `trace` level.|
|`level`|sets the desired level for package debug messages, which must be one of `none`, `error`, `warning`, `info`, `debug`, `trace` or a number from `0` to `5`.|


##### Description

If no arguments are specified, this command simply returns the current
level.

Debug messages will only work if the package was built with debugging
support. Otherwise, this level will be ignored.

##### Return value

 An unsigned integer corresponding to the current level
for both package and TLS backend debug messages.

#### <a name='::mtls::handshake'></a>handshake [[::mtls](README\.html\#::mtls)]

Forces handshake to take place


> `handshake` *`channel`*<br>

##### Parameters

|||
|----|----|
|`channel`|TLS channel on which the handshake process should be started.|


##### Return value

 `0` if handshake is still in progress (non-blocking), or `1` if
the handshake was successful. If the handshake failed this procedure
will throw an error.

#### <a name='::mtls::import'></a>import [[::mtls](README\.html\#::mtls)]

Adds TLS layer for existing socket channel.


> `import` *`channel ?options?`*<br>

##### Parameters

|||
|----|----|
|`channel`|A regular Tcl channel created wich Tcl [socket](https://www.tcl.tk/man/tcl8.6/TclCmd/socket.htm) command.|
|`-cadir dir`|Provide the directory containing the CA certificates.|
|`-cafile filename`|Provide the CA file.|
|`-cert data`|Provide the contents of a certificate to use, as a PEM encoded or a binary DER encoded value (X.509 DER).|
|`-certfile filename`|Provide the name of a file containing certificate to use.|
|`-cipher list`|Provide the list of cipher suites to use.|
|`-dhparams filename`|Provide a Diffie-Hellman parameters file.|
|`-key data`|Provide the private key to use as a PEM encoded or a binary DER encoded value (X.509 DER).|
|`-keyfile filename`|Provide the private key file. (default: value of `-certfile`)|
|`-model channel`|This will force this channel to share the same parameters as the specified channel.|
|`-password callback`|If supplied, this callback will be invoked to unlock the private key of a certificate. The callback should return a string which represents the password to be used.|
|`-require bool`|Require a valid certificate from peer during TLS handshake. (default: `true` for client connections and `false` for server connections)|
|`-server bool`|Handshake as server if true, else handshake as client. (default: `false`)|
|`-servername host`|Use to name the logical host we are talking to and expecting a certificate for.|
|`-tls1.2 bool`|Enable use of TLS v1.2 protocol (default: `true`)|
|`-tls1.3 bool`|Enable use of TLS v1.3 protocol (default: `true`)|


##### Description

TLS-enables a regular Tcl channel and sets session parameters for
TLS handshake.

For compatibility with **tcltls**, this procedure can also accept
`-command callback`, `-request bool`, `-ssl2 bool`, `-ssl3 bool`,
`-tls1 bool` and `-tls1.1 bool` arguments. However, they will be ignored.

##### Return value

 The name of a TLS-enabled Tcl channel that can be used to send
and receive data.

#### <a name='::mtls::init'></a>init [[::mtls](README\.html\#::mtls)]

Sets the default settings for newly created TLS channels.


> `init` *`?options?`*<br>

##### Parameters

|||
|----|----|
|`?options?`|options supported by the [import](README\.html\#::mtls::import) and [socket](README\.html\#::mtls::socket) commands.|


##### Description

Sets the default settings for newly created TLS channels. The `-model`,
`-server`, `-servername` options will be accepted but silently ignored.

##### Return value

 The dict value with the currently defined default options.

#### <a name='::mtls::protocols'></a>protocols [[::mtls](README\.html\#::mtls)]

Get a list of supported TLS protocols.


> `protocols` *``*<br>

##### Return value

 A Tcl list of supported TLS protocols.

#### <a name='::mtls::socket'></a>socket [[::mtls](README\.html\#::mtls)]

Create a TLS socket channel.


> `socket` *`?options? host port`*<br>
> `socket` *`?-server command? ?options? port`*<br>

##### Parameters

|||
|----|----|
|`-autoservername bool`|Automatically set the `-servername` as the host argument (default: `true`)|
|`options`|options accepted by the normal Tcl command [socket](https://www.tcl.tk/man/tcl8.6/TclCmd/socket.htm) and the [import](README\.html\#::mtls::import) command.|


##### Description

This is a helper function for creating TLS-enabled channels. It behaves
exactly the same as the native Tcl [socket](https://www.tcl.tk/man/tcl8.6/TclCmd/socket.htm)
command, and accepts all of its options. In addition, it takes all
parameters from the [import](README\.html\#::mtls::import) command to configure the newly created
channel.

It also accepts the option `-autoservername`. If set to `true`,
the value of the `-servername` option will be automatically detected
from the specified connection parameters.

##### Return value

 The name of a TLS-enabled Tcl channel that can be used to send
and receive data.

#### <a name='::mtls::status'></a>status [[::mtls](README\.html\#::mtls)]

Gets the current security status of an TLS channel.


> `status` *`channel`*<br>

##### Parameters

|||
|----|----|
|`channel`|TLS channel from which to obtain status|


##### Description

The returned value will be a Tcl dictionary with the following keys:

|||
|----|----|
|version|The protocol version used for the connection: `TLSv1.1`, `TLSv1.2`, `unknown`.|
|cipher|The current cipher in use between the client and server.|
|sbits|The number of bits used for the session key.|


For compatibility with **tcltls**, this procedure can also accept
`-local` optional argument. However, it will be ignored.

##### Return value

 The Tcl dict value with the current security status of
the TLS channel.

#### <a name='::mtls::unimport'></a>unimport [[::mtls](README\.html\#::mtls)]

Unstacks the TLS-enabling of a regular Tcl channel.


> `unimport` *`channel`*<br>

##### Parameters

|||
|----|----|
|`channel`|TLS channel from which TLS layer is to be removed.|


##### Description

Provided for symmetry to [import](README\.html\#::mtls::import) command. This unstacks
the TLS-enabling of a regular Tcl channel.

##### Return value

 nothing.

#### <a name='::mtls::version'></a>version [[::mtls](README\.html\#::mtls)]

Get the type and version of the TLS backend in use.


> `version` *``*<br>

##### Return value

 A string with the type and version number of
the TLS backend used.

