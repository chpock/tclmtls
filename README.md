# tclmtls

This Tcl package provides an extension which implements Transport Layer Security (TLS) over Transmission Control Protocol (TCP) network communication channels.

## Description

Usual Tcl package for SSL/TLS communication channels is [tcltls](https://core.tcl-lang.org/tcltls). But this package has a number of disadvantages:

* it uses OpenSSL/LibreSSL libraries. These libraries are huge (see [issue#11308](https://github.com/openssl/openssl/issues/11308)), contain a lot of obsolete and practically useless code/functions
* no support for CA certificates provided by the operating system
* unsafe default parameters

There are also alternatives, but they have their disadvantages:

* Package [twapi](https://www.magicsplat.com/tcl-docs/twapi/tls.html): Windows platform only; comes as part of the entire twapi package (which is very cool, but also critically huge)
* Package [TclCurl](https://github.com/flightaware/tclcurl-fa): depends on the even larger cURL library and therefore has both greater build complexity and size; support for client connections only

**tclmtls** solves these disadvantages. It has the following features:

* uses the [mbedTLS](https://github.com/Mbed-TLS/mbedtls) library with minimal size
* interface is compatible with tcltls, most of the existing code will work as is, without modifications
* uses CA certificates from the operating system on Linux/Windows/MacOS platforms
* uses only modern TLS1.2/TLS1.3 protocols, which are more than sufficient for successful connections to most services
* certificate and hostname verification, SNI are enabled by default
* multi-platform, Linux/Windows/MacOS supported
* possibility to exclude the client or server part to minimize the size even more
* easy to build, no 3rd-party libraries, everything you need to build is in this repository
* was created as a base for the use of SSL alternative backends

## Compatibility

This package requires TCL 8.6 or later.

## Installation

This package uses the Tcl Extension Architecture (TEA) to build and install on Linux, Mac, or Windows platforms. For Windows platform only building with Mingw-w64 toolchain is supported.

The standard TEA config, make and install process is supported.

```shell
$ cd tclmtls
$ ./configure --enable-64bit
$ make
$ make test
$ make install
```

The supported configure options include all of the standard TEA configure script options, plus:

| Option  | Description |
| ------------ | ------------ |
| `--disable-client` | disable SSL client part |
| `--disable-server` | disable SSL server part |

By default, mbedTLS will be built and used. It is possible to use an already built library, for this it is possible to specify the following options:

| Option  | Description |
| ------------ | ------------ |
| `--with-mbedtls=<dir>` | path to root directory of Mbed-TLS installation |
| `--with-mbedtls-include=<dir>` | path to include directory of Mbed-TLS installation |
| `--with-mbedtls-lib=<dir>` | path to lib directory of Mbed-TLS installation |

Options that can be used for development:

| Option  | Description |
| ------------ | ------------ |
| `--enable-debug=<level>` | enable debug messages. Possible levels are: none, error, warning, info, on |
| `--enable-sanitize` | enable sanitizers |

## Usage

**tclmtls** supports all documented [tcltls](https://core.tcl-lang.org/tcltls/wiki/Documentation) commands except callbacks. See the instructions for using this package:

* [https://core.tcl-lang.org/tcltls/wiki?name=Documentation](https://core.tcl-lang.org/tcltls/wiki?name=Documentation)
* [https://wiki.tcl-lang.org/page/tls](https://wiki.tcl-lang.org/page/tls)

## Copyrights

Copyright (C) 2024 Konstantin Kushnir <chpock@gmail.com>

## License

This code is licensed under the same terms as the Tcl Core.

This package contains [mbedTLS](https://github.com/Mbed-TLS/mbedtls) sources which are distributed under a dual [Apache-2.0](https://spdx.org/licenses/Apache-2.0.html) OR [GPL-2.0-or-later](https://spdx.org/licenses/GPL-2.0-or-later.html) license.
