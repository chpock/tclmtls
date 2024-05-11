#!/bin/sh

# Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

rm -f selfsigned-*.pem

set -x

openssl genrsa -out selfsigned-key.pem 2048
openssl req -new -key selfsigned-key.pem -out selfsigned-cert.csr \
    -subj "/C=CA/ST=Wonderland/L=Alice Home/OU=Test Server (Self-signed)/O=mtls package/CN=127.0.0.1"
openssl x509 -req -days 365000 -in selfsigned-cert.csr \
    -signkey selfsigned-key.pem -out selfsigned-cert.pem
rm -f selfsigned-cert.csr
