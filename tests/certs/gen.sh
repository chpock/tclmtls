#!/bin/sh

# Copyright (c) 2024 Konstantin Kushnir <chpock@gmail.com>
#
# See the file "license.terms" for information on usage and redistribution
# of this file, and for a DISCLAIMER OF ALL WARRANTIES.

rm -f ca-*.pem server-*.pem client-*.pem

set -x
openssl genrsa -out ca-key.pem 4096
openssl req -new -text -x509 -nodes -days 365000 -key ca-key.pem -out ca-cert.pem \
    -subj "/C=CA/ST=Wonderland/L=Alice Home/OU=Test CA/O=mtls package"

openssl req -text -newkey rsa:4096 -nodes -keyout server-key.pem -out server-req.pem \
    -subj "/C=CA/ST=Wonderland/L=Alice Home/OU=Test Server/O=mtls package/CN=127.0.0.1"

echo 'subjectAltName=DNS:localhost,IP:127.0.0.1' > server.ext
openssl x509 -req -text -days 365000 -set_serial 01 -in server-req.pem -out server-cert.pem \
    -CA ca-cert.pem -CAkey ca-key.pem \
    -extfile server.ext
rm -f server.ext

openssl req -text -newkey rsa:4096 -nodes -keyout client-key.pem -out client-req.pem \
    -subj "/C=CA/ST=Wonderland/L=Alice Home/OU=Test Client/O=mtls package/CN=127.0.0.1"
openssl x509 -req -text -days 365000 -set_serial 01 -in client-req.pem -out client-cert.pem -CA ca-cert.pem -CAkey ca-key.pem
