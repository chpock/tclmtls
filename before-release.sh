#!/bin/sh

SELF_DIR="$(dirname "$0")"
VERSION="$(cat "$SELF_DIR/version")"

rm -f "$SELF_DIR/manifest.uuid"

tclsh "$SELF_DIR/tools/ruff.tcl" "::Introduction ::mtls" -preeval "source $SELF_DIR/doc/doc.ruff" \
    -outdir "$SELF_DIR/doc" -outfile "mtls.html" -product "mtls" -version "$VERSION" \
    -sortnamespaces false -hidenamespace ::Introduction -navigation sticky

tclsh "$SELF_DIR/tools/ruff.tcl" "::mtls" -preeval "source $SELF_DIR/doc/doc.ruff" \
    -outdir "$SELF_DIR/doc" -outfile "mtls.n" -format nroff -product "mtls" -version "$VERSION"

tclsh "$SELF_DIR/tools/ruff.tcl" "::Introduction ::mtls" -preeval "source $SELF_DIR/doc/doc.ruff" \
    -outdir "$SELF_DIR" -outfile "README.md" -format markdown -product "mtls" -version "$VERSION"
