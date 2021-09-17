#!/bin/sh
set -e
if [ -z "$NIX_PRIMER_VERSION" ]; then
    git describe --match="" --always --abbrev=40 --dirty
else
    echo "$NIX_PRIMER_VERSION"
fi
