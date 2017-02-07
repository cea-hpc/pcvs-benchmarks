#!/bin/sh

echo "STARTED BY WRAPPER $@"
"$@" || exit 1
