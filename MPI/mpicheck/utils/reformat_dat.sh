#!/bin/bash

for f in "$@"; do sort -u -f -o $f $f; done
