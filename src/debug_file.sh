#!/bin/bash

file=$1

export GRAM_DEBUG=d
make
export GRAM_DEBUG=
./parse-test -p ${file}
