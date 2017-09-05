#!/bin/bash

file=$1

if [ $file = "debug" ]; then
	echo "hehe"
	export GRAM_DEBUG=d
else
	echo "huhu"
	export GRAM_DEBUG=""
fi

echo $GRAM_DEBUG
make grammar
stack build
