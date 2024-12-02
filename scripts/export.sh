#!/bin/bash

### export _build dir to ebin dir
BUILD_LIBS=_build/default/lib
EBIN_DIR=_build/ebin
CUR_DIR=$(cd $(dirname $0); pwd)

# compile
cd $CUR_DIR/../ && rebar3 get-deps && rebar3 compile


if [ ! -d $EBIN_DIR ]; then
  mkdir -p $EBIN_DIR
fi

for i in $(ls $BUILD_LIBS);
do
    echo "copying beam: $BUILD_LIBS/$i/ebin"
    cp -rf $BUILD_LIBS/$i/ebin/* $EBIN_DIR
done