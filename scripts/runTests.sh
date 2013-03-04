#! /bin/bash

pushd tests

ml-build nullable.cm SimpleTests.doTests nullable

if [ $? -eq 0 ]; then
    echo "*** build successful, running tests ***"
    sml @SMLload=nullable.x86-darwin
    rm nullable.x86-darwin
fi

popd
