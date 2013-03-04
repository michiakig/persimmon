#! /bin/bash

function smlnj {
    pushd tests
    ml-build nullable.cm SimpleTests.doTests nullable
    if [ $? -eq 0 ]; then
        echo "*** build successful, running tests ***"
        sml @SMLload=nullable.x86-darwin
        rm nullable.x86-darwin
    fi
    popd
}

function mlton_build {
    mlton persimmon.mlb
    if [ $? -eq 0 ]; then
        echo "*** build successful, running tests ***"
        ./persimmon
        rm persimmon
    fi
}

if [ $1 = smlnj ]; then
    smlnj
elif [ $1 = mlton ]; then
    mlton_build
else
    smlnj
    mlton_build
fi
