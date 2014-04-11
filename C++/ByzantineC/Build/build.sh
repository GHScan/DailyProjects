#! /bin/bash

select buildType in Debug Release
do
    mkdir -p $buildType
    cd $buildType
    cmake -DCMAKE_BUILD_TYPE=$buildType ../..
    cd ..
    break
done
