#!/usr/bin/env bash

for dir in *
do
    cd $dir && make clean && cd ..
done 2> /dev/null > /dev/null
