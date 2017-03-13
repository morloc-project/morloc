#!/usr/bin/env bash

echo
echo '=============='
echo 'Testing atomic'
echo '=============='
echo

cd atomic
make
cd ..


echo
echo '=============='
echo 'Testing array'
echo '=============='
echo

cd array
make
cd ..

echo
echo '=============='
echo 'Testing tuples'
echo '=============='
echo

cd tuple
make
cd ..
