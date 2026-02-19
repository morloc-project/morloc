#!/usr/bin/env bash

./concurrency-tests/run-tests.sh

./stress/run-all.sh

./typecheck-benchmark/run-benchmarks.sh

./daemon-tests/run-tests.sh

echo "libmorloc tests not defined yet"
# cmorloc-tests:

echo "error-message tests not defined yet"
# error-message-tests:

echo "executable benchmarks not yet up" 
# executable-benchmark/distributed:
# executable-benchmark/parallel-interop:
# executable-benchmark/serial-interop:
