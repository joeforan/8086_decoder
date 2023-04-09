#!/usr/bin/env bash
#
set -euo pipefail

./check.sh
./test.sh
./build.sh


TEST_FILE=${1}
TMP_FILE=$(mktemp --tmpdir=/tmp)

export RUST_BACKTRACE=1
./app/target/release/myapp ${TEST_FILE} > ${TMP_FILE}
nasm ${TMP_FILE} -o ${TMP_FILE}.bin
diff ${TEST_FILE} ${TMP_FILE}.bin
