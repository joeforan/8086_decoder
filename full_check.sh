#!/usr/bin/env bash
#
set -euo pipefail

./build.sh

TEST_FILE=${1}
TMP_FILE=$(mktemp --tmpdir=/tmp)

export RUST_BACKTRACE=1
echo "Disassembling"
./app/target/release/myapp ${TEST_FILE} > ${TMP_FILE}
echo "Back Assembling"
nasm ${TMP_FILE} -o ${TMP_FILE}.bin
echo "Checking diff"
diff ${TEST_FILE} ${TMP_FILE}.bin
