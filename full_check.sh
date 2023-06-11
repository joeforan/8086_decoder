#!/usr/bin/env bash
#
set -euo pipefail

./build.sh

BASEDIR=$(realpath $(dirname $0))
if [ -z ${1+x} ]; then
    echo "Usage: ./full_check.sh file"
fi
TEST_FILE=${1}
TMP_DIR=$(mktemp -d --tmpdir=${BASEDIR}/tmp)
TMP_FILE=${TMP_DIR}/test.asm


export RUST_BACKTRACE=1
echo "Disassembling"
./app/target/release/myapp ${TEST_FILE} > ${TMP_FILE}
echo "Back Assembling"
nasm ${TMP_FILE} -o ${TMP_DIR}/test.bin
echo "Checking diff"
diff ${TEST_FILE} ${TMP_DIR}/test.bin
