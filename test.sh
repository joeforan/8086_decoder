#!/usr/bin/env bash

function cargo(){
    docker run --rm -e USER --user "$(id -u)":"$(id -g)" -v "$PWD":/usr/src/myapp -w /usr/src/myapp rust:1.26.0 cargo $1
}

pushd ./app > /dev/null
cargo test
popd > /dev/null
