#!/bin/sh
cargo build
rust-gdb -ex 'set env LD_LIBRARY_PATH=target/debug' target/debug/lccc
