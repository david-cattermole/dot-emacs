#!/bin/bash
#

gcc -g -o test_attach_c test_attach_c.c

rustc -g -O test_attach_rust.rs -o test_attach_rust
