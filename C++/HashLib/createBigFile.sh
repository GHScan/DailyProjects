#! /bin/bash

dd if=/dev/zero of=bigFile bs=1M count=$1
