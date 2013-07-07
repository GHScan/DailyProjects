#! /bin/bash

set -e

srcfile=$1

./main $*
nasm -f elf32 $srcfile.asm -o $srcfile.o
gcc $srcfile.o -o $srcfile.exe

./$srcfile.exe

rm $srcfile.*
