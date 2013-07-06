#! /bin/bash

set -e

srcfile=$1
asmfile=$srcfile.asm
objfile=$srcfile.o
exefile=$srcfile.exe

./main $*
nasm -f elf32 $asmfile -o $objfile
gcc $objfile -o $exefile
./$exefile
rm $objfile $exefile
rm $asmfile
