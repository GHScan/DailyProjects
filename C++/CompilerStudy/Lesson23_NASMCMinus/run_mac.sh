
srcfile=$1

./main $*

nasm -f macho  $srcfile.asm -o $srcfile.o 
gcc -m32 $srcfile.o -o $srcfile.exe 
cp $srcfile.asm __.asm

./$srcfile.exe

rm $srcfile.*
