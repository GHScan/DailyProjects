#! /bin/bash

# generate all.c
if [ ! -f all.c ]; then
    cat *.h *.cpp > _all.c
    (for ((i=0;i<10;++i)); do cat _all.c; done) > all.c
    rm _all.c
fi

# generate main10
if [ ! -f main10 ]; then
    (for((i=0;i<30;++i)); do cat main; done) > main10
fi

# generate bigFile
if [ ! -f bigFile ]; then
    ./createBigFile.sh 64
fi

algos="rle rle_2 rle_4"
testFiles="all.c pch.h.gch main10 bigFile"

for f in $testFiles; do
    echo $f ":"
    for algo in $algos; do
        result=`(bash -c "time ./main -a $algo -i $f -o ${f}_x" 2>&1; bash -c "time ./main -x -a $algo -i ${f}_x -o ${f}_x2" 2>&1; du -b $f; du -b ${f}_x)  | python benchmark_help.py`
        if diff $f ${f}_x2 > /dev/null ;then
            printf "\t%20s : %s %s %s\n" $algo $result
        else
            echo "failed: $algo"
        fi
        rm -r ${f}_x ${f}_x2
    done
done

#rm -f all.c main10 bigFile
