#! /bin/bash

N=1000000
K=500000

select sortType in *Sort
do
    ./numsGen $N $K > 1.txt
    cat 1.txt | ./$sortType $N > 2.txt
    sort -n 1.txt > 3.txt
    (diff 2.txt 3.txt 1>diff.txt) || echo 'sort failed!'
    rm 1.txt 2.txt 3.txt
    break
done
