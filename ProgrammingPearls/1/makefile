#gccflags=-std=c++11 -g
gccflags=-std=c++11 -O3 -DNDEBUG

all: numsGen libSort bitVecSort bitVecPassSort libPassSort mergeSort qSort shellSort

numsGen: 4_numsGen.cpp
	g++ $(gccflags) 4_numsGen.cpp -o numsGen

mergeSort: mergeSort.cpp
	g++ $(gccflags) mergeSort.cpp -o mergeSort

qSort: qSort.cpp
	g++ $(gccflags) qSort.cpp -o qSort

libSort: 1_libSort.cpp
	g++ $(gccflags) 1_libSort.cpp -o libSort

libPassSort: libPassSort.cpp
	g++ $(gccflags) libPassSort.cpp -o libPassSort

bitVecSort: 3_bitVecSort.cpp
	g++ $(gccflags) 3_bitVecSort.cpp -o bitVecSort

bitVecPassSort: 5_bitVecPassSort.cpp
	g++ $(gccflags) 5_bitVecPassSort.cpp -o bitVecPassSort

shellSort: shellSort.cpp
	g++ $(gccflags) shellSort.cpp -o shellSort

clean:
	rm *Sort numsGen
