main: main.cpp gen.py *.s makefile
	python gen.py gen_gemm48 > gemm48_gen.s
	python gen.py gen_gemm96 > gemm96_gen.s
	g++ -O3 -mfma -std=c++14 -c main.cpp -o main.o
	g++ -c *.s 
	g++ *.o -o main

clean:
	rm *.o main
	rm *_gen.s
