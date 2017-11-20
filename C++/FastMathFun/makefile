main: main.cpp gen.py *.s makefile
	python gen.py gen_gemm48 > gemm48_gen.s
	python gen.py gen_gemm96 > gemm96_gen.s
	python gen.py gen_transform_cols_L1 > transform_cols_L1_gen.s
	python gen.py gen_transform_cols_L2 > transform_cols_L2_gen.s
	python gen.py gen_transform_cols_L3 > transform_cols_L3_gen.s
	python gen.py gen_transform_cols_Memory > transform_cols_Memory_gen.s
	g++ -O3 -mfma -std=c++14 -c main.cpp -o main.o
	g++ -c *.s 
	g++ *.o -o main

clean:
	rm *.o main
	rm *_gen.s
