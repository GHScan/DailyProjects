main: main.cpp *.s makefile
	g++ -O3 -mfma -std=c++11 -c main.cpp -o main.o
	g++ -c *.s 
	g++ *.o -o main

clean:
	rm *.o main
