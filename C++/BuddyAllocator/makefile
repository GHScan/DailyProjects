.Phony : all clean

all : main

main : test.c buddy.c 
	gcc -g -Wall -o $@ $^ -std=c99

clean : 
	rm -f main main.exe
