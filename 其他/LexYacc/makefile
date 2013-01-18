.PHONY: all build rebuild clean
all: build
build: main
main: syntax.y lex.l
	yacc -d syntax.y -b syntax
	flex lex.l 
	g++ syntax.tab.c -o main
rebuild: clean
	$(MAKE)
clean:
	rm -f $(wildcard *.c) $(wildcard *.h) ./main ./main.exe
