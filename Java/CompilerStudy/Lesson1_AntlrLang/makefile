.PHONY: all build clean

all: build

build: JSMinusParser.java
	javac -g *java template/*java

rel_build: JSMinusParser.java
	javac *java template/*java

JSMinusParser.java: JSMinus.g
	java org.antlr.Tool JSMinus.g

clean:
	rm -rf JSMinus.tokens JSMinusLexer.java JSMinusParser.java
	rm -rf *class template/*class
