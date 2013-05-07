.PHONY: all build clean

gfile=$(wildcard *.g)
lex_file=$(gfile:.g=Lexer.java)
parse_file=$(gfile:.g=Parser.java)
token_file=$(gfile:.g=.tokens)
java_files=$(wildcard *.java)
class_files=$(java_files:.java=.class)

all: build
build: build_gfile build_java_files
clean:
	rm -f $(lex_file) $(parse_file) $(token_file) $(class_files)

build_gfile: $(lex_file)
$(lex_file): $(gfile)
	java org.antlr.Tool $(gfile)

build_java_files: $(class_files)
$(class_files):%.class:%.java
	javac $^

