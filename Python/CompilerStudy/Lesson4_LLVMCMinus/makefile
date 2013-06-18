.PHONY: all build clean

all: build

build: CMinus.tokens

CMinus.tokens: CMinus.g
	java org.antlr.Tool CMinus.g

clean:
	rm -f *pyc CMinus.tokens CMinusLexer.py CMinusParser.py
