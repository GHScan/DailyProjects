
.PHONY: all build clean

java_files=$(wildcard *.java)
class_files=$(java_files:.java=.class)

all: build
build: $(class_files)
clean:
	rm -f $(class_files)

$(class_files):%.class:%.java
	javac $^
