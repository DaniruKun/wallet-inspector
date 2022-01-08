.PHONY: all clean

all: bin

bin: wallet-inspector.lisp build-binary.sh Makefile
	mkdir -p bin
	./build-binary.sh wallet-inspector.lisp
	mv wallet-inspector bin/

install:
	cp ./bin/wallet-inspector /usr/local/bin/

clean:
	rm -rf 'bin'
