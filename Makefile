TARGET=loc

all:
	cd frontend && ${MAKE} && cp ${TARGET} .. && ${MAKE} clean

.PHONY: clean
clean:
	rm -f ${TARGET}
	cd frontend && ${MAKE} clean
	cd backends && ${MAKE} clean

.PHONY: install
install:
	test -d ~/.loc || mkdir ~/.loc
	mkdir -p ~/.loc/bin
	mkdir -p ~/.loc/lib
	mkdir -p ~/.loc/etc
	cp loc ~/.loc/bin
	cp backends/parse.awk ~/.loc/bin
	cp -rf backends/lib ~/.loc
	cp -rf backends/etc ~/.loc
	test -d ~/bin || mkdir ~/bin
	cp backends/build.sh ~/bin/loc
