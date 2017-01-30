TARGET=loc

all:
	cd frontend && ${MAKE}

.PHONY: install
install:
	mkdir -p ~/.loc/bin
	mkdir -p ~/.loc/etc
	mkdir -p ~/.loc/lib
	cp frontend/loc ~/.loc/bin
	cp -rf backends/core ~/.loc/lib
	cp -rf backends/etc ~/.loc
	test -d ~/bin || mkdir ~/bin
	cp frontend/loc ~/bin/loc

.PHONY: test
test:
	cd tests && ./runtests.sh -K

.PHONY: clean
clean:
	rm -f ${TARGET}
	cd frontend && ${MAKE} clean
	cd backends && ${MAKE} clean
