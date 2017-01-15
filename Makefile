TARGET=loc

all:
	cd frontend && ${MAKE}

# personal kludge
.PHONY: asdf
asdf:
	${MAKE} install
	loc -o zzz current.loc


.PHONY: clean
clean:
	rm -f ${TARGET}
	cd frontend && ${MAKE} clean
	cd backends && ${MAKE} clean

.PHONY: install
install:
	mkdir -p ~/.loc/bin
	mkdir -p ~/.loc/etc
	mkdir -p ~/.loc/lib
	cp frontend/loc ~/.loc/bin
	cp backends/parse-grammar.awk ~/.loc/bin
	cp backends/build-nexus.awk ~/.loc/bin
	cp -rf backends/core ~/.loc/lib
	cp -rf backends/etc ~/.loc
	test -d ~/bin || mkdir ~/bin
	cp backends/build.sh ~/bin/loc
