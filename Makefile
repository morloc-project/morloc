all:
	cd frontend && ${MAKE}

.PHONY: install
install:
	mkdir -p ~/.loc/bin
	mkdir -p ~/.loc/lib
	cp frontend/loc ~/.loc/bin/loc
	cp -rf backend/core ~/.loc/lib
	test -d ~/bin || mkdir ~/bin
	ln -sf ${PWD}/backend/src/loc/loc.py ${HOME}/bin/loc

.PHONY: test
test:
	cd tests && ./runtests.sh -K

.PHONY: clean
clean:
	cd frontend && ${MAKE} clean
