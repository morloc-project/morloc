all:
	cd frontend && ${MAKE}

.PHONY: install
install:
	mkdir -p ~/.morloc/bin
	mkdir -p ~/.morloc/lib
	cp frontend/morloc ~/.morloc/bin/morloc
	cp -rf backend/core ~/.morloc/lib
	test -d ~/bin || mkdir ~/bin
	ln -sf ${PWD}/backend/src/morloc/morloc.py ${HOME}/bin/morloc

.PHONY: test
test:
	cd tests && ./runtests.sh -K

.PHONY: clean
clean:
	cd frontend && ${MAKE} clean
