TARGET=rat
RTARGET=R

all:
	cd frontend && ${MAKE} && mv ${TARGET} .. && ${MAKE} clean

.PHONY: clean
clean:
	rm -f ${TARGET} ${RTARGET}
	cd frontend && ${MAKE} clean

.PHONY: r
r:
	cd backends/R && ${MAKE} && mv ${RTARGET} ../.. && ${MAKE} clean

.PHONY: install
install:
	mkdir -p ~/bin
	${MAKE} && cp rat ~/bin/rat_frontend
	${MAKE} r && cp R ~/bin/rat_R_backend
	cp rat.sh ~/bin/rat
