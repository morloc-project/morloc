TARGET=loc
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
	${MAKE} && cp loc ~/bin/loc_frontend
	${MAKE} r && cp R ~/bin/loc_R_backend
	cp loc.sh ~/bin/loc
