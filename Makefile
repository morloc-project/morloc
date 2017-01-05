TARGET=loc

all:
	cd frontend && ${MAKE} && cp ${TARGET} .. && ${MAKE} clean

.PHONY: clean
clean:
	rm -f ${TARGET}
	cd frontend && ${MAKE} clean
