all:
	cd src && ${MAKE} && mv rats .. && ${MAKE} clean

.PHONY: clean
clean:
	rm -f rats
	cd src && ${MAKE} clean
