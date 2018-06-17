TARGET=parse-prototype

all:
	ghc -dynamic ${TARGET}.hs

.Phony: run
run:
	./${TARGET} hscode.hs rcode.R

.Phony: clean
clean:
	rm -f *.o *.hi ${TARGET}
