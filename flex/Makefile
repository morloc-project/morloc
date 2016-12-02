TARGET=ratparser

all:
	flex rats.l
	cc lex.yy.c -lfl -o ${TARGET}

.PHONY: clean
clean:
	rm -f lex.yy.c ${TARGET}
