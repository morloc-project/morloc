TARGET=rat

all:
	flex ${TARGET}.l
	bison -d ${TARGET}.y
	cc lex.yy.c ${TARGET}.tab.c -lfl -o ${TARGET}

.PHONY: clean
clean:
	rm -f lex.yy.* ${TARGET}.tab.* ${TARGET} a.out
