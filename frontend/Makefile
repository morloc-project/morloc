TARGET=rat

all:
	flex ${TARGET}.l
	bison -Wall --report=all -d ${TARGET}.y
	cc lex.yy.c bufstack.c ${TARGET}.tab.c -lfl -o ${TARGET}

.PHONY: clean
clean:
	rm -f lex.yy.* ${TARGET}.tab.* ${TARGET} lexer
	rm -f rat.output tok.log
