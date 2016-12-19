TARGET=rat

all:
	flex ${TARGET}.l
	bison -Wall --report=all -d ${TARGET}.y
	gcc lex.yy.c ril.c types.c bufstack.c ${TARGET}.tab.c -lfl -o ${TARGET}

.PHONY: clean
clean:
	rm -f lex.yy.* ${TARGET}.tab.* ${TARGET} lexer
	rm -f rat.output tok.log
