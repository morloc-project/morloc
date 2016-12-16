TARGET=rat

all:
	flex ${TARGET}.l
	bison -Wall --report=all -d ${TARGET}.y
	cc lex.yy.c ${TARGET}.tab.c -lfl -o ${TARGET}

.PHONY: clean
clean:
	rm -f lex.yy.* ${TARGET}.tab.* ${TARGET} lexer

.PHONY: flex
flex:
	flex ${TARGET}.l
	cc -lfl lex.yy.c -o lexer
