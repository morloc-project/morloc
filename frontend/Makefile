TARGET=loc

FILES=bufstack.c label.c list.c composition.c manifold.c lil.c

all:
	flex ${TARGET}.l
	bison -Wall --report=all -d ${TARGET}.y
	gcc lex.yy.c ${FILES} ${TARGET}.tab.c -lfl -o ${TARGET}

.PHONY: clean
clean:
	rm -f lex.yy.* ${TARGET}.tab.* ${TARGET} lexer
	rm -f loc.output tok.log
