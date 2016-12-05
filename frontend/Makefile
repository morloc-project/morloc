TARGET=rats

all:
	flex rats.l
	bison -d rats.y
	cc lex.yy.c rats.tab.c -lfl -o ${TARGET}

.PHONY: clean
clean:
	rm -f lex.yy.* rats.tab.* ${TARGET}
