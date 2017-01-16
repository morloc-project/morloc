TARGET=loc
CC=gcc
XXFLAGS=-Wall -g

SRC_BISON=${TARGET}.y
SRC_LEXER=${TARGET}.l



C_LEXER=lex.yy.c
H_LEXER=lex.yy.h
C_PARSER=${TARGET}.tab.c
H_PARSER=${TARGET}.tab.h

FILES =          \
	${C_LEXER}   \
	${C_PARSER}  \
	bufstack.c   \
	lhs.c        \
	ws.c         \
	w.c          \
	hof.c        \
	label.c      \
	couplet.c    \
	manifold.c   \
	build.c      \
	build_path.c \
	build_io.c   \
	build_mod.c  \
	type.c       \
	ws_access.c  \
	lil.c

all:
	bison -d --report=all -Wall ${SRC_BISON}
	flex ${SRC_LEXER}
	${CC} -Wall ${XXFLAGS} -lfl -o ${TARGET} ${FILES}

clean:
	rm -f ${TARGET} ${C_PARSER} ${H_PARSER} ${C_LEXER} ${H_LEXER}
	rm -f tok.log loc.output
	rm -f vgcore*
