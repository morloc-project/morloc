TARGET=morloc
CC=gcc
XXFLAGS=-Wall -Wextra -Wno-unused-parameter -g -std=gnu99
# XXFLAGS=-Wall -Wextra -Wno-unused-parameter \
#         -Wno-implicit-function-declaration -Wno-int-conversion \
#         -Wformat-security \
#         -g -std=c99
LINKERFLAGS=-lfl

SRC_BISON=${TARGET}.y
SRC_LEXER=${TARGET}.l

LEXER=lex.yy
PARSER=${TARGET}.tab

OBJECTS =               \
	label.o             \
	couplet.o           \
	manifold.o          \
	section.o           \
	w.o                 \
	ws.o                \
	lhs.o               \
	hof.o               \
	ws_access.o         \
	type_util.o         \
	type_Generic.o      \
	type_GenericList.o  \
	type_HomoSet.o      \
	type_HomoSetList.o  \
	type_ManifoldList.o \
	type.o              \
	build_io.o          \
	build_path.o        \
	build_mod.o         \
	build.o             \
	lil.o               \
	${LEXER}.o          \
	bufstack.o

${TARGET}: ${OBJECTS} 
	${CC} -o ${TARGET} ${OBJECTS} ${PARSER}.o ${LINKERFLAGS}

%.o: %.c %.h
	${CC} -c ${XXFLAGS} $< -o $@

${LEXER}.o: ${SRC_LEXER} ${SRC_BISON}
	flex ${SRC_LEXER}
	bison -d --report=all -Wall ${SRC_BISON}
	${CC} -c ${XXFLAGS} ${LEXER}.c ${PARSER}.c

.PHONY: clean
clean:
	rm -f ${TARGET} ${PARSER}.[hc] ${LEXER}.[hc]
	rm -rf ${OBJECTS} ${PARSER}.o
	rm -f tok.log morloc.output
	rm -f vgcore*
