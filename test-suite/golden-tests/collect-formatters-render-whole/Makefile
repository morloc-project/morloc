# Whole-list `render:` -- TDD-red until the raw return path is wired.
#
# `morloc make` currently fails (the synthesis rejects whole-list render with a
# dfail), so nexus-cpp is never produced and the invocation below yields no
# output -- obs.txt stays empty and differs from exp.txt (the intended
# size-headered raw block). When whole-list render is wired, the build
# succeeds, the command emits the verbatim block, and this test turns green.

all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus-cpp main-cpp.loc 2> build.err || true
	printf -- '=== render (whole) verbatim ===\n' >> obs.txt
	if [ -x ./nexus-cpp ] ; then \
		./nexus-cpp streamRW --whole >> obs.txt 2>> obs.err ; \
	else \
		echo "(build failed: whole-list render not wired -- see build.err)" >> obs.txt ; \
	fi

clean:
	rm -rf nexus-cpp pools __pycache__ *.err obs.txt
