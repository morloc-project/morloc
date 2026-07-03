# Verify MORLOC_SKIP_NULL_CHECK=1 bypasses the morloc NUL guard so R
# itself raises the error. The function reads its Str argument from a
# file (`source: file`); the file holds a literal NUL byte.

all:
	rm -f *.err obs.txt nul.txt
	morloc make -o nexus main.loc 2> build.err
	printf 'abc\000def' > nul.txt
	MORLOC_SKIP_NULL_CHECK=1 ./nexus rEcho nul.txt > /dev/null 2> obs.err && echo 'RC=0' > obs.txt || echo 'RC<>0' > obs.txt
	# morloc's guard message must be absent (skip flag worked).
	if grep -F 'does not support embedded NUL bytes' obs.err > /dev/null ; then \
	  echo 'GUARD STILL FIRED' >> obs.txt ; \
	else \
	  echo 'GUARD SKIPPED' >> obs.txt ; \
	fi
	# R must still surface its own NUL error.
	if grep -i 'nul' obs.err > /dev/null ; then \
	  echo 'NUL ERROR FROM R' >> obs.txt ; \
	else \
	  echo 'NO NUL ERROR' >> obs.txt ; \
	fi

clean:
	rm -rf nexus pools __pycache__ nul.txt
