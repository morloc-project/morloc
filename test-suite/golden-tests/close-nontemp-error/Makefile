all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	printf -- '--- @close on non-temp path: clean rejection ---\n' >> obs.txt
	./nexus bad > /dev/null 2> close.err ; true
	if grep -qi 'registered temp file' close.err ; then \
		echo 'OK: clean "not a registered temp file" error' >> obs.txt ; \
	else \
		printf 'WRONG message: %s\n' "$$(head -1 close.err)" >> obs.txt ; \
	fi
	cat close.err > obs.err

clean:
	rm -rf nexus pools *.err
