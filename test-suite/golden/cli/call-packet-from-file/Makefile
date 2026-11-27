all:
	rm -f *.err *.out *.pkt *.dat obs.txt
	morloc make -o nexus main.loc 2> build.err
	MORLOC_SKIP_UTF8_CHECK=1 ./nexus -f packet bad 1 > bad.pkt 2> bad.err
	./nexus -f packet good 1 > good.pkt 2> good.err
	for c in good bad; do \
	  python3 call.py 3 $$c.pkt $$c.dat; \
	  morloc-nexus run --call-packet $$c.dat --socket-base pipe-cpp --output-file $$c.out --output-form packet nexus-build/manifest.json 2> $$c-call.err && echo "$$c OK" >> obs.txt || echo "$$c FAILED" >> obs.txt; \
	  grep -o "not valid UTF-8 (byte 1 of 3) in data read from outside" $$c-call.err | head -n 1 >> obs.txt; \
	done

clean:
	rm -rf nexus pools __pycache__ *.err *.out *.pkt *.dat
	rm -rf *-build *build.tmp*
