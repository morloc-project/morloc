all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	for c in copyPy copyCpp conditionR ; do \
	  echo "--- $$c ---" >> obs.txt ; \
	  ./nexus $$c --n >> obs.txt 2> $$c.err ; echo "exit $$?" >> obs.txt ; \
	  grep -o "a step of this fold failed\|a put was still stepping" $$c.err >> obs.txt ; \
	  cat $$c.err >> obs.err ; \
	done ; true

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
