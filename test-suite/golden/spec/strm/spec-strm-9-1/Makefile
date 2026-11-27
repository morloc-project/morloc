all:
	rm -f obs.txt *.err
	morloc make -o nexus main.loc 2> build.err
	for c in inThread inChild; do \
	  if ./nexus $$c > /dev/null 2> $$c.err; then echo "$$c: succeeded" >> obs.txt; \
	  elif grep -q "raised" $$c.err; then echo "$$c: failed, naming the raised put" >> obs.txt; \
	  else echo "$$c: failed for another reason" >> obs.txt; fi; \
	  cat $$c.err >> obs.err; \
	done

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
