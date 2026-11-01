REFUSED = morloc make -o bad $(1).loc 2> $(1).err || true; \
	if grep -q "$(2)" $(1).err && grep -q "$(3)" $(1).err; then echo "OK: $(1) refused" >> obs.txt; else echo "UNEXPECTED: $(1)" >> obs.txt; fi

RUN = ./nexus $(1) >> obs.txt 2> made.err; grep -o "made in [a-z]*" made.err >> obs.txt

HINT = import only the module that sources 'mkAdder' in the language you need

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	$(call RUN,madeInPy 1)
	$(call RUN,madeInCpp 1)
	$(call RUN,atRoot 1)
	$(call RUN,passedToCpp 1)
	$(call RUN,labeled 1)
	$(call RUN,fiveInPy 1)
	$(call RUN,chosenAtCall true 1)
	$(call RUN,chosenAtCall false 1)
	if morloc typecheck -v --realize main.loc > check.err 2> /dev/null && ! grep -q 'cost check' check.err; then echo "OK: built placement costs the least" >> obs.txt; else echo "UNEXPECTED: built placement does not cost the least found" >> obs.txt; fi
	$(call REFUSED,bad-cpp,cannot leave the pool that made it,$(HINT))
	$(call REFUSED,bad-py,cannot leave the pool that made it,$(HINT))
	$(call REFUSED,bad-both,cannot leave the pool that made it,$(HINT))
	$(call REFUSED,bad-two,pass through the other's pool,$(HINT))
	morloc make -o explicit explicit.loc 2> explicit.err
	./explicit useInCpp 1 >> obs.txt 2> made.err
	grep -o "made in [a-z]*" made.err >> obs.txt

clean:
	rm -rf nexus bad explicit pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
