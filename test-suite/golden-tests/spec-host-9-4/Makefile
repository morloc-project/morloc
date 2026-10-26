REFUSED = morloc make -o bad $(1).loc 2> $(1).err || true; \
	if grep -q "$(2)" $(1).err; then echo "OK: $(1) refused" >> obs.txt; else echo "UNEXPECTED: $(1)" >> obs.txt; fi

RUN = ./nexus $(1) >> obs.txt 2> made.err; grep -o "made in [a-z]*" made.err >> obs.txt

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	$(call RUN,chosenHome true 1)
	$(call RUN,chosenHome false 1)
	$(call RUN,doHome 1)
	$(call RUN,suspendedHome 1)
	$(call RUN,aliasHome 1)
	./nexus relayedAway 1 >> obs.txt 2>> obs.err
	if morloc typecheck -v --realize main.loc > check.err 2> /dev/null && ! grep -q 'cost check' check.err; then echo "OK: built placement costs the least" >> obs.txt; else echo "UNEXPECTED: built placement does not cost the least found" >> obs.txt; fi
	$(call REFUSED,bad-chosen,cannot leave the pool that made it)
	$(call REFUSED,bad-do,cannot leave the pool that made it)
	$(call REFUSED,bad-suspended,cannot leave the pool that made it)
	$(call REFUSED,bad-alias,cannot leave the pool that made it)
	$(call REFUSED,bad-data,cannot leave the pool that made it)
	$(call REFUSED,bad-branches,depending on which branch runs)
	$(call REFUSED,bad-suspended-two,pass through the other's pool)
	$(call REFUSED,bad-unrelated,cannot leave the pool that made it)
	$(call REFUSED,bad-relayed,cannot leave the pool that made it)
	if grep -q "has sources in" bad-unrelated.err; then echo "UNEXPECTED: an unrelated term offered as a source" >> obs.txt; else echo "OK: no unrelated source offered" >> obs.txt; fi

clean:
	rm -rf nexus bad pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
