GUARD = MORLOC_FORBID_SELF_CALL=1

all:
	rm -f *.err obs.txt
	for L in cpp py r rust; do \
	  morloc make -o nexus-$$L $$L.loc 2>> build.err; \
	  echo "$$L $$($(GUARD) ./nexus-$$L goF 3 100 2>> obs.err || echo failed)" >> obs.txt; \
	done
	morloc make -o nexus-crossing crossing.loc 2>> build.err
	echo "crossing $$($(GUARD) ./nexus-crossing crossing 3 10 2>> obs.err || echo failed)" >> obs.txt

clean:
	rm -rf nexus-* pools *.err __pycache__
	rm -rf *-build *build.tmp*
