all:
	rm -f *.err obs.txt
	for L in cpp py r rust; do \
	  morloc make -o nexus-$$L $$L.loc 2>> build.err; \
	  echo "$$L nested $$(./nexus-$$L nested 2 30 2>> obs.err)" >> obs.txt; \
	  ./nexus-$$L effects $$L.mlc 20000 2>> obs.err; \
	  echo "$$L effects $$(./nexus-$$L count $$L.mlc 2>> obs.err)" >> obs.txt; \
	done

clean:
	rm -rf nexus-* pools *.err *.mlc __pycache__
	rm -rf *-build *build.tmp*
