all:
	rm -f *.err obs.txt
	for l in py r cpp rs; do morloc make -o nexus-$$l main-$$l.loc 2>> build.err; done
	for l in py r cpp rs; do \
	  ./nexus-$$l andDiv 0 >> obs.txt 2>> obs.err || echo "$$l andDiv failed" >> obs.txt; \
	  ./nexus-$$l andDiv 2 >> obs.txt 2>> obs.err || echo "$$l andDiv failed" >> obs.txt; \
	  ./nexus-$$l orDiv 0 >> obs.txt 2>> obs.err || echo "$$l orDiv failed" >> obs.txt; \
	  ./nexus-$$l andSrc 0 >> obs.txt 2>> obs.err || echo "$$l andSrc failed" >> obs.txt; \
	  ./nexus-$$l orSrc 0 >> obs.txt 2>> obs.err || echo "$$l orSrc failed" >> obs.txt; \
	  ./nexus-$$l prefix 0 >> obs.txt 2>> obs.err || echo "$$l prefix failed" >> obs.txt; \
	  ./nexus-$$l folded '[true,true,false]' >> obs.txt 2>> obs.err || echo "$$l folded failed" >> obs.txt; \
	  ./nexus-$$l section true >> obs.txt 2>> obs.err || echo "$$l section failed" >> obs.txt; \
	done

clean:
	rm -rf nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
