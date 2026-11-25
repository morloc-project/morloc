# The pad lengths shift where the later columns land inside the block.
PADS = 1 5 9 13
DECIMAL = viaPy viaCpp viaR viaRust
VIEW = viewViaPy viewViaCpp viewViaRust

all:
	rm -f *.err obs.txt
	morloc make -X rust:lto=thin -o nexus main.loc 2> build.err
	for c in $(DECIMAL) $(VIEW); do \
	  for p in $(PADS); do \
	    echo "$$c pad=$$p $$(./nexus $$c $$p 2>> obs.err)" >> obs.txt; \
	  done; \
	done

clean:
	rm -rf nexus pools *.err __pycache__ .RData
	rm -rf *-build *build.tmp*
