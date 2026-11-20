define run
	@echo "--- $(1) ---" >> obs.txt
	@if ./nexus $(1) >> obs.txt 2> case.err ; then echo UNEXPECTED >> obs.txt ; else grep -v "^  at \|^Error: run failed" case.err >> obs.txt ; fi
	@cat case.err >> obs.err
	@rm -f case.err
endef

all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	$(call run,bareOnErr 0)
	$(call run,errOnOk 3)
	$(call run,litOnOk 4)
	$(call run,nestedOnOk 2)
	./nexus nestedOnOk 1 >> obs.txt 2>> obs.err

clean:
	rm -f *.err nexus
	rm -rf *-build *build.tmp* __pycache__
