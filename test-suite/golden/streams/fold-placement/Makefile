TMP := $(CURDIR)/gather-tmp

all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	rm -rf $(TMP) && mkdir -p $(TMP)
	MORLOC_TMPDIR=$(TMP) ./nexus cmd --n >> obs.txt 2>> obs.err
	rm -rf $(TMP) && mkdir -p $(TMP)
	MORLOC_TMPDIR=$(TMP) ./nexus cmdPy --n >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err $(TMP)
	rm -rf *-build *build.tmp*
