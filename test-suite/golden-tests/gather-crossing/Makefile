TMP := $(CURDIR)/gather-tmp

all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus cmd --show >> obs.txt 2>> obs.err
	./nexus cmd2 --show >> obs.txt 2>> obs.err
	rm -rf $(TMP) && mkdir -p $(TMP)
	MORLOC_TMPDIR=$(TMP) ./nexus cmd3 --show 2 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err $(TMP)
	rm -rf *-build *build.tmp*
