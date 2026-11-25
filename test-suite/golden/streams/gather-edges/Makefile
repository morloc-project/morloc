TMP := $(CURDIR)/gather-tmp

all:
	: > obs.txt
	rm -f *.err handler-ran
	rm -rf $(TMP)
	mkdir -p $(TMP)
	morloc make -o nexus main.loc 2> build.err
	for L in Cpp Py R Rust ; do for f in show count slice ; do echo "--- empty$$L --$$f ---" >> obs.txt ; ./nexus empty$$L --$$f >> obs.txt 2>> obs.err ; done ; done ; true
	for L in Cpp Py R Rust ; do for f in show count slice other ; do echo "--- ten$$L --$$f ---" >> obs.txt ; ./nexus ten$$L --$$f >> obs.txt 2>> obs.err ; done ; done ; true
	for L in Cpp Py R Rust ; do for f in mark count ; do echo "--- fail$$L --$$f ---" >> obs.txt ; if MORLOC_TMPDIR=$(TMP) ./nexus fail$$L --$$f >> obs.txt 2> fail.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi ; grep -c "producer failed after 2 batches" fail.err >> obs.txt ; cat fail.err >> obs.err ; done ; done ; true
	for L in Cpp Py ; do echo "--- late$$L --show ---" >> obs.txt ; if MORLOC_TMPDIR=$(TMP) ./nexus late$$L --show >> obs.txt 2>> obs.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi ; done
	echo "--- tenArgCpp 5 --plus ---" >> obs.txt ; ./nexus tenArgCpp 5 --plus >> obs.txt 2>> obs.err ; true
	if test -e handler-ran ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi
	find $(TMP) -name "morloc-gather-*" | wc -l | tr -d " " >> obs.txt

clean:
	rm -rf nexus pools __pycache__ $(TMP) *.err handler-ran
	rm -rf *-build *build.tmp*
