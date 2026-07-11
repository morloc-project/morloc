all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	printf 'a\t5\nb\t25\nc\t30\n' > entries.tsv
	printf 'key\tvalue\na\t5\nb\t25\n' > headed.tsv
	./nexus entries.tsv                                 >> obs.txt 2>> obs.err
	./nexus '[["a",5],["b",25],["c",30]]'               >> obs.txt 2>> obs.err
	if ./nexus headed.tsv 2>&1 | grep -qF 'looks like a column-name header'; then echo "OK: header hint" >> obs.txt; else echo "FAIL: header hint missing" >> obs.txt; fi
	if ./nexus tally --log-dir X entries.tsv 2>&1 | grep -qF 'nexus option, not a command option'; then echo "OK: nexus-flag hint" >> obs.txt; else echo "FAIL: nexus-flag hint missing" >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ *.err entries.tsv headed.tsv
