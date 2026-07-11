all:
	rm -f *.err obs.txt
	morloc make -o single single.loc 2>  build.err
	morloc make -o multi  multi.loc  2>> build.err
	printf 'a\t5\nb\t25\nc\t30\n' > entries.tsv
	./single --minimum 20 entries.tsv      >> obs.txt 2>> obs.err
	./single @ --minimum 20 entries.tsv    >> obs.txt 2>> obs.err
	./single echoMap --minimum 20 entries.tsv >> obs.txt 2>> obs.err
	./single -y --minimum 20 entries.tsv   >> obs.txt 2>> obs.err
	./single -y --minimum 20 entries.tsv                     >> obs.txt 2>> obs.err
	./single --minimum 20 -y entries.tsv                     >> obs.txt 2>> obs.err
	./single --minimum 20 entries.tsv                        >> obs.txt 2>> obs.err
	./single -h 2> /dev/null | grep -qF -- '--minimum' && echo OK >> obs.txt || echo MISSING >> obs.txt
	./single --help 2> /dev/null | grep -qF -- '--minimum' && echo OK >> obs.txt || echo MISSING >> obs.txt
	./single -h @ 2> /dev/null | grep -qF -- '--log-dir' && echo OK >> obs.txt || echo MISSING >> obs.txt
	./single @ -h 2> /dev/null | grep -qF -- '--minimum' && echo OK >> obs.txt || echo MISSING >> obs.txt
	./multi fhead 7                                          >> obs.txt 2>> obs.err
	./multi describe 42                                      >> obs.txt 2>> obs.err
	./multi @ fhead 7                                        >> obs.txt 2>> obs.err
	if ./multi fhead --log-dir X 7 >> obs.txt 2>> obs.err; then \
		echo "UNEXPECTED: nexus flag after subcmd accepted" >> obs.txt; \
	else \
		echo "OK: nexus flag right of subcmd rejected" >> obs.txt; \
	fi
	if ./multi fheed 7 >> obs.txt 2>> obs.err; then \
		echo "UNEXPECTED: typo accepted" >> obs.txt; \
	else \
		echo "OK: typo rejected" >> obs.txt; \
	fi

clean:
	rm -rf single multi pools __pycache__ *.err entries.tsv
