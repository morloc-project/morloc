all:
	rm -f *.err obs.txt tabs.csv commas.txt bom.tab one.csv mixed.csv utf16.csv
	morloc make -o nexus main.loc 2> build.err
	printf 'x\ty\n1\ta,b\n' > tabs.csv
	printf 'x,y\n1,a\tb\n' > commas.txt
	printf '\357\273\277"x","y"\r\n1,a\r\n' > bom.tab
	printf 'only\nv,w\n' > one.csv
	printf 'a,b\tc\n1,2\t3\n' > mixed.csv
	printf '\377\376x\000' > utf16.csv
	./nexus rows tabs.csv >> obs.txt 2>> obs.err
	./nexus rows commas.txt >> obs.txt 2>> obs.err
	./nexus rows bom.tab >> obs.txt 2>> obs.err
	./nexus table tabs.csv >> obs.txt 2>> obs.err
	./nexus open one.csv >> obs.txt 2>> obs.err
	./nexus open mixed.csv >> obs.txt 2>> obs.err
	if ./nexus rows utf16.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ tabs.csv commas.txt bom.tab one.csv mixed.csv utf16.csv
	rm -f *.err
	rm -rf *-build *build.tmp*
