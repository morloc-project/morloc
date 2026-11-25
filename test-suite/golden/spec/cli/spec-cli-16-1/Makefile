all:
	rm -f *.err obs.txt ok.csv nullstr.csv quotedint.csv
	morloc make -o nexus main.loc 2> build.err
	printf 'n,s,t\n,,""\n1,"",a\n' > ok.csv
	printf 'n,s,t\n1,a,\n' > nullstr.csv
	printf 'n,s,t\n"",a,b\n' > quotedint.csv
	./nexus rows ok.csv >> obs.txt 2>> obs.err
	if ./nexus rows nullstr.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows quotedint.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ ok.csv nullstr.csv quotedint.csv
	rm -f *.err
	rm -rf *-build *build.tmp*
