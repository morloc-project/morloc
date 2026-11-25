all:
	rm -f *.err obs.txt in.csv out.csv out.tsv tout.tsv
	morloc make -o nexus main.loc 2> build.err
	printf 's,n,r,b\n"a,b",1,0.5,true\n"",,nan,false\n"say ""hi""\tx",-3,1e+300,TRUE\n"two\nlines",4,-inf,False\n' > in.csv
	./nexus -f csv rows in.csv >> obs.txt 2>> obs.err
	./nexus -f tsv rows in.csv >> obs.txt 2>> obs.err
	./nexus -f csv table in.csv >> obs.txt 2>> obs.err
	./nexus -f tsv table in.csv >> obs.txt 2>> obs.err
	./nexus -f csv rows in.csv > out.csv 2>> obs.err
	./nexus -f tsv rows in.csv > out.tsv 2>> obs.err
	./nexus -f tsv table in.csv > tout.tsv 2>> obs.err
	./nexus rows in.csv >> obs.txt 2>> obs.err
	./nexus rows out.csv >> obs.txt 2>> obs.err
	./nexus rows out.tsv >> obs.txt 2>> obs.err
	./nexus table in.csv >> obs.txt 2>> obs.err
	./nexus table tout.tsv >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ in.csv out.csv out.tsv tout.tsv
	rm -f *.err
	rm -rf *-build *build.tmp*
