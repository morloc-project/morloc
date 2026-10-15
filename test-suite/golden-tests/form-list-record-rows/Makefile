all:
	rm -f *.err *.csv *.tsv *.txt.in
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	printf 'a,b,1,2,0.5,true\nc,d,3,4,1e3,false\n' > rows.csv
	printf 'a\tb\t1\t2\t0.5\ttrue\n' > rows.tsv
	printf 'a b 1 2 0.5 true\n' > rows.txt.in
	printf 'a,b,1,2,0.5,true\r\n\r\nc,d,3,4,1,false\r\n' > crlf.csv
	printf '"x, y","say ""hi""",1,2,0.5,true\n,e,5,6,2,true\n' > quoted.csv
	printf '{"name":"a","tag":"b","x":1,"y":2,"w":0.5,"ok":true}\n' > objects.txt.in
	: > empty.csv
	printf 'a,1\nb,null\n' > opt.csv
	printf 'a,b,1,x,0.5,true\n' > badint.csv
	printf 'a,b,1,2,0.5,true,9\n' > wide.csv
	printf 'a,b,1,2,0.5,true\nc,d,3,4,1\n' > narrow.csv
	printf 'name,tag,x,y,w,ok\na,b,1,2,0.5,true\n' > header.csv
	./nexus rows rows.csv >> obs.txt 2>> obs.err
	./nexus rows rows.tsv >> obs.txt 2>> obs.err
	./nexus rows rows.txt.in >> obs.txt 2>> obs.err
	./nexus rows crlf.csv >> obs.txt 2>> obs.err
	./nexus rows quoted.csv >> obs.txt 2>> obs.err
	./nexus rows objects.txt.in >> obs.txt 2>> obs.err
	./nexus rows '[{"name":"a","tag":"b","x":1,"y":2,"w":0.5,"ok":true}]' >> obs.txt 2>> obs.err
	./nexus rows empty.csv >> obs.txt 2>> obs.err
	./nexus opts opt.csv >> obs.txt 2>> obs.err
	if ./nexus rows badint.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows wide.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows narrow.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows header.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ *.err *.csv *.tsv *.txt.in
	rm -rf *-build *build.tmp*
