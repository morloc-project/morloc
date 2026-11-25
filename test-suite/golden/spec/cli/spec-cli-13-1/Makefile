all:
	rm -f *.err obs.txt t.csv t.json t.arrow t.parquet r.pkt t.fifo
	morloc make -o nexus main.loc 2> build.err
	printf 'y,x\na,1\nb,2\n' > t.csv
	printf '[{"x":1,"y":"a"}]' > t.json
	./nexus -f arrow table t.csv > t.arrow 2>> obs.err
	./nexus -f parquet table t.csv > t.parquet 2>> obs.err
	./nexus -f packet rows t.csv > r.pkt 2>> obs.err
	./nexus rows t.csv >> obs.txt 2>> obs.err
	./nexus rows t.arrow >> obs.txt 2>> obs.err
	./nexus rows t.parquet >> obs.txt 2>> obs.err
	./nexus rows r.pkt >> obs.txt 2>> obs.err
	./nexus rows - < t.csv >> obs.txt 2>> obs.err
	./nexus table t.csv >> obs.txt 2>> obs.err
	./nexus table t.parquet >> obs.txt 2>> obs.err
	./nexus table - < t.csv >> obs.txt 2>> obs.err
	./nexus open t.csv >> obs.txt 2>> obs.err
	rm -f t.fifo && mkfifo t.fifo && { cat t.csv > t.fifo & ./nexus table t.fifo >> obs.txt 2>> obs.err; }
	if ./nexus rows '[{"x":1,"y":"a"}]' > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows t.json > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus table t.json > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus open t.json > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ t.csv t.json t.arrow t.parquet r.pkt t.fifo
	rm -f *.err
	rm -rf *-build *build.tmp*
