all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus onlyStmt 5 > obs.txt 2> obs.err
	./nexus onlyStmt 20 >> obs.txt 2>> obs.err
	./nexus afterBind 5 >> obs.txt 2>> obs.err
	./nexus afterBind 15 >> obs.txt 2>> obs.err
	./nexus afterLet 5 >> obs.txt 2>> obs.err
	./nexus afterLet 15 >> obs.txt 2>> obs.err
	./nexus afterBare 3 >> obs.txt 2>> obs.err
	./nexus afterBare 6 >> obs.txt 2>> obs.err
	./nexus multiClause -- -5 >> obs.txt 2>> obs.err
	./nexus multiClause 5 >> obs.txt 2>> obs.err
	./nexus multiClause 50 >> obs.txt 2>> obs.err
	./nexus multiClause 500 >> obs.txt 2>> obs.err
	./nexus doInThen 0 >> obs.txt 2>> obs.err
	./nexus doInThen 5 >> obs.txt 2>> obs.err
	./nexus doInElse 0 >> obs.txt 2>> obs.err
	./nexus doInElse 5 >> obs.txt 2>> obs.err
	./nexus doInBoth 0 >> obs.txt 2>> obs.err
	./nexus doInBoth 5 >> obs.txt 2>> obs.err
	./nexus nestedGuards 0 >> obs.txt 2>> obs.err
	./nexus nestedGuards 5 >> obs.txt 2>> obs.err
	./nexus nestedGuards 30 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
