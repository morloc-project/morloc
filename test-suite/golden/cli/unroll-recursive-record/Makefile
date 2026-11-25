all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- one ---" >> obs.txt
	./nexus depth --label a >> obs.txt 2>> obs.err
	echo "--- chained ---" >> obs.txt
	./nexus depth --label a --next '{"label":"b","next":{"label":"c","next":null}}' >> obs.txt 2>> obs.err
	echo "--- empty ---" >> obs.txt
	./nexus depth >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
