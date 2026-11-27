all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus make 3 > obs.txt 2>> obs.err
	./nexus viaPy '{"grün":1,"名前":"rot"}' >> obs.txt 2>> obs.err
	./nexus viaCpp '{"grün":2,"名前":"rot"}' >> obs.txt 2>> obs.err
	./nexus viaR '{"grün":3,"名前":"rot"}' >> obs.txt 2>> obs.err
	./nexus chain '{"grün":4,"名前":"grau"}' >> obs.txt 2>> obs.err
	./nexus colours '["Hell","Mittelgroß"]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
