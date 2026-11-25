all:
	: > obs.txt
	rm -f *.err effRec.stderr
	morloc make -o nexus main.loc 2> build.err
	./nexus effRec 5 2 >> obs.txt 2> effRec.stderr
	@grep -c "^noisy:5$$" effRec.stderr >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err effRec.stderr
	rm -rf *-build *build.tmp*
