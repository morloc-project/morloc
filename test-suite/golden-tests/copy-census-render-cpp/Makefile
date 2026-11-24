all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus texts 1 > texts.out 2>> obs.err || echo "failed" >> obs.txt
	grep -ao 'copies=[0-9]*' texts.out >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err texts.out
