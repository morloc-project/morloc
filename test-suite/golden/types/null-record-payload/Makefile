all:
	rm -f *.err obs.txt
	morloc make -o py py.loc 2> build.err
	./py pa > obs.txt 2> obs.err
	./py pl >> obs.txt 2>> obs.err
	./py hs >> obs.txt 2>> obs.err
	./py counts >> obs.txt 2>> obs.err
	morloc make -o r r.loc 2>> build.err
	./r pa >> obs.txt 2>> obs.err
	./r pl >> obs.txt 2>> obs.err
	./r hs >> obs.txt 2>> obs.err
	./r counts >> obs.txt 2>> obs.err
	morloc make -o cpp cpp.loc 2>> build.err
	./cpp pa >> obs.txt 2>> obs.err
	./cpp pl >> obs.txt 2>> obs.err
	./cpp counts >> obs.txt 2>> obs.err
	./cpp nested >> obs.txt 2>> obs.err
	morloc make -o rust rust.loc 2>> build.err
	./rust pa >> obs.txt 2>> obs.err
	./rust pl >> obs.txt 2>> obs.err
	./rust counts >> obs.txt 2>> obs.err
	./rust nested >> obs.txt 2>> obs.err

clean:
	rm -rf py r cpp rust pools *err __pycache__
	rm -rf *-build *build.tmp*
