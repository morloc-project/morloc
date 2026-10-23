all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus run hello '{"f0":"a","f1":"b","f2":"","f3":"","f4":"","f5":"","f6":"","f7":"","f8":"","f9":""}' > obs.txt 2> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
