all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus crossLangRec 5 3 >> obs.txt 2> obs.err
	./nexus crossLangRec 10 0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
