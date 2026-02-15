all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus appendAllInt 1 2 3 > obs.txt 2> obs.err
	./nexus appendAllStr '"hello"' '" "' '"world"' >> obs.txt 2>> obs.err
	./nexus showAppend 10 20 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
