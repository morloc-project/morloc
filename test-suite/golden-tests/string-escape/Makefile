all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	>obs.txt
	./nexus pyQ    2>> obs.err >> obs.txt
	./nexus cppQ   2>> obs.err >> obs.txt
	./nexus rQ     2>> obs.err >> obs.txt
	./nexus pyAll  2>> obs.err >> obs.txt
	./nexus cppAll 2>> obs.err >> obs.txt
	./nexus rAll   2>> obs.err >> obs.txt
	./nexus pureQ  2>> obs.err >> obs.txt
	./nexus pureAll 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
