all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	>obs.txt
	./nexus singleChar 2>> obs.err >> obs.txt
	./nexus emptyLike 2>> obs.err >> obs.txt
	./nexus rtl 2>> obs.err >> obs.txt
	./nexus combining 2>> obs.err >> obs.txt
	./nexus surrogateLike 2>> obs.err >> obs.txt
	./nexus nullLike 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
