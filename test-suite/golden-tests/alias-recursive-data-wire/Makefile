all:
	rm -f *.err obs.txt *.pkt
	morloc make -o nexus main.loc 2> build.err
	./nexus idT '{"Node":[{"Leaf":[1]},{"Leaf":[2]}]}' > obs.txt 2>> obs.err
	./nexus keep one.pkt >> obs.txt 2>> obs.err
	./nexus keepAll all.pkt >> obs.txt 2>> obs.err
	./nexus rose 1 >> obs.txt 2>> obs.err
	./nexus rose2 1 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__ *.err *.pkt
	rm -rf *-build *build.tmp*
