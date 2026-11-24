all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus fnFields 1 >> obs.txt 2>> obs.err
	./nexus fnFields 0 >> obs.txt 2>> obs.err
	./nexus pyLet 3 >> obs.txt 2>> obs.err
	./nexus pyLet 0 >> obs.txt 2>> obs.err
	./nexus shadowed 1 >> obs.txt 2>> obs.err
	./nexus nested 2 >> obs.txt 2>> obs.err
	./nexus nested 1 >> obs.txt 2>> obs.err
	./nexus nested 0 >> obs.txt 2>> obs.err
	./nexus unread 1 >> obs.txt 2>> obs.err
	./nexus unread 0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
