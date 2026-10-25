all:
	rm -f *.err obs.txt
	morloc typecheck --realize main.loc 2> build.err | sed -n '/^placement:$$/,/^$$/p' > obs.txt
	morloc make -o nexus main.loc 2>> build.err
	./nexus often '[1,2,3,4]' >> obs.txt
	./nexus once '[1,2,3,4]' >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
