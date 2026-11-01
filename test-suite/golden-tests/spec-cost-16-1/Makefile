all:
	rm -f *.err obs.txt
	morloc typecheck --realize main.loc 2> build.err | sed -n '/^placement:$$/,/^$$/p' > obs.txt
	morloc make -o nexus main.loc 2>> build.err
	./nexus doubled 3 >> obs.txt
	./nexus shown 3 >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
