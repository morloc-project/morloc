all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus once -h 2>> obs.err | sed -n '/^Return:/,/^$$/p' >> obs.txt
	./nexus plain -h 2>> obs.err | sed -n '/^Return:/,/^$$/p' >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
