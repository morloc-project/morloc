all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus untagRust '["a","b"]' 2>> obs.err >> obs.txt
	./nexus tagRust '["a","b"]' 2>> obs.err >> obs.txt
	./nexus untagCpp '["a","b"]' 2>> obs.err >> obs.txt
	./nexus tagCpp '["a","b"]' 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools log __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
