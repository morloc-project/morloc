all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err && echo "UNEXPECTED_SUCCESS" > obs.txt || echo "build_rejected" > obs.txt

clean:
	rm -rf nexus pools log __pycache__
