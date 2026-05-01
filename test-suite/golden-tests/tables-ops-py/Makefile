all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	{ \
	  ./nexus sliced    5 ; \
	  ./nexus sorted    5 ; \
	  ./nexus picked    4 ; \
	  ./nexus reversed_ 4 ; \
	} > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__
