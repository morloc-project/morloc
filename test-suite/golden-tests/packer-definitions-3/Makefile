all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo '"Alice"' > obs.txt 2> obs.err

.PHONY: clean
clean:
	rm -rf nexus pools *pdf *rda __pycache__
