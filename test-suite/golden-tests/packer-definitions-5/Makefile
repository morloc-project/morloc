all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo 10 20 > obs.txt 2> obs.err

.PHONY: clean
clean:
	rm -rf nexus *pdf *rda __pycache__
