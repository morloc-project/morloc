all:
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus mmSum >> obs.txt 2> obs.err
	./nexus vScaledSum 2.0 '[1,2,3,4]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools build.err obs.err __pycache__
