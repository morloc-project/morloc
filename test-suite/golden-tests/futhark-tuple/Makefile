all:
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus twSum >> obs.txt 2> obs.err
	./nexus tTag >> obs.txt 2>> obs.err
	./nexus tScaled >> obs.txt 2>> obs.err
	./nexus tMat >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools build.err obs.err __pycache__
