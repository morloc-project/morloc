all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus myabs 5 > obs.txt 2> obs.err
	./nexus myabs 0 >> obs.txt 2>> obs.err
	./nexus clamp 2 8 5 >> obs.txt 2>> obs.err
	./nexus clamp 2 8 1 >> obs.txt 2>> obs.err
	./nexus clamp 2 8 10 >> obs.txt 2>> obs.err
	./nexus classify 5 >> obs.txt 2>> obs.err
	./nexus classify 50 >> obs.txt 2>> obs.err
	./nexus classify 200 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
