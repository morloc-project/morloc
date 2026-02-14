all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo '[6,5]' > obs.txt 2> obs.err
	./nexus bar '[6,5]' 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools
