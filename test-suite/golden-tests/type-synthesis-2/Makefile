all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus f '"hello"' > obs.txt 2> obs.err

clean:
	rm -rf nexus nexus.cpy lib/foo/__pycache__
