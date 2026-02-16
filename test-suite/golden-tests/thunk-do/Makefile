all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- doReturn ---" > obs.txt
	./nexus doReturn >> obs.txt 2>> obs.err
	echo "--- doBind ---" >> obs.txt
	./nexus doBind >> obs.txt 2>> obs.err
	echo "--- doBare ---" >> obs.txt
	./nexus doBare >> obs.txt 2>> obs.err
	echo "--- doChain ---" >> obs.txt
	./nexus doChain >> obs.txt 2>> obs.err
	echo "--- doMixed ---" >> obs.txt
	./nexus doMixed >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
