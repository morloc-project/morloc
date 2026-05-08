all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- ioOnly ---" > obs.txt
	./nexus ioOnly >> obs.txt 2>> obs.err
	echo "--- effectThenPure ---" >> obs.txt
	./nexus effectThenPure >> obs.txt 2>> obs.err
	echo "--- polyglotEffectChain ---" >> obs.txt
	./nexus polyglotEffectChain >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
