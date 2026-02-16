all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- letShared ---" > obs.txt
	./nexus letShared >> obs.txt 2>> obs.err
	echo "--- letIndep ---" >> obs.txt
	./nexus letIndep >> obs.txt 2>> obs.err
	echo "--- letChain ---" >> obs.txt
	./nexus letChain >> obs.txt 2>> obs.err
	echo "--- letMultiForce ---" >> obs.txt
	./nexus letMultiForce >> obs.txt 2>> obs.err
	echo "--- letNested ---" >> obs.txt
	./nexus letNested >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
