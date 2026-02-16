all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- forceOnce ---" > obs.txt
	./nexus forceOnce >> obs.txt 2>> obs.err
	echo "--- forceTwice ---" >> obs.txt
	./nexus forceTwice >> obs.txt 2>> obs.err
	echo "--- forceShared ---" >> obs.txt
	./nexus forceShared >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
