all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- forceTuple ---" > obs.txt
	./nexus forceTuple >> obs.txt 2>> obs.err
	echo "--- forceApp ---" >> obs.txt
	./nexus forceApp >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
