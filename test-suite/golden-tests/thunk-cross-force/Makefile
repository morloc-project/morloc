all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- combined ---" > obs.txt
	./nexus combined >> obs.txt 2>> obs.err
	echo "--- addForced ---" >> obs.txt
	./nexus addForced >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
