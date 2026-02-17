all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo > obs.txt

clean:
	rm -rf nexus pools log __pycache__
