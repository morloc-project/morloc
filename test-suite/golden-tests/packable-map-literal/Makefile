all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus mapSize >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
