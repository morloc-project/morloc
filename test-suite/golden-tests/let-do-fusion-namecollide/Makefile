all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- collide ---" > obs.txt
	./nexus collide >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
