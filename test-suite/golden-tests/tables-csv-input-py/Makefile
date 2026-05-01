all:
	rm -f *.err obs.txt input.csv
	morloc make -o nexus main.loc 2> build.err
	printf 'x,y\n42,alpha\n17,beta\n99,gamma\n' > input.csv
	./nexus echo input.csv > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__ input.csv
