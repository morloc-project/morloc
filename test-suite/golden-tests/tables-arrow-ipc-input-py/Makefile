all:
	rm -f *.err obs.txt input.arrow
	python3 mkfixture.py
	morloc make -o nexus main.loc 2> build.err
	./nexus echo input.arrow > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__ input.arrow
