all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus crashme 0 2>&1 | grep -ci "signal\|crash\|abort" > obs.txt || echo "0" > obs.txt

clean:
	rm -rf nexus pools* __pycache__ *.err obs.txt .manifest
