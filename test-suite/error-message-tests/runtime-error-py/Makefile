all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus failme 0 2>&1 | grep -c "PYERR" > obs.txt || echo "0" > obs.txt

clean:
	rm -rf nexus pools* __pycache__ *.err obs.txt .manifest
