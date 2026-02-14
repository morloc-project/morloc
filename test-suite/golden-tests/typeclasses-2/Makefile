all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus paste '["a","b"]' > obs.txt 2> obs.err
	./nexus sum '[1,2,3]' 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus __pycache__ y z
