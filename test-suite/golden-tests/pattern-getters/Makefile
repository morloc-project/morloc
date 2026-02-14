all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testr   >  obs.txt
	./nexus testpy  2>> obs.err  >> obs.txt
	./nexus testcpp 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus __pycache__
