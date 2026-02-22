all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus srcFoo '"c"' > obs.txt 2> obs.err
	./nexus natFoo '"e"' 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools __pycache__
