all:
	rm -f *.err obs.txt *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus x > obs.txt 2> obs.err
	./nexus y 2>> obs.err  >> obs.txt
	./nexus z 2>> obs.err  >> obs.txt
	./nexus f 99 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus log __pycache__
