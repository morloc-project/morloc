all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus pfoo '"wena"' 2> obs.txt 2>> obs.err  >> obs.txt
	./nexus rfoo '"wena"' 2>> obs.txt 2>> obs.err  >> obs.txt
	./nexus cfoo '"wena"' 2>> obs.txt 2>> obs.err  >> obs.txt


clean:
	rm -rf nexus log __pycache__
