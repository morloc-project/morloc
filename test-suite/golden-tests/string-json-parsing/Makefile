all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus pfoo a.json > obs.txt 2> obs.err
	./nexus pfoo b.json 2>> obs.err  >> obs.txt
	./nexus cfoo a.json 2>> obs.err  >> obs.txt
	./nexus cfoo b.json 2>> obs.err  >> obs.txt
	./nexus rfoo a.json 2>> obs.err  >> obs.txt
	./nexus rfoo b.json 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools __pycache__
