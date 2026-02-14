all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus price --spot 100 --strike 105 --volatility 0.2 --paths 100 --steps 50 > obs.txt 2> obs.err
	./nexus analyzeDist --spot 100 --paths 100 2>> obs.err  >> obs.txt
	./nexus paths --paths 5 --steps 10 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus __pycache__
