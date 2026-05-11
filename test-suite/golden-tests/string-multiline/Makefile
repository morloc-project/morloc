all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus s1 >  obs.txt
	./nexus s2 2>> obs.err  >> obs.txt
	./nexus s3 2>> obs.err  >> obs.txt
	./nexus s4 2>> obs.err  >> obs.txt
	./nexus s5 2>> obs.err  >> obs.txt
	./nexus s6 2>> obs.err  >> obs.txt
	./nexus s7 2>> obs.err  >> obs.txt
	./nexus s8 2>> obs.err  >> obs.txt
	./nexus s9 2>> obs.err  >> obs.txt
	./nexus s10 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools
