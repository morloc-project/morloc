all:
	rm -f *.err obs.txt *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus test_addition > obs.txt 2> obs.err
	./nexus test_multiplication 2>> obs.err  >> obs.txt
	./nexus test_exponentiation 2>> obs.err  >> obs.txt
	./nexus test_concat 2>> obs.err  >> obs.txt
	./nexus test_combined 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools log
