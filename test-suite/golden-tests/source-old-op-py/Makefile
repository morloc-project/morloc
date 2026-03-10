all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus test_add 3 4 > obs.txt 2> obs.err
	./nexus test_sub 10 3 >> obs.txt 2>> obs.err
	./nexus test_mul 5 6 >> obs.txt 2>> obs.err
	./nexus test_neg 42 >> obs.txt 2>> obs.err
	./nexus test_expr >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
