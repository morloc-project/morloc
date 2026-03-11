all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus addTwo 3 4 > obs.txt 2> obs.err
	./nexus mulAdd 2 3 10 >> obs.txt 2>> obs.err
	./nexus nested 1 2 3 4 >> obs.txt 2>> obs.err
	./nexus letExpr 3 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
