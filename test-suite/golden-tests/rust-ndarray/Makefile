all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus dot '[1.0,2.0,3.0]' '[4.0,5.0,6.0]'                    >> obs.txt 2>> obs.err
	./nexus matmul '[[2,2],[1.0,2.0,3.0,4.0]]' '[[2,2],[5.0,6.0,7.0,8.0]]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *-build *.err
