all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus entryPay 5 >> obs.txt 2>> obs.err
	./nexus entryOpt 5 >> obs.txt 2>> obs.err
	./nexus entryOpt null >> obs.txt 2>> obs.err
	./nexus optOut 5 >> obs.txt 2>> obs.err
	./nexus fnValue 5 >> obs.txt 2>> obs.err
	./nexus listElemField 5 >> obs.txt 2>> obs.err
	./nexus armBeside 5 >> obs.txt 2>> obs.err
	./nexus tupleFields 5 >> obs.txt 2>> obs.err
	./nexus testLet 5 >> obs.txt 2>> obs.err
	./nexus guardLet 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
