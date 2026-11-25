all:
	rm -f *.err obs.txt
	rm -f *.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus shapeReadCpp shapeReadCpp.dat >> obs.txt 2>> obs.err
	./nexus shapeWriteCpp shapeWriteCpp.dat >> obs.txt 2>> obs.err
	./nexus shapeReadPy shapeReadPy.dat >> obs.txt 2>> obs.err
	./nexus shapeWritePy shapeWritePy.dat >> obs.txt 2>> obs.err
	./nexus shapeReadR shapeReadR.dat >> obs.txt 2>> obs.err
	./nexus shapeWriteR shapeWriteR.dat >> obs.txt 2>> obs.err
	./nexus estrReadCpp estrReadCpp.dat >> obs.txt 2>> obs.err
	./nexus estrWriteCpp estrWriteCpp.dat >> obs.txt 2>> obs.err
	./nexus estrReadPy estrReadPy.dat >> obs.txt 2>> obs.err
	./nexus estrWritePy estrWritePy.dat >> obs.txt 2>> obs.err
	./nexus estrReadR estrReadR.dat >> obs.txt 2>> obs.err
	./nexus estrWriteR estrWriteR.dat >> obs.txt 2>> obs.err
	./nexus vfstReadCpp vfstReadCpp.dat >> obs.txt 2>> obs.err
	./nexus vfstWriteCpp vfstWriteCpp.dat >> obs.txt 2>> obs.err
	./nexus vfstReadPy vfstReadPy.dat >> obs.txt 2>> obs.err
	./nexus vfstWritePy vfstWritePy.dat >> obs.txt 2>> obs.err
	./nexus vfstReadR vfstReadR.dat >> obs.txt 2>> obs.err
	./nexus vfstWriteR vfstWriteR.dat >> obs.txt 2>> obs.err
	rm -f *.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f *.dat
	rm -rf *-build *build.tmp*
