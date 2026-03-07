all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus testListLiteral > obs.txt 2> obs.err
	./nexus testListMempty >> obs.txt 2>> obs.err
	./nexus testListAppend >> obs.txt 2>> obs.err
	./nexus testListCons >> obs.txt 2>> obs.err
	./nexus testListUncons >> obs.txt 2>> obs.err
	./nexus testListSnoc >> obs.txt 2>> obs.err
	./nexus testListUnsnoc >> obs.txt 2>> obs.err
	./nexus testListAt >> obs.txt 2>> obs.err
	./nexus testDequeLiteral >> obs.txt 2>> obs.err
	./nexus testDequeMempty >> obs.txt 2>> obs.err
	./nexus testDequeAppend >> obs.txt 2>> obs.err
	./nexus testDequeCons >> obs.txt 2>> obs.err
	./nexus testDequeUncons >> obs.txt 2>> obs.err
	./nexus testDequeSnoc >> obs.txt 2>> obs.err
	./nexus testDequeUnsnoc >> obs.txt 2>> obs.err
	./nexus testVectorLiteral >> obs.txt 2>> obs.err
	./nexus testVectorAppend >> obs.txt 2>> obs.err
	./nexus testVectorCons >> obs.txt 2>> obs.err
	./nexus testVectorUncons >> obs.txt 2>> obs.err
	./nexus testVectorSnoc >> obs.txt 2>> obs.err
	./nexus testVectorUnsnoc >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
