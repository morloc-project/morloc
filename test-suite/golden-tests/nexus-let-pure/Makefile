all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- letSimpleInt ---" > obs.txt
	./nexus letSimpleInt >> obs.txt 2> obs.err
	echo "--- letSimpleStr ---" >> obs.txt
	./nexus letSimpleStr >> obs.txt 2>> obs.err
	echo "--- letSimpleBool ---" >> obs.txt
	./nexus letSimpleBool >> obs.txt 2>> obs.err
	echo "--- letSimpleReal ---" >> obs.txt
	./nexus letSimpleReal >> obs.txt 2>> obs.err
	echo "--- letSimpleList ---" >> obs.txt
	./nexus letSimpleList >> obs.txt 2>> obs.err
	echo "--- letSimpleTuple ---" >> obs.txt
	./nexus letSimpleTuple >> obs.txt 2>> obs.err
	echo "--- letNested ---" >> obs.txt
	./nexus letNested >> obs.txt 2>> obs.err
	echo "--- letCrossRef ---" >> obs.txt
	./nexus letCrossRef >> obs.txt 2>> obs.err
	echo "--- letDeepChain ---" >> obs.txt
	./nexus letDeepChain >> obs.txt 2>> obs.err
	echo "--- letMultiRef ---" >> obs.txt
	./nexus letMultiRef >> obs.txt 2>> obs.err
	echo "--- letUnused ---" >> obs.txt
	./nexus letUnused >> obs.txt 2>> obs.err
	echo "--- letShadow ---" >> obs.txt
	./nexus letShadow >> obs.txt 2>> obs.err
	echo "--- letNestedStruct ---" >> obs.txt
	./nexus letNestedStruct >> obs.txt 2>> obs.err
	echo "--- letGetter ---" >> obs.txt
	./nexus letGetter >> obs.txt 2>> obs.err
	echo "--- letWithArg ---" >> obs.txt
	./nexus letWithArg 42 >> obs.txt 2>> obs.err
	echo "--- letSelfRef ---" >> obs.txt
	./nexus letSelfRef >> obs.txt 2>> obs.err
	echo "--- letDeepNest ---" >> obs.txt
	./nexus letDeepNest >> obs.txt 2>> obs.err
	echo "--- letCrossRefChain ---" >> obs.txt
	./nexus letCrossRefChain >> obs.txt 2>> obs.err
	echo "--- letRecord ---" >> obs.txt
	./nexus letRecord >> obs.txt 2>> obs.err
	echo "--- letRecordGetter ---" >> obs.txt
	./nexus letRecordGetter >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log
