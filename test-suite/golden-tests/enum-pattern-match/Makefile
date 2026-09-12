all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- complementA ---" >> obs.txt
	./nexus complement A >> obs.txt 2>> obs.err
	echo "--- complementC ---" >> obs.txt
	./nexus complement C >> obs.txt 2>> obs.err
	echo "--- complementG ---" >> obs.txt
	./nexus complement G >> obs.txt 2>> obs.err
	echo "--- complementT ---" >> obs.txt
	./nexus complement T >> obs.txt 2>> obs.err
	echo "--- purineA ---" >> obs.txt
	./nexus purine A >> obs.txt 2>> obs.err
	echo "--- purineC ---" >> obs.txt
	./nexus purine C >> obs.txt 2>> obs.err
	echo "--- purineG ---" >> obs.txt
	./nexus purine G >> obs.txt 2>> obs.err
	echo "--- purineT ---" >> obs.txt
	./nexus purine T >> obs.txt 2>> obs.err
	echo "--- firstOfPairHead ---" >> obs.txt
	./nexus firstOfPair '["A","C"]' >> obs.txt 2>> obs.err
	echo "--- firstOfPairTail ---" >> obs.txt
	./nexus firstOfPair '["C","A"]' >> obs.txt 2>> obs.err
	echo "--- firstOfPairNone ---" >> obs.txt
	./nexus firstOfPair '["C","G"]' >> obs.txt 2>> obs.err
	echo "--- roundTripC ---" >> obs.txt
	./nexus roundTrip C >> obs.txt 2>> obs.err
	echo "--- roundTripT ---" >> obs.txt
	./nexus roundTrip T >> obs.txt 2>> obs.err
	echo "--- ordinalWalk ---" >> obs.txt
	./nexus ordinalWalk '["A","C","G","T"]' >> obs.txt 2>> obs.err
	echo "--- crossPool ---" >> obs.txt
	./nexus crossPool G >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
