all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- cppGrow ---" >> obs.txt
	./nexus cppGrow '{"Full":[1]}' >> obs.txt 2>> obs.err
	echo "--- rustGrow ---" >> obs.txt
	./nexus rustGrow '{"Full":[1]}' >> obs.txt 2>> obs.err
	echo "--- pyGrow ---" >> obs.txt
	./nexus pyGrow '{"Full":[1]}' >> obs.txt 2>> obs.err
	echo "--- rGrow ---" >> obs.txt
	./nexus rGrow '{"Full":[1]}' >> obs.txt 2>> obs.err
	echo "--- cppShout ---" >> obs.txt
	./nexus cppShout '{"Full":["hi"]}' >> obs.txt 2>> obs.err
	echo "--- rustShout ---" >> obs.txt
	./nexus rustShout '{"Full":["hi"]}' >> obs.txt 2>> obs.err
	echo "--- cppWrap ---" >> obs.txt
	./nexus cppWrap '{"item":1}' >> obs.txt 2>> obs.err
	echo "--- rustWrap ---" >> obs.txt
	./nexus rustWrap '{"item":1}' >> obs.txt 2>> obs.err
	echo "--- pyWrap ---" >> obs.txt
	./nexus pyWrap '{"item":1}' >> obs.txt 2>> obs.err
	echo "--- rWrap ---" >> obs.txt
	./nexus rWrap '{"item":1}' >> obs.txt 2>> obs.err
	echo "--- cppMade ---" >> obs.txt
	./nexus cppMade 5 >> obs.txt 2>> obs.err
	echo "--- rustMade ---" >> obs.txt
	./nexus rustMade 5 >> obs.txt 2>> obs.err
	echo "--- cppWrapMade ---" >> obs.txt
	./nexus cppWrapMade 5 >> obs.txt 2>> obs.err
	echo "--- rustWrapMade ---" >> obs.txt
	./nexus rustWrapMade 5 >> obs.txt 2>> obs.err
	echo "--- cppUnbox ---" >> obs.txt
	./nexus cppUnbox '{"Full":[5]}' >> obs.txt 2>> obs.err
	./nexus cppUnbox '"Empty"' >> obs.txt 2>> obs.err
	echo "--- rustUnbox ---" >> obs.txt
	./nexus rustUnbox '{"Full":[5]}' >> obs.txt 2>> obs.err
	./nexus rustUnbox '"Empty"' >> obs.txt 2>> obs.err
	echo "--- roundTrip ---" >> obs.txt
	./nexus roundTrip '{"Full":[0]}' >> obs.txt 2>> obs.err
	./nexus roundTrip '"Empty"' >> obs.txt 2>> obs.err
	echo "--- cppDepth ---" >> obs.txt
	./nexus cppDepth 1 >> obs.txt 2>> obs.err
	echo "--- rustDepth ---" >> obs.txt
	./nexus rustDepth 1 >> obs.txt 2>> obs.err
	echo "--- cppRoseSize ---" >> obs.txt
	./nexus cppRoseSize '{"Branch":[1,[{"Tip":[2]},{"Branch":[3,[{"Tip":[4]}]]}]]}' >> obs.txt 2>> obs.err
	echo "--- rustRoseSize ---" >> obs.txt
	./nexus rustRoseSize '{"Branch":[1,[{"Tip":[2]},{"Branch":[3,[{"Tip":[4]}]]}]]}' >> obs.txt 2>> obs.err
	echo "--- cppRoseMade ---" >> obs.txt
	./nexus cppRoseMade 1 >> obs.txt 2>> obs.err
	echo "--- rustRoseMade ---" >> obs.txt
	./nexus rustRoseMade 1 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
