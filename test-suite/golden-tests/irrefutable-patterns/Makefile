all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- letTuple0 ---" >> obs.txt
	./nexus letTuple0 >> obs.txt 2>> obs.err
	echo "--- letTuple1 ---" >> obs.txt
	./nexus letTuple1 >> obs.txt 2>> obs.err
	echo "--- letRecordA ---" >> obs.txt
	./nexus letRecordA >> obs.txt 2>> obs.err
	echo "--- letRecordB ---" >> obs.txt
	./nexus letRecordB >> obs.txt 2>> obs.err
	echo "--- letRecordReorder ---" >> obs.txt
	./nexus letRecordReorder >> obs.txt 2>> obs.err
	echo "--- letWildcardOnly ---" >> obs.txt
	./nexus letWildcardOnly >> obs.txt 2>> obs.err
	echo "--- letAsPair ---" >> obs.txt
	./nexus letAsPair >> obs.txt 2>> obs.err
	echo "--- fnTupleFst ---" >> obs.txt
	./nexus fnTupleFst '[1,2]' >> obs.txt 2>> obs.err
	echo "--- fnTupleSnd ---" >> obs.txt
	./nexus fnTupleSnd '[1,2]' >> obs.txt 2>> obs.err
	echo "--- fnRecordPick ---" >> obs.txt
	./nexus fnRecordPick '{"a":5,"b":6}' >> obs.txt 2>> obs.err
	echo "--- fnNestedMix ---" >> obs.txt
	./nexus fnNestedMix '[10,{"a":1,"b":2}]' >> obs.txt 2>> obs.err
	echo "--- lamTupleFst ---" >> obs.txt
	./nexus lamTupleFst >> obs.txt 2>> obs.err
	echo "--- lamWildcardKeep ---" >> obs.txt
	./nexus lamWildcardKeep >> obs.txt 2>> obs.err
	echo "--- doTupleBind ---" >> obs.txt
	./nexus doTupleBind >> obs.txt 2>> obs.err
	echo "--- doRecordBind ---" >> obs.txt
	./nexus doRecordBind >> obs.txt 2>> obs.err
	echo "--- doWildcardBind ---" >> obs.txt
	./nexus doWildcardBind >> obs.txt 2>> obs.err
	echo "--- doNestedBind ---" >> obs.txt
	./nexus doNestedBind >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
