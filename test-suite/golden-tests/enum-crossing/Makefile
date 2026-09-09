all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- pyCompA ---" >> obs.txt
	./nexus pyComp A >> obs.txt 2>> obs.err
	echo "--- pyCompC ---" >> obs.txt
	./nexus pyComp C >> obs.txt 2>> obs.err
	echo "--- cppCompG ---" >> obs.txt
	./nexus cppComp G >> obs.txt 2>> obs.err
	echo "--- rCompT ---" >> obs.txt
	./nexus rComp T >> obs.txt 2>> obs.err
	echo "--- rCompC ---" >> obs.txt
	./nexus rComp C >> obs.txt 2>> obs.err
	echo "--- rustCompA ---" >> obs.txt
	./nexus rustComp A >> obs.txt 2>> obs.err
	echo "--- pyToCpp ---" >> obs.txt
	./nexus pyToCpp G >> obs.txt 2>> obs.err
	echo "--- cppToR ---" >> obs.txt
	./nexus cppToR C >> obs.txt 2>> obs.err
	echo "--- rToRust ---" >> obs.txt
	./nexus rToRust T >> obs.txt 2>> obs.err
	echo "--- listComp ---" >> obs.txt
	./nexus listComp '["A","C","G","T"]' >> obs.txt 2>> obs.err
	echo "--- optIdValue ---" >> obs.txt
	./nexus optId G >> obs.txt 2>> obs.err
	echo "--- optIdNull ---" >> obs.txt
	./nexus optId null >> obs.txt 2>> obs.err
	echo "--- jsonHelpEnum ---" >> obs.txt
	if ./nexus --json-help 2>> obs.err | grep -q '"enum"'; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi
	echo "--- rejectsUnknownConstructor ---" >> obs.txt
	if ./nexus pyComp N >> obs.err 2>&1; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
