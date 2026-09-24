all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	-./nexus captureLambda 1 > obs.txt 2> obs.err
	-./nexus captureNested 1 >> obs.txt 2>> obs.err
	-./nexus shadowTopLevel 1 >> obs.txt 2>> obs.err
	-./nexus shadowImport 5 >> obs.txt 2>> obs.err
	-./nexus sameNameHelpers 2 >> obs.txt 2>> obs.err
	-./nexus crossModuleHelper 3 >> obs.txt 2>> obs.err
	-./nexus recursiveViaAlias 10 >> obs.txt 2>> obs.err
	-./nexus recursiveExport 10 >> obs.txt 2>> obs.err
	-./nexus recursiveExportUsed 10 >> obs.txt 2>> obs.err
	-./nexus recursiveExportDropped 7 >> obs.txt 2>> obs.err
	-./nexus lambdaShadowsWhere 5 >> obs.txt 2>> obs.err
	-./nexus patternInRecursiveHelper '[1,2]' >> obs.txt 2>> obs.err
	-./nexus shadowFromSibling 1 >> obs.txt 2>> obs.err
	-morloc make -o shadow shadow.loc 2> shadow-build.err
	-./shadow shadowTopLevelHelper 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus shadow __pycache__ *.err
	rm -rf *-build *build.tmp*
