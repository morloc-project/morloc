all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus optIoPy 5                 >  obs.txt 2>  obs.err
	./nexus optIoPy -- -3             >> obs.txt 2>> obs.err
	./nexus optIoCpp 7                >> obs.txt 2>> obs.err
	./nexus optIoCpp -- -1            >> obs.txt 2>> obs.err
	./nexus optIoR 4                  >> obs.txt 2>> obs.err
	./nexus optIoR -- -1              >> obs.txt 2>> obs.err
	./nexus optIoChain 10             >> obs.txt 2>> obs.err
	./nexus optIoChain -- -5          >> obs.txt 2>> obs.err
	./nexus listOfThunksCpp           >> obs.txt 2>> obs.err
	./nexus guardSubsumePy 3          >> obs.txt 2>> obs.err
	./nexus guardSubsumePy 0          >> obs.txt 2>> obs.err
	./nexus guardMultiCpp 200         >> obs.txt 2>> obs.err
	./nexus guardMultiCpp 20          >> obs.txt 2>> obs.err
	./nexus guardMultiCpp 5           >> obs.txt 2>> obs.err
	./nexus guardMultiCpp -- -1       >> obs.txt 2>> obs.err
	./nexus nestedGuardR 200          >> obs.txt 2>> obs.err
	./nexus nestedGuardR 5            >> obs.txt 2>> obs.err
	./nexus nestedGuardR -- -1        >> obs.txt 2>> obs.err
	./nexus thunkForcedTwicePy        >> obs.txt 2>> obs.err
	./nexus crossOptRToCpp 42         >> obs.txt 2>> obs.err
	./nexus crossOptRToCpp -- -1      >> obs.txt 2>> obs.err
	./nexus argPurePy                 >> obs.txt 2>> obs.err
	./nexus argPureCpp                >> obs.txt 2>> obs.err
	./nexus argPureR                  >> obs.txt 2>> obs.err
	./nexus listMixedPy               >> obs.txt 2>> obs.err
	./nexus listMixedCpp              >> obs.txt 2>> obs.err
	./nexus listMixedR                >> obs.txt 2>> obs.err
	./nexus tupleMixedPy              >> obs.txt 2>> obs.err
	./nexus tupleMixedCpp             >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__
