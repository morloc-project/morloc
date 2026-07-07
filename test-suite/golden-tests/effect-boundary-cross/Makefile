all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus cppFromPy 5           >  obs.txt 2> obs.err
	./nexus pyFromCpp 5           >> obs.txt 2>> obs.err
	./nexus rpcInDoBlock          >> obs.txt 2>> obs.err
	./nexus rpcMultiEffect        >> obs.txt 2>> obs.err
	./nexus rpcCallbackAcross     >> obs.txt 2>> obs.err
	./nexus rpcNullary            >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__
