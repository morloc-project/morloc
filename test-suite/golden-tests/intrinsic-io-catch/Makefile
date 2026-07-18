all:
	rm -f *.err obs.txt
	rm -f /tmp/io-catch-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus catchLoad 42                                            >  obs.txt 2>  obs.err
	./nexus catchLoadFallbackChain                                  >> obs.txt 2>> obs.err
	./nexus catchRead x                                             >> obs.txt 2>> obs.err
	./nexus catchOpen                                               >> obs.txt 2>> obs.err
	./nexus catchOpenTypedIStream                                   >> obs.txt 2>> obs.err
	./nexus catchFschema                                            >> obs.txt 2>> obs.err
	./nexus catchLoadNested                                         >> obs.txt 2>> obs.err
	./nexus directOpenSucceeds /tmp/io-catch-direct-ok.dat          >> obs.txt 2>> obs.err
	rm -f /tmp/io-catch-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/io-catch-*.dat
