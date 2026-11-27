all:
	rm -f *.err obs.txt
	morloc make -o recordwire record-wire.loc 2> build.err
	./recordwire howRec '{"left":1,"right":"x"}' >> obs.txt 2> obs.err

clean:
	rm -f *.err
	rm -rf recordwire __pycache__
	rm -rf *-build *build.tmp*
