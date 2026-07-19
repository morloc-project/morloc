all:
	rm -f *.err obs.txt
	rm -f /tmp/catch-varwidth-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus catchStrHappy         /tmp/catch-varwidth-str.dat        >  obs.txt 2>  obs.err
	./nexus catchStrFallback                                         >> obs.txt 2>> obs.err
	./nexus catchListHappy        /tmp/catch-varwidth-list.dat       >> obs.txt 2>> obs.err
	./nexus catchListFallback                                        >> obs.txt 2>> obs.err
	./nexus catchTupleHappy       /tmp/catch-varwidth-tup.dat        >> obs.txt 2>> obs.err
	./nexus catchTupleFallback                                       >> obs.txt 2>> obs.err
	./nexus catchListStrHappy     /tmp/catch-varwidth-liststr.dat    >> obs.txt 2>> obs.err
	./nexus catchListStrFallback                                     >> obs.txt 2>> obs.err
	rm -f /tmp/catch-varwidth-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/catch-varwidth-*.dat
