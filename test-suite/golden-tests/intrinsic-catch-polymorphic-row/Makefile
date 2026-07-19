all:
	rm -f *.err obs.txt
	rm -f /tmp/poly-catch-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus catchIntLoadFallback                                         >  obs.txt 2>  obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/poly-catch-*.dat
