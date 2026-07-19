all:
	rm -f *.err obs.txt
	rm -f /tmp/catch-crosslang-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus saveCpp        /tmp/catch-crosslang-vec.dat        >  obs.txt 2>  obs.err
	./nexus loadPyHappy    /tmp/catch-crosslang-vec.dat        >> obs.txt 2>> obs.err
	./nexus loadPyMismatch /tmp/catch-crosslang-vec.dat        >> obs.txt 2>> obs.err
	rm -f /tmp/catch-crosslang-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/catch-crosslang-*.dat
