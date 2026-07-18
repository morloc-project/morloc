all:
	rm -f *.err obs.txt
	rm -f /tmp/load-schema-mismatch-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus saveThenLoadWrongType /tmp/load-schema-mismatch-str.dat  >  obs.txt 2>  obs.err
	rm -f /tmp/load-schema-mismatch-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/load-schema-mismatch-*.dat
