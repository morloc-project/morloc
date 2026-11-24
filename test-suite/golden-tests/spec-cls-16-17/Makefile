all:
	rm -rf *.err obs.txt std-build
	morloc make -o nexus main.loc 2> build.err
	./nexus run '[{"x":1},{"x":2}]' >> obs.txt 2>> obs.err
	./nexus direct '[{"x":1},{"x":2}]' >> obs.txt 2>> obs.err
	morloc make -o std std.loc 2>> build.err
	./std run '[{"x":1},{"x":2}]' >> obs.txt 2>> obs.err
	grep -c "\"pools\":\[\]" std-build/manifest.json >> obs.txt

clean:
	rm -rf nexus std pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
