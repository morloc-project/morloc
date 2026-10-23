all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo -h 2>> obs.err | grep -c "Minutes until the storm\|minutes from the alias\|Wind speed in m/s" >> obs.txt || true
	./nexus bar -h 2>> obs.err | grep -c "Minutes until the storm\|minutes from the alias" >> obs.txt || true
	./nexus foo -w 7 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
