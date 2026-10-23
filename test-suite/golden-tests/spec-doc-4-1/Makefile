all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus travel -h 2>> obs.err | grep -c "Travel time" >> obs.txt || true
	./nexus travel2 -h 2>> obs.err | grep -c "Travel time" >> obs.txt || true
	./nexus travel2 -h 2>> obs.err | grep -c "Wind speed in m/s" >> obs.txt || true
	./nexus travel2 -w 7 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
