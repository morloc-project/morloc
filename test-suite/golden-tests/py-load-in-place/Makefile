all:
	rm -f *.err obs.txt /tmp/py-lip-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus emptyLoad /tmp/py-lip-emptyLoad.dat >> obs.txt 2>> obs.err
	./nexus strLoad /tmp/py-lip-strLoad.dat >> obs.txt 2>> obs.err
	./nexus recLoad /tmp/py-lip-recLoad.dat >> obs.txt 2>> obs.err
	./nexus nestedLoad /tmp/py-lip-nestedLoad.dat >> obs.txt 2>> obs.err
	./nexus streamLoad /tmp/py-lip-streamLoad.dat >> obs.txt 2>> obs.err
	./nexus vecLoad /tmp/py-lip-vecLoad.dat >> obs.txt 2>> obs.err
	./nexus bytesLoad /tmp/py-lip-bytesLoad.dat >> obs.txt 2>> obs.err
	rm -f /tmp/py-lip-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
	rm -f /tmp/py-lip-*.dat
