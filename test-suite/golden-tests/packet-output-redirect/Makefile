all:
	: > obs.txt
	rm -f *.err *.pkt
	morloc make -o nexus main.loc 2> build.err
	./nexus -f packet -z 0 pyBig 2000000 > py-new.pkt 2>> obs.err
	: > py-app.pkt && ./nexus -f packet -z 0 pyBig 2000000 >> py-app.pkt 2>> obs.err
	cmp -s py-new.pkt py-app.pkt && echo SAME >> obs.txt || echo DIFFERENT >> obs.txt
	printf 'abc' > py-pre.pkt && ./nexus -f packet -z 0 pyBig 2000000 >> py-pre.pkt 2>> obs.err
	tail -c +4 py-pre.pkt | cmp -s - py-new.pkt && echo SAME >> obs.txt || echo DIFFERENT >> obs.txt
	./nexus -f packet -z 3 cppBig 2000000 > cpp-new.pkt 2>> obs.err
	printf 'abc' > cpp-pre.pkt && ./nexus -f packet -z 3 cppBig 2000000 >> cpp-pre.pkt 2>> obs.err
	tail -c +4 cpp-pre.pkt | cmp -s - cpp-new.pkt && echo SAME >> obs.txt || echo DIFFERENT >> obs.txt
	rm -f *.pkt

clean:
	rm -rf nexus pools __pycache__ *.err *.pkt
	rm -rf *-build *build.tmp*
