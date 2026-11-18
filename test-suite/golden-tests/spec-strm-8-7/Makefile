all:
	: > obs.txt
	rm -f *.err s.pkt z.pkt a.out
	morloc make -o nexus main.loc 2> build.err
	./nexus levels >> obs.txt 2>> obs.err
	./nexus -f jsonl levels >> obs.txt 2>> obs.err
	./nexus -f packet -o s.pkt levels 2>> obs.err
	./nexus total s.pkt >> obs.txt 2>> obs.err
	./nexus -f packet -z 0 -o z.pkt levels 2>> obs.err
	./nexus total z.pkt >> obs.txt 2>> obs.err
	./nexus levels --all >> obs.txt 2>> obs.err
	./nexus levels --all=a.out --no-stdout >> obs.txt 2>> obs.err
	cat a.out >> obs.txt
	./nexus viaHelper >> obs.txt 2>> obs.err
	./nexus viaHelper -h 2>> obs.err | grep '^Return' >> obs.txt
	if ./nexus closes > /dev/null 2>> obs.err; then echo "closes: exit 0" >> obs.txt; else echo "closes: failed" >> obs.txt; fi

clean:
	rm -rf nexus pools log __pycache__ *.err s.pkt z.pkt a.out
	rm -rf *-build *build.tmp*
