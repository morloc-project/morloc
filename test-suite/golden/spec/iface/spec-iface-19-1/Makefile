all:
	rm -f *.err *.out obs.txt
	morloc make -o nexus main.loc 2> build.err
	printf '%s\n' "$$(./nexus note 'say "hi"' 2>> obs.err)" >> obs.txt
	./nexus -f json note 'say "hi"' >> obs.txt 2>> obs.err
	printf '%s\n' "$$(./nexus tagged 'say "hi"' --note 2>> obs.err)" >> obs.txt
	./nexus tagged 'say "hi"' >> obs.txt 2>> obs.err
	./nexus tagged 'say "hi"' --note=n.out 2>> obs.err && printf '%s\n' "$$(cat n.out)" >> obs.txt
	printf '%s\n' "$$(./nexus note 'say "hi"' --up=u.out 2>> obs.err)" >> obs.txt

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err *.out
