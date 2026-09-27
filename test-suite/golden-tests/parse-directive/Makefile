all:
	rm -rf *.err ran.txt s.mlc tmp && mkdir tmp
	: > obs.txt
	cp n.txt N.TXT.GZ && cp a.csv UP.CSV && printf '[3,4]\n' > 'a:b.json'
	morloc make -o nexus main.loc 2> build.err
	./nexus writeStream s.mlc >> obs.txt 2>> obs.err
	./nexus total csv:a.csv >> obs.txt 2>> obs.err
	./nexus total pycsv:b.pcsv >> obs.txt 2>> obs.err
	./nexus total a.csv >> obs.txt 2>> obs.err
	./nexus total b.pcsv >> obs.txt 2>> obs.err
	./nexus total UP.CSV >> obs.txt 2>> obs.err
	./nexus total '[4,5]' >> obs.txt 2>> obs.err
	./nexus total morloc:j.csv >> obs.txt 2>> obs.err
	./nexus total a:b.json >> obs.txt 2>> obs.err
	./nexus sizes n.txt >> obs.txt 2>> obs.err
	./nexus sizes N.TXT.GZ >> obs.txt 2>> obs.err
	./nexus sizes pylines:n.txt >> obs.txt 2>> obs.err
	cat n.txt | ./nexus sizes lines:- >> obs.txt 2>> obs.err
	./nexus sizes s.mlc >> obs.txt 2>> obs.err
	./nexus sizesPy n.txt >> obs.txt 2>> obs.err
	./nexus pair a.csv n.txt >> obs.txt 2>> obs.err
	./nexus pair '[1]' n.txt >> obs.txt 2>> obs.err
	./nexus pair a.csv s.mlc >> obs.txt 2>> obs.err
	./nexus pair '[1]' s.mlc >> obs.txt 2>> obs.err
	./nexus shown a.csv >> obs.txt 2>> obs.err
	./nexus shown -p a.csv >> obs.txt 2>> obs.err
	./nexus -f json shown a.csv >> obs.txt 2>> obs.err
	./nexus extra 1 >> obs.txt 2>> obs.err
	./nexus extra -x a.csv 1 >> obs.txt 2>> obs.err
	./nexus extra -x '[5]' 1 >> obs.txt 2>> obs.err
	./nexus firstOf n.txt >> obs.txt 2>> obs.err
	./nexus recorded ran.txt bad.csv 2>> obs.txt || echo "exit $$?" >> obs.txt
	if [ -e ran.txt ]; then echo "body ran" >> obs.txt; else echo "body not run" >> obs.txt; fi
	./nexus shaped a.csv names.txt title.txt >> obs.txt 2>> obs.err
	./nexus shaped '[1]' names.txt title.txt >> obs.txt 2>> obs.err
	cat n.txt | ./nexus twoStdin lines:- x - 2>> obs.txt || echo "exit $$?" >> obs.txt
	env -u MORLOC_TMPDIR TMPDIR=$$PWD/tmp ./nexus sizes n.txt >> obs.txt 2>> obs.err
	env -u MORLOC_TMPDIR TMPDIR=$$PWD/tmp ./nexus sizes missing.txt 2>> obs.err || echo "missing file rejected" >> obs.txt
	ls tmp | wc -l >> obs.txt
	./nexus total -h >> obs.txt 2>> obs.err
	morloc make -o purenexus pure.loc 2> pure.err && echo UNEXPECTED_SUCCESS >> obs.txt || echo build_rejected >> obs.txt

clean:
	rm -rf nexus purenexus pools __pycache__ *.err ran.txt s.mlc tmp N.TXT.GZ UP.CSV 'a:b.json'
	rm -rf *-build *build.tmp*
