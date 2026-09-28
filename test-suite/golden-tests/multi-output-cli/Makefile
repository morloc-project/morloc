all:
	: > obs.txt
	rm -f *.err *.out
	morloc make -o nexus main.loc 2> build.err
	./nexus 3 --csv=a.out >> obs.txt 2>> obs.err
	cat a.out >> obs.txt
	./nexus -f json @ 3 --csv=b.out >> obs.txt 2>> obs.err
	./nexus 3 --csv=c.out --no-stdout >> obs.txt 2>> obs.err
	cat c.out >> obs.txt
	./nexus -o o.out @ 3 --csv=d.out --no-stdout >> obs.txt 2>> obs.err
	wc -c < o.out >> obs.txt
	./nexus 3 --no-stdout >> obs.txt 2>> obs.err
	./nexus --csv 3 >> obs.txt 2>> obs.err
	./nexus 3 --table=t.out >> obs.txt 2>> obs.err
	cat t.out >> obs.txt
	if ./nexus 3 --csv --table 2>> obs.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi
	if ./nexus 3 --csv --no-stdout 2>> obs.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi
	if ./nexus 3 --csv=x.out --table=./x.out 2>> obs.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi
	if ./nexus -o y.out @ 3 --csv=y.out 2>> obs.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi
	if ./nexus 3 --csv= 2>> obs.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi
	echo keep > e.out
	if ./nexus 3 --csv=e.out --boom=f.out 2>> obs.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi
	cat e.out >> obs.txt
	ls -A | grep -c -e '^f.out$$' -e 'morloc-' >> obs.txt || true
	./nexus -h | grep -c -e '--csv\[=<PATH>\]' -e '--no-stdout' >> obs.txt
	echo old > g.out
	chmod 600 g.out
	./nexus 3 --csv=g.out --no-stdout 2>> obs.err
	stat -c %a g.out >> obs.txt
	cat g.out >> obs.txt
	ln -sf h.out i.out
	if ./nexus 3 --csv=h.out --table=i.out 2>> obs.err ; then echo UNEXPECTED >> obs.txt ; else echo OK >> obs.txt ; fi
	ln -sf j.out k.out
	./nexus 3 --csv=k.out --no-stdout 2>> obs.err
	test -L k.out && cat j.out >> obs.txt
	if ./nexus 3 --csv=/dev/null --no-stdout 2>> obs.err ; then echo OK >> obs.txt ; else echo UNEXPECTED >> obs.txt ; fi

clean:
	rm -rf nexus pools __pycache__ *.err *.out
	rm -rf *-build *build.tmp*
