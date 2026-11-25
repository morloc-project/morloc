all:
	rm -f *.err obs.txt ok.csv plus.csv i8.csv intdot.csv NaN.csv dot5.csv huge.csv yes.csv enumcase.csv space.csv f32.csv bigtable.csv
	morloc make -o nexus main.loc 2> build.err
	printf 'b,i,n,r,f,e,s\nTRUE,-128,12345678901234567890123,1e+300,0.1,Minus, x \nfalse,127,-0042,nan,inf,Plus,""\n' > ok.csv
	printf 'b,i,n,r,f,e,s\nTRUE,+1,1,1,1,Plus,a\n' > plus.csv
	printf 'b,i,n,r,f,e,s\nTRUE,128,1,1,1,Plus,a\n' > i8.csv
	printf 'b,i,n,r,f,e,s\nTRUE,1,1.0,1,1,Plus,a\n' > intdot.csv
	printf 'b,i,n,r,f,e,s\nTRUE,1,1,NaN,1,Plus,a\n' > NaN.csv
	printf 'b,i,n,r,f,e,s\nTRUE,1,1,.5,1,Plus,a\n' > dot5.csv
	printf 'b,i,n,r,f,e,s\nTRUE,1,1,1e999,1,Plus,a\n' > huge.csv
	printf 'b,i,n,r,f,e,s\nyes,1,1,1,1,Plus,a\n' > yes.csv
	printf 'b,i,n,r,f,e,s\nTRUE,1,1,1,1,plus,a\n' > enumcase.csv
	printf 'b,i,n,r,f,e,s\nTRUE, 1,1,1,1,Plus,a\n' > space.csv
	printf 'b,i,n,r,f,e,s\nTRUE,1,1,1,1e39,Plus,a\n' > f32.csv
	printf 'x,y\n99999999999999999999,a\n' > bigtable.csv
	./nexus rows ok.csv >> obs.txt 2>> obs.err
	if ./nexus rows plus.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows i8.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows intdot.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows NaN.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows dot5.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows huge.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows yes.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows enumcase.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows space.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus rows f32.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi
	if ./nexus table bigtable.csv > /dev/null 2>> obs.err; then echo UNEXPECTED >> obs.txt; else echo OK >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ ok.csv plus.csv i8.csv intdot.csv NaN.csv dot5.csv huge.csv yes.csv enumcase.csv space.csv f32.csv bigtable.csv
	rm -f *.err
	rm -rf *-build *build.tmp*
