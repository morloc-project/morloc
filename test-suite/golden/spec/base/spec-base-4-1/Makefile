all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus cppGood 1 >> obs.txt 2>> obs.err
	./nexus cppBad 1 >> obs.txt 2> bad.err && echo UNEXPECTED >> obs.txt || echo OK >> obs.txt
	grep -q "C++.*not valid UTF-8.*byte 2" bad.err && echo NAMED >> obs.txt || echo UNNAMED >> obs.txt
	./nexus cppBadToPy 1 >> obs.txt 2> topy.err && echo UNEXPECTED >> obs.txt || echo OK >> obs.txt
	grep -q "C++.*not valid UTF-8" topy.err && echo NAMED >> obs.txt || echo UNNAMED >> obs.txt
	./nexus rBad 1 >> obs.txt 2> rbad.err && echo UNEXPECTED >> obs.txt || echo OK >> obs.txt
	grep -q "R.*not valid UTF-8.*byte 2" rbad.err && echo NAMED >> obs.txt || echo UNNAMED >> obs.txt
	./nexus rLatin 1 >> obs.txt 2>> obs.err
	./nexus rEuro 1 >> obs.txt 2>> obs.err
	./nexus rC1 1 >> obs.txt 2>> obs.err
	./nexus pyBytes 1 >> obs.txt 2> pybytes.err && echo UNEXPECTED >> obs.txt || echo OK >> obs.txt
	grep -q "Python.*not valid UTF-8.*byte 2" pybytes.err && echo NAMED >> obs.txt || echo UNNAMED >> obs.txt
	./nexus rBytes 1 >> obs.txt 2>> obs.err
	./nexus echo "$$(printf 'a\377b')" >> obs.txt 2> argv.err && echo UNEXPECTED >> obs.txt || echo OK >> obs.txt
	grep -q "an argument is not valid UTF-8" argv.err && echo NAMED >> obs.txt || echo UNNAMED >> obs.txt
	MORLOC_SKIP_UTF8_CHECK=1 ./nexus cppBadToPy 1 >> obs.txt 2> skipenv.err && echo UNEXPECTED >> obs.txt || echo OK >> obs.txt
	grep -q "C++ produced" skipenv.err && echo CHECKED >> obs.txt || echo SKIPPED >> obs.txt
	morloc make --unsafe-skip-utf8-check -o skipped main.loc 2>> build.err
	./skipped cppBadToPy 1 >> obs.txt 2> skipflag.err && echo UNEXPECTED >> obs.txt || echo OK >> obs.txt
	grep -q "C++ produced" skipflag.err && echo CHECKED >> obs.txt || echo SKIPPED >> obs.txt

clean:
	rm -rf nexus skipped pools __pycache__ *.err
	rm -rf *-build *build.tmp*
