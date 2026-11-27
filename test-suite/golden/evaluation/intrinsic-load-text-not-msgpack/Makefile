all:
	rm -f *.err obs.txt a.txt b.txt c.txt
	morloc make -o nexus main.loc 2> build.err
	printf '1.5' > a.txt ; printf '5x' > b.txt ; printf '42' > c.txt
	for f in a.txt b.txt c.txt ; do ./nexus loadInt $$f >> obs.txt 2>> obs.err ; done
	rm -f a.txt b.txt c.txt

clean:
	rm -rf nexus pools __pycache__ *.err a.txt b.txt c.txt
	rm -rf *-build *build.tmp*
