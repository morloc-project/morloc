all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	printf -- '--- fold: batch boundaries preserved ---\n' >> obs.txt
	./nexus stream --folded >> obs.txt 2>> obs.err
	printf -- '--- gather: same producer, whole list ---\n' >> obs.txt
	./nexus stream --all >> obs.txt 2>> obs.err
	printf -- '--- fold: four threads, one accumulator each ---\n' >> obs.txt
	./nexus pstream --parallel >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
