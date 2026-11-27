all:
	rm -f *.err obs.txt
	morloc typecheck main.loc > obs.txt 2>&1 || true

clean:
	rm -f *.err
	rm -rf *-build *build.tmp*
