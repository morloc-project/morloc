all:
	rm -f *.err obs.txt
	morloc make -o sh main.loc 2> build.err
	printf -- '--- source file, dash ---\n' > obs.txt
	printf 'hello from stdin\n' | ./sh readIt - >> obs.txt 2>> obs.err
	printf -- '--- source file, /dev/stdin ---\n' >> obs.txt
	printf 'hello again\n' | ./sh readIt /dev/stdin >> obs.txt 2>> obs.err
	printf -- '--- source file, real path ---\n' >> obs.txt
	printf 'from a file\n' > payload.txt
	./sh readIt payload.txt >> obs.txt 2>> obs.err
	printf -- '--- check.path, dash ---\n' >> obs.txt
	printf 'x\n' | ./sh pathIt - >> obs.txt 2>> obs.err
	printf -- '--- check.path, real path ---\n' >> obs.txt
	./sh pathIt payload.txt >> obs.txt 2>> obs.err

clean:
	rm -rf sh pools log __pycache__ *.err payload.txt
	rm -rf *-build *build.tmp*
