all:
	rm -f *.err obs.txt
	rm -rf main pools
	morloc make main.loc > obs.txt 2>&1 || true
	grep -oF 'and to a field inside it' obs.txt > obs.txt.tmp
	mv obs.txt.tmp obs.txt

clean:
	rm -f *.err
	rm -rf main pools __pycache__
	rm -rf *-build *build.tmp*
