all:
	rm -f *.err obs.txt
	rm -rf main pools
	# A positional argument with a `default:` but no `arg:` flag must fail
	# at `morloc make` with an actionable message, not silently drop the
	# default and leave the argument required.
	morloc make main.loc > obs.txt 2>&1 || true
	grep -oF 'cannot take defaults' obs.txt > obs.txt.tmp
	mv obs.txt.tmp obs.txt

clean:
	rm -f *.err
	rm -rf main pools __pycache__
