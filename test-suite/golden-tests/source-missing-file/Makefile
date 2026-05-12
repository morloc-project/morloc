all:
	rm -f *.err obs.txt
	rm -rf main pools
	# C20: `source X from "missing.py"` must fail at morloc make,
	# not at pool runtime with a confusing ImportError.
	morloc make main.loc > obs.txt 2>&1 || true
	grep -F 'Source file not found' obs.txt > obs.txt.tmp
	mv obs.txt.tmp obs.txt

clean:
	rm -f *.err
	rm -rf main pools
