all:
	rm -f *.err obs.txt
	python3 check.py ../../../data/lang/py/pool.py > obs.txt 2> obs.err
	Rscript check.R ../../../data/lang/r/pool.R >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
