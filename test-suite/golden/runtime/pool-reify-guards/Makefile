all:
	rm -f *.err obs.txt
	python3 check.py ../../../../binders/py/pool.py > obs.txt 2> obs.err
	Rscript check.R ../../../../binders/r/pool.R >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
