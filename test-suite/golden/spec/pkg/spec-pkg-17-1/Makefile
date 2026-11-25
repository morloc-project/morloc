all:
	rm -f *.err obs.txt
	env -u MORLOC_STRICT_CONDA MORLOC_ENV=t MORLOC_BUILD_HOOK=true CONDA_PREFIX=$(CURDIR)/fake morloc make -o managed main.loc 2> build.err
	./managed runTool 1 >> obs.txt 2>> obs.err
	env -u MORLOC_ENV -u MORLOC_BUILD_HOOK -u MORLOC_STRICT_CONDA morloc make -o plain main.loc 2>> build.err
	./plain runTool 1 >> obs.txt 2>> obs.err

clean:
	rm -rf managed plain __pycache__ *.err
	rm -rf *-build *build.tmp*
