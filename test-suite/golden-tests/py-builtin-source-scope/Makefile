all:
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus count abc >> obs.txt 2> obs.err
	./nexus render 42 >> obs.txt 2>> obs.err
	./nexus shout abc >> obs.txt 2>> obs.err
	if timeout 60 ./nexus job 3 >> obs.txt 2> job.err; then echo UNEXPECTED >> obs.txt; elif grep -q "builtins' has no attribute 'run_job'" job.err; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi
	if timeout 60 ./nexus table 3 >> obs.txt 2> table.err; then echo UNEXPECTED >> obs.txt; elif grep -q "builtins' has no attribute 'dispatch'" table.err; then echo OK >> obs.txt; else echo UNEXPECTED >> obs.txt; fi

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
