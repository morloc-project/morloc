all:
	rm -f *.err obs.txt
	morloc make -o where_form where_form.loc 2> build.where.err
	morloc make -o ctor_form ctor_form.loc 2> build.ctor.err
	printf -- '--- where form ---\n' > obs.txt
	./where_form --json-help | python3 describe.py >> obs.txt 2>> obs.err
	printf -- '--- constructor form ---\n' >> obs.txt
	./ctor_form --json-help | python3 describe.py >> obs.txt 2>> obs.err

clean:
	rm -rf where_form ctor_form pools log __pycache__ *.err
	rm -rf *-build *build.tmp*
