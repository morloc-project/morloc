all:
	: > obs.txt
	rm -f *.err
	if morloc make -o nexus main.loc 2> build.err; then \
	  ./nexus localOk 5 >> obs.txt 2>> obs.err; \
	  if ./nexus crossesPools 5 >> obs.txt 2>> obs.err; then \
	    echo "UNEXPECTED: a host-created closure crossed a pool" >> obs.txt; \
	  else \
	    echo "OK refused at run time" >> obs.txt; \
	  fi; \
	else \
	  echo "OK refused at build time" >> obs.txt; \
	fi

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
