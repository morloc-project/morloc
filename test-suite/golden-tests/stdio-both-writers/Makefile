SHELL := /bin/bash

all:
	rm -f *.err *.tmp obs.txt obs.err
	morloc make -o nexus main.loc 2> build.err
	# dataAndLog: @stdout and @stderr each carry their own stream
	# file. Independent SHM claim slots per stdio kind + independent
	# nexus-side mutexes.
	./nexus dataAndLog > data.tmp 2> log.tmp
	@echo "=== data_nonempty ===" >> obs.txt
	@if [ -s data.tmp ]; then echo "yes" >> obs.txt; else echo "no" >> obs.txt; fi
	@echo "=== log_nonempty ===" >> obs.txt
	@if [ -s log.tmp ]; then echo "yes" >> obs.txt; else echo "no" >> obs.txt; fi
	@rm -f *.tmp

clean:
	rm -rf nexus pools __pycache__ *.err *.tmp
