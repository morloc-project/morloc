define run_lang
	echo "--- $(1) ---" >> obs.txt
	rm -f log.txt *.out
	./$(1) squares log.txt 4 3 --lines >> obs.txt 2>> obs.err
	./$(1) squares log.txt 4 3 --pad >> obs.txt 2>> obs.err
	./$(1) squares log.txt 4 3 --sink >> obs.txt 2>> obs.err
	: > log.txt
	./$(1) squares log.txt 4 3 --lines=l.out --pad=p.out --sink=s.out --summary >> obs.txt 2>> obs.err
	cat log.txt l.out p.out s.out >> obs.txt
	./$(1) noisy log.txt 2 --lines >> obs.txt 2>> obs.err
	./$(1) noisy log.txt 2 --lines=n1.out --summary >> obs.txt 2>> obs.err
	./$(1) noisy log.txt 2 --lines=n2.out --no-stdout >> obs.txt 2>> obs.err
	cat n1.out n2.out >> obs.txt
endef

all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus-py main-py.loc 2> build-py.err
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	morloc make -o nexus-r main-r.loc 2> build-r.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	$(call run_lang,nexus-py)
	$(call run_lang,nexus-cpp)
	$(call run_lang,nexus-r)
	$(call run_lang,nexus-rust)

clean:
	rm -rf nexus-py nexus-cpp nexus-r nexus-rust pools __pycache__ *.err *.out log.txt
	rm -rf *-build *build.tmp*
