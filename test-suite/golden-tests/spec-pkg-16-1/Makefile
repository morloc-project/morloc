all:
	rm -f *.err obs.txt
	cd missing && env -u MORLOC_ENV -u MORLOC_BUILD_HOOK -u MORLOC_STRICT_CONDA morloc make -o nexus main.loc 2> ../missing.err && echo UNEXPECTED_SUCCESS >> ../obs.txt || (grep -q "tool dependency 'q'.*no_such_tool_q" ../missing.err && echo REFUSED) >> ../obs.txt
	cd present && env -u MORLOC_ENV -u MORLOC_BUILD_HOOK -u MORLOC_STRICT_CONDA morloc make -o nexus main.loc 2> ../build.err
	present/nexus one 1 >> obs.txt 2>> obs.err

clean:
	rm -rf missing/nexus present/nexus missing/*-build present/*-build missing/*build.tmp* present/*build.tmp* missing/__pycache__ present/__pycache__ *.err
	rm -rf *-build *build.tmp*
