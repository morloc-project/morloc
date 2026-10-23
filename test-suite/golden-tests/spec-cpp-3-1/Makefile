all:
	rm -f *.err obs.txt
	morloc make -o bad-view bad-view.loc 2> build-view.err && echo "UNEXPECTED_SUCCESS view" >> obs.txt || (grep -q "CPP-3" build-view.err && echo "rejected view" || echo "UNEXPECTED_ERROR view") >> obs.txt
	morloc make -o bad-ptr bad-ptr.loc 2> build-ptr.err && echo "UNEXPECTED_SUCCESS ptr" >> obs.txt || (grep -q "CPP-3" build-ptr.err && echo "rejected ptr" || echo "UNEXPECTED_ERROR ptr") >> obs.txt
	morloc make -o bad-weak bad-weak.loc 2> build-weak.err && echo "UNEXPECTED_SUCCESS weak" >> obs.txt || (grep -q "CPP-3" build-weak.err && echo "rejected weak" || echo "UNEXPECTED_ERROR weak") >> obs.txt
	morloc make -o bad-shared bad-shared.loc 2> build-shared.err && echo "UNEXPECTED_SUCCESS shared" >> obs.txt || (grep -q "CPP-3" build-shared.err && echo "rejected shared" || echo "UNEXPECTED_ERROR shared") >> obs.txt
	morloc make -o bad-field bad-field.loc 2> build-field.err && echo "UNEXPECTED_SUCCESS field" >> obs.txt || (grep -q "CPP-3" build-field.err && echo "rejected field" || echo "UNEXPECTED_ERROR field") >> obs.txt
	morloc make -o bad-fnonly bad-fnonly.loc 2> build-fnonly.err && echo "UNEXPECTED_SUCCESS fnonly" >> obs.txt || (grep -q "CPP-3" build-fnonly.err && echo "rejected fnonly" || echo "UNEXPECTED_ERROR fnonly") >> obs.txt
	morloc make -o nexus ok.loc 2> build.err
	./nexus names 3 >> obs.txt 2>> obs.err
	./nexus shared 3 >> obs.txt 2>> obs.err
	./nexus docs 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus bad-view bad-ptr bad-weak bad-shared bad-field bad-fnonly pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
