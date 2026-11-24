all:
	: > obs.txt
	rm -f *.err s.pkt a.out
	morloc make -o nexus main.loc 2> build.err
	-./nexus -f packet -o s.pkt streamCpp 2>> obs.err
	echo "--- Cpp ---" >> obs.txt
	-./nexus streamCpp --all >> obs.txt 2>> obs.err
	-./nexus streamCpp --all=a.out --each >> obs.txt 2>> obs.err
	-cat a.out >> obs.txt
	-./nexus readCpp s.pkt >> obs.txt 2>> obs.err
	echo "--- Py ---" >> obs.txt
	-./nexus streamPy --all >> obs.txt 2>> obs.err
	-./nexus streamPy --all=a.out --each >> obs.txt 2>> obs.err
	-cat a.out >> obs.txt
	-./nexus readPy s.pkt >> obs.txt 2>> obs.err
	echo "--- R ---" >> obs.txt
	-./nexus streamR --all >> obs.txt 2>> obs.err
	-./nexus streamR --all=a.out --each >> obs.txt 2>> obs.err
	-cat a.out >> obs.txt
	-./nexus readR s.pkt >> obs.txt 2>> obs.err
	echo "--- Rust ---" >> obs.txt
	-./nexus streamRust --all >> obs.txt 2>> obs.err
	-./nexus streamRust --all=a.out --each >> obs.txt 2>> obs.err
	-cat a.out >> obs.txt
	-./nexus readRust s.pkt >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err s.pkt a.out
	rm -rf *-build *build.tmp*
