all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	echo "--- cpp ---" >> obs.txt
	./nexus-cpp roundWrap 3 >> obs.txt 2>> obs.err
	./nexus-cpp roundRec 4 >> obs.txt 2>> obs.err
	./nexus-cpp roundPlain 5 >> obs.txt 2>> obs.err
	rm -rf types-cpp
	morloc types main-cpp.loc -o types-cpp --lang cpp 2> types-cpp.err
	grep -o 'not written.*: .*' types-cpp.err >> obs.txt
	grep -rhoE 'struct (Wrap|Rec|Plain)\b' types-cpp | sort >> obs.txt
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	echo "--- rust ---" >> obs.txt
	./nexus-rust roundWrap 3 >> obs.txt 2>> obs.err
	./nexus-rust roundRec 4 >> obs.txt 2>> obs.err
	./nexus-rust roundPlain 5 >> obs.txt 2>> obs.err
	rm -rf types-rust
	morloc types main-rust.loc -o types-rust --lang rust 2> types-rust.err
	grep -o 'not written.*: .*' types-rust.err >> obs.txt
	grep -rhoE 'struct (Wrap|Rec|Plain)\b' types-rust | sort >> obs.txt

clean:
	rm -rf nexus-cpp nexus-rust types-cpp types-rust pools __pycache__ *err *-build *build.tmp*
