all:
	rm -f *.err obs.txt
	morloc make -o bad-let bad-let.loc 2> build-let.err && echo "UNEXPECTED_SUCCESS let" >> obs.txt || (grep -q "discards qualifiers" build-let.err && echo "rejected let" || echo "UNEXPECTED_ERROR let") >> obs.txt
	morloc make -o bad-getter bad-getter.loc 2> build-getter.err && echo "UNEXPECTED_SUCCESS getter" >> obs.txt || (grep -q "discards qualifiers" build-getter.err && echo "rejected getter" || echo "UNEXPECTED_ERROR getter") >> obs.txt
	morloc make -o bad-param bad-param.loc 2> build-param.err && echo "UNEXPECTED_SUCCESS param" >> obs.txt || (grep -q "discards qualifiers" build-param.err && echo "rejected param" || echo "UNEXPECTED_ERROR param") >> obs.txt
	morloc make -o bad-op bad-op.loc 2> build-op.err && echo "UNEXPECTED_SUCCESS op" >> obs.txt || (grep -q "no match for .operator+" build-op.err && echo "rejected op" || echo "UNEXPECTED_ERROR op") >> obs.txt
	morloc make -o bad-hof bad-hof.loc 2> build-hof.err && echo "UNEXPECTED_SUCCESS hof" >> obs.txt || (grep -q "discards qualifiers" build-hof.err && echo "rejected hof" || echo "UNEXPECTED_ERROR hof") >> obs.txt
	morloc make -o bad-newtype bad-newtype.loc 2> build-newtype.err && echo "UNEXPECTED_SUCCESS newtype" >> obs.txt || (grep -q "discards qualifiers" build-newtype.err && echo "rejected newtype" || echo "UNEXPECTED_ERROR newtype") >> obs.txt
	morloc make -o bad-mixed bad-mixed.loc 2> build-mixed.err && echo "UNEXPECTED_SUCCESS mixed" >> obs.txt || (grep -q "discards qualifiers" build-mixed.err && echo "rejected mixed" || echo "UNEXPECTED_ERROR mixed") >> obs.txt
	morloc make -o bad-sortby bad-sortby.loc 2> build-sortby.err && echo "UNEXPECTED_SUCCESS sortby" >> obs.txt || (grep -q "discards qualifiers" build-sortby.err && echo "rejected sortby" || echo "UNEXPECTED_ERROR sortby") >> obs.txt
	morloc make -o bad-fn bad-fn.loc 2> build-fn.err && echo "UNEXPECTED_SUCCESS fn" >> obs.txt || (grep -q "discards qualifiers" build-fn.err && echo "rejected fn" || echo "UNEXPECTED_ERROR fn") >> obs.txt
	morloc make -o nexus ok.loc 2> build.err
	./nexus letConst 5 >> obs.txt 2>> obs.err
	./nexus forward 5 >> obs.txt 2>> obs.err
	./nexus overload 5 >> obs.txt 2>> obs.err
	./nexus literals 5 >> obs.txt 2>> obs.err
	./nexus getter 5 >> obs.txt 2>> obs.err
	./nexus loop 5 >> obs.txt 2>> obs.err
	./nexus param 5 >> obs.txt 2>> obs.err
	./nexus op 5 >> obs.txt 2>> obs.err
	./nexus lambda 5 >> obs.txt 2>> obs.err
	./nexus folded 5 >> obs.txt 2>> obs.err
	./nexus sorted 5 >> obs.txt 2>> obs.err
	./nexus newtyped 5 >> obs.txt 2>> obs.err
	./nexus mixedOp 5 >> obs.txt 2>> obs.err
	./nexus fnConst 5 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus bad-let bad-getter bad-param bad-op bad-hof bad-newtype bad-mixed bad-sortby bad-fn pools __pycache__
	rm -f *.err
	rm -rf *-build *build.tmp*
