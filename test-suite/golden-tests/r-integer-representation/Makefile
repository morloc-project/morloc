all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus mapU64 '[5,7,9]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus mapU64 '[0,5]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus mapI64 '[-5,7]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus mapLitU64 '[0,5]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus mapLitU64 '[5,0]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus mapLitI64 '[0,-5]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus mapInt '[1,2]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus eqI64 7 >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus eqU64 7 >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus sortI64 '[3,-1,9007199254740993,-9007199254740993,0]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus negI64 '[5,-7]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus bigLit 1 >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus narrowI32 7 >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus narrowU8 1 > /dev/null 2>> obs.err && echo accepted >> obs.txt || echo rejected >> obs.txt
	./nexus truncI32 5 >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus truncI32 -5 >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus negU64 1 > /dev/null 2>> obs.err && echo accepted >> obs.txt || echo rejected >> obs.txt
	./nexus idU64 9223372036854775808 > /dev/null 2>> obs.err && echo accepted >> obs.txt || echo rejected >> obs.txt
	./nexus idU64 9223372036854775807 >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus idInt 2147483647 >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus idInt -2147483648 > /dev/null 2>> obs.err && echo accepted >> obs.txt || echo rejected >> obs.txt
	./nexus idInt 2147483648 > /dev/null 2>> obs.err && echo accepted >> obs.txt || echo rejected >> obs.txt
	./nexus litI64 '[1,-1]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus litU64 '[1,2]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus litInt '[1,2]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus litI32 '[1,2]' >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus tryU64 9007199254740993 >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus tryInt -2147483647 >> obs.txt 2>> obs.err || echo error >> obs.txt
	./nexus tryInt -2147483648 > /dev/null 2>> obs.err && echo accepted >> obs.txt || echo rejected >> obs.txt
	./nexus intoInt -2147483648 > /dev/null 2>> obs.err && echo accepted >> obs.txt || echo rejected >> obs.txt
	grep -c "qualified with L" obs.err >> obs.txt || true

clean:
	rm -rf nexus pools
	rm -rf *-build *build.tmp*
	rm -f *.err
