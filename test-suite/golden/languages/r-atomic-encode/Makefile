all:
	rm -f *.err obs.txt *.dat
	morloc make -o nexus main.loc 2> build.err
	echo "--- Bool ---" >> obs.txt
	./nexus writeBool Bool.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus backBool Bool.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus lglsBool 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus lglsBool 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	echo "--- I8 ---" >> obs.txt
	./nexus writeI8 I8.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus backI8 I8.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsI8 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsI8 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsI8 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsI8 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsI8 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsI8 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	echo "--- I16 ---" >> obs.txt
	./nexus writeI16 I16.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus backI16 I16.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsI16 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsI16 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsI16 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsI16 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsI16 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsI16 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	echo "--- I32 ---" >> obs.txt
	./nexus writeI32 I32.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus backI32 I32.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsI32 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsI32 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsI32 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsI32 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsI32 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsI32 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	echo "--- I64 ---" >> obs.txt
	./nexus writeI64 I64.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus backI64 I64.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsI64 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsI64 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsI64 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsI64 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsI64 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsI64 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	echo "--- U8 ---" >> obs.txt
	./nexus writeU8 U8.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus backU8 U8.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsU8 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsU8 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsU8 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsU8 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsU8 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsU8 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	echo "--- U16 ---" >> obs.txt
	./nexus writeU16 U16.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus backU16 U16.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsU16 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsU16 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsU16 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsU16 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsU16 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsU16 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	echo "--- U32 ---" >> obs.txt
	./nexus writeU32 U32.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus backU32 U32.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsU32 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsU32 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsU32 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsU32 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsU32 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsU32 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	echo "--- U64 ---" >> obs.txt
	./nexus writeU64 U64.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus backU64 U64.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsU64 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsU64 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsU64 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsU64 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsU64 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsU64 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	echo "--- Int ---" >> obs.txt
	./nexus writeInt Int.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus backInt Int.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsInt 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsInt 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsInt 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsInt 1 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsInt 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsInt 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	echo "--- F32 ---" >> obs.txt
	./nexus writeF32 F32.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus backF32 F32.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsF32 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsF32 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsF32 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsF32 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	echo "--- F64 ---" >> obs.txt
	./nexus writeF64 F64.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus backF64 F64.dat >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsF64 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsF64 0 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus intsF64 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt
	./nexus dblsF64 2 >> obs.txt 2>> obs.err || echo rejected >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err *.dat
	rm -rf *-build *build.tmp*
