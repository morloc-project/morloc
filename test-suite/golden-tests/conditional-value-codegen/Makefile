all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus classify true 5 8             >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus classify false 5 8            >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus classifyAll true 5 '[3,5,8]'  >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus classifyAll false 5 '[3,5,8]' >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus validate 42     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus validate 200    >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus validate -- -3  >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus validateCpp 42     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus validateCpp 200    >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus validateCpp -- -3  >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus purePick true 1 2  >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus purePick false 1 2 >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus pureLit true       >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus pureLit false      >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus pureThrow true     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus pureThrow false    >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus tupArm 5     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus tupArm 200   >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus listArm 5    >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus listArm 200  >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus recArm 5     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus recArm 200   >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus litArm false >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus litArm true  >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus strArm false >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus strArm true  >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus optArm 5      >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus optArm 200    >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus optArmCpp 5   >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus optArmCpp 200 >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus nvArm 0 1     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus nvArm 0 0     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus nvArm 1 0     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus letArm 5       >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus letArm 200     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus letArmCpp 5    >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus letArmCpp 200  >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus validateR 42   >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus validateR 200  >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus optArmR 5      >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus optArmR 200    >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus nvArmR 0 1     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus nvArmR 0 0     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus nvArmR 1 0     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus letArmR 5      >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus letArmR 200    >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus listArmR 5     >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus listArmR 200   >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus recArmR 5      >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus recArmR 200    >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	morloc make -o nx_vf    bug_value_first.loc 2> bug_vf.err    && echo "value-first-cpp-build:ok" >> obs.txt || echo "value-first-cpp-build:FAILED" >> obs.txt
	morloc make -o nx_nest  bug_nested.loc      2> bug_nest.err  && echo "nested-cpp-build:ok"      >> obs.txt || echo "nested-cpp-build:FAILED"      >> obs.txt
	morloc typecheck --realize bug_nested.loc > /dev/null 2> bug_realize.err && echo "realize-nested:ok" >> obs.txt || echo "realize-nested:CRASH" >> obs.txt
	morloc make -o nx_opt bug_opt_arm.loc      2> bug_opt.err && echo "optional-arm-build:ok" >> obs.txt || echo "optional-arm-build:FAILED" >> obs.txt
	morloc make -o nx_nva bug_nest_val_arm.loc 2> bug_nva.err && echo "nest-val-arm-build:ok" >> obs.txt || echo "nest-val-arm-build:FAILED" >> obs.txt

clean:
	rm -rf nexus nx_vf nx_nest nx_opt nx_nva pools __pycache__ *.err
