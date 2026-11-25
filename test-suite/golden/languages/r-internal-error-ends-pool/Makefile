all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus tryAbort 1 > out.txt 2> obs.err ; echo "exit:$$?" >> obs.txt
	cat out.txt >> obs.txt ; rm -f out.txt
	grep -c "a runtime invariant failed" obs.err >> obs.txt
	grep -oE "Pool process exited with status [0-9]+" obs.err | head -1 >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err out.txt
	rm -rf *-build *build.tmp*
