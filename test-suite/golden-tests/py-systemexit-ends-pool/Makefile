all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus pyExit 1 2> exit.err ; echo "exit:$$?" >> obs.txt
	grep -oE "Pool process exited with status [0-9]+" exit.err | head -1 >> obs.txt
	MORLOC_PY_POOL=thread ./nexus pyExit 1 2> thread.err ; echo "exit:$$?" >> obs.txt
	grep -oE "Pool process exited with status [0-9]+" thread.err | head -1 >> obs.txt
	cat exit.err thread.err > obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
