all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus listU8 > obs.txt 2> obs.err
	./nexus listDir >> obs.txt 2>> obs.err
	./nexus vecU8 >> obs.txt 2>> obs.err
	./nexus vecDir >> obs.txt 2>> obs.err
	./nexus i64 >> obs.txt 2>> obs.err
	./nexus u64Wide >> obs.txt 2>> obs.err
	./nexus vecDirElems >> obs.txt 2>> obs.err
	./nexus vecDirAt >> obs.txt 2>> obs.err
	./nexus recvVecDir >> obs.txt 2>> obs.err
	./nexus recvListDir >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err
	rm -rf *-build *build.tmp*
