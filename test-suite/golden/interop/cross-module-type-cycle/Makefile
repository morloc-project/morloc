all:
	rm -f *.err obs.txt
	-timeout 10s morloc make -o nexus main.loc 2> build.err
	grep -c "Import cycle" build.err > obs.txt || echo 0 > obs.txt

clean:
	rm -rf nexus pools *err
	rm -rf *-build *build.tmp*
