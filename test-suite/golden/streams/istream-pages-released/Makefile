all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus make 50 small.mlc 2>> obs.err
	./nexus make 500 large.mlc 2>> obs.err
	a=$$(./nexus drainPeak small.mlc 2>> obs.err); b=$$(./nexus drainPeak large.mlc 2>> obs.err); \
	  echo "pages-released=$$([ $$((b - a)) -lt 12000 ] && echo yes || echo "no ($$a -> $$b KiB)")" > obs.txt

clean:
	rm -rf nexus pools *.err *.mlc
	rm -rf *-build *build.tmp*
