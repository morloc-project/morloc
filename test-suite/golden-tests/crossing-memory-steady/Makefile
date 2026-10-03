all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	g=$$(./nexus steady 4000 2>> obs.err); \
	echo "holds-steady=$$([ "$$g" -ge 0 ] && [ "$$g" -le 512 ] && echo yes || echo "no (grew $$g KiB)")" > obs.txt

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
