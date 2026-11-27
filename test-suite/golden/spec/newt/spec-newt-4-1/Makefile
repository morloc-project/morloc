all:
	rm -f *.err obs.txt
	if morloc make -o nexus main.loc 2> build.err; then echo "UNEXPECTED_SUCCESS"; elif grep -q "pools/cpp/" build.err; then echo "rejected_by_cxx"; else echo "build_rejected"; fi > obs.txt

clean:
	rm -rf nexus pools log __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
