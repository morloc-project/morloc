all:
	rm -f *.err obs.txt
	-timeout 10s morloc make -o nexus main.loc 2> build.err
	grep -c "Mutual recursion between type definitions" build.err > obs.txt || echo 0 > obs.txt

clean:
	rm -rf nexus pools *err obs.txt
