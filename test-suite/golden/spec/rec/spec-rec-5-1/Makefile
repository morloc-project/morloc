all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus swap '{"left":[1],"right":[2]}' '{"left":[3],"right":[4]}' 2>> obs.err >> obs.txt

clean:
	rm -rf nexus pools log
	rm -rf *-build *build.tmp*
	rm -f *.err
