all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus copy '{"Succ":[{"Succ":["Zero"]}]}' >> obs.txt 2>> obs.err
	./nexus tag '{"Succ":[{"Succ":["Zero"]}]}' >> obs.txt 2>> obs.err
	./nexus tag '{"Succ":["Zero"]}' >> obs.txt 2>> obs.err
	./nexus twice '{"Succ":["Zero"]}' >> obs.txt 2>> obs.err
	./nexus leaves '{"Fork":["Tip",{"Fork":["Tip","Tip"]}]}' >> obs.txt 2>> obs.err
	./nexus invert '{"Node":[1,"Leaf",{"Node":[2,"Leaf","Leaf"]}]}' >> obs.txt 2>> obs.err
	./nexus viaPool '{"Succ":[{"Succ":["Zero"]}]}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
