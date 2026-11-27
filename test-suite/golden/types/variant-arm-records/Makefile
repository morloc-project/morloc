all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- heldCpp ---" >> obs.txt
	./nexus heldCpp '{"Hold":[{"name":"abc","x":1.5}]}' >> obs.txt 2>> obs.err
	echo "--- chainCpp ---" >> obs.txt
	./nexus chainCpp '{"Chain":[{"v":1,"next":{"v":2,"next":null}}]}' >> obs.txt 2>> obs.err
	echo "--- madeCpp ---" >> obs.txt
	./nexus madeCpp '{"Made":[{"a":7,"b":"q"}]}' >> obs.txt 2>> obs.err
	echo "--- leaf ---" >> obs.txt
	./nexus heldCpp '"Leaf"' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
