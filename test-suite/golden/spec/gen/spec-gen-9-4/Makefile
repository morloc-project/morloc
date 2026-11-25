all:
	rm -f *.err obs.txt
	morloc make -o nexus-cpp main-cpp.loc 2> build-cpp.err
	./nexus-cpp intTrip '{"d":{"Pick":[3]},"n":1}' >> obs.txt 2>> obs.err
	./nexus-cpp intTrip '{"d":"Skip","n":2}' >> obs.txt 2>> obs.err
	./nexus-cpp strTrip '{"d":{"Pick":["x"]},"n":3}' >> obs.txt 2>> obs.err
	./nexus-cpp picked '{"d":{"Pick":[3]},"n":1}' >> obs.txt 2>> obs.err
	./nexus-cpp picked '{"d":"Skip","n":2}' >> obs.txt 2>> obs.err
	./nexus-cpp duoTrip '{"One":[4]}' >> obs.txt 2>> obs.err
	./nexus-cpp duoTrip '{"Two":["w"]}' >> obs.txt 2>> obs.err
	./nexus-cpp roseTrip '{"Rose":[1,[{"Rose":[2,[]]},{"Rose":[3,[{"Rose":[4,[]]}]]}]]}' >> obs.txt 2>> obs.err
	./nexus-cpp tagTrip '{"Tagged":[9]}' >> obs.txt 2>> obs.err
	./nexus-cpp dateTrip '{"d":{"Pick":["17"]},"n":5}' >> obs.txt 2>> obs.err
	./nexus-cpp dateTrip '{"d":"Skip","n":6}' >> obs.txt 2>> obs.err
	./nexus-cpp intChoice '{"Pick":[1]}' >> obs.txt 2>> obs.err
	./nexus-cpp i32Choice '{"Pick":[2]}' >> obs.txt 2>> obs.err
	./nexus-cpp choiceTrip '{"Pick":[{"hc":{"Pick":[null]}}]}' >> obs.txt 2>> obs.err
	./nexus-cpp holderTrip '{"hc":{"Pick":[{"hc":"Skip"}]}}' >> obs.txt 2>> obs.err
	./nexus-cpp outerPicked '{"Pick":[{"Pick":[1]}]}' >> obs.txt 2>> obs.err
	./nexus-cpp vecTrip '{"V":[7]}' >> obs.txt 2>> obs.err
	./nexus-cpp sTrip '{"x":{"Only":[3]}}' >> obs.txt 2>> obs.err
	./nexus-cpp fooTrip '{"Node":[[{"Leaf":[{"B":[{"Leaf":["Z"]}]}]},{"Leaf":["Z"]}]]}' >> obs.txt 2>> obs.err
	./nexus-cpp barTrip '{"B":[{"Node":[[{"Leaf":["Z"]}]]}]}' >> obs.txt 2>> obs.err
	./nexus-cpp fnHeld '3' >> obs.txt 2>> obs.err
	./nexus-cpp fbHeld '4' >> obs.txt 2>> obs.err
	morloc make -o nexus-rust main-rust.loc 2> build-rust.err
	./nexus-rust intTrip '{"d":{"Pick":[3]},"n":1}' >> obs.txt 2>> obs.err
	./nexus-rust intTrip '{"d":"Skip","n":2}' >> obs.txt 2>> obs.err
	./nexus-rust strTrip '{"d":{"Pick":["x"]},"n":3}' >> obs.txt 2>> obs.err
	./nexus-rust picked '{"d":{"Pick":[3]},"n":1}' >> obs.txt 2>> obs.err
	./nexus-rust picked '{"d":"Skip","n":2}' >> obs.txt 2>> obs.err
	./nexus-rust duoTrip '{"One":[4]}' >> obs.txt 2>> obs.err
	./nexus-rust duoTrip '{"Two":["w"]}' >> obs.txt 2>> obs.err
	./nexus-rust roseTrip '{"Rose":[1,[{"Rose":[2,[]]},{"Rose":[3,[{"Rose":[4,[]]}]]}]]}' >> obs.txt 2>> obs.err
	./nexus-rust tagTrip '{"Tagged":[9]}' >> obs.txt 2>> obs.err
	./nexus-rust dateTrip '{"d":{"Pick":["17"]},"n":5}' >> obs.txt 2>> obs.err
	./nexus-rust dateTrip '{"d":"Skip","n":6}' >> obs.txt 2>> obs.err
	./nexus-rust intChoice '{"Pick":[1]}' >> obs.txt 2>> obs.err
	./nexus-rust i32Choice '{"Pick":[2]}' >> obs.txt 2>> obs.err
	./nexus-rust choiceTrip '{"Pick":[{"hc":{"Pick":[null]}}]}' >> obs.txt 2>> obs.err
	./nexus-rust holderTrip '{"hc":{"Pick":[{"hc":"Skip"}]}}' >> obs.txt 2>> obs.err
	./nexus-rust outerPicked '{"Pick":[{"Pick":[1]}]}' >> obs.txt 2>> obs.err
	./nexus-rust vecTrip '{"V":[7]}' >> obs.txt 2>> obs.err
	./nexus-rust sTrip '{"x":{"Only":[3]}}' >> obs.txt 2>> obs.err
	./nexus-rust fooTrip '{"Node":[[{"Leaf":[{"B":[{"Leaf":["Z"]}]}]},{"Leaf":["Z"]}]]}' >> obs.txt 2>> obs.err
	./nexus-rust barTrip '{"B":[{"Node":[[{"Leaf":["Z"]}]]}]}' >> obs.txt 2>> obs.err
	./nexus-rust fnHeld '3' >> obs.txt 2>> obs.err
	./nexus-rust fbHeld '4' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus-cpp nexus-rust pools *err *-build *build.tmp*
