all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- eval ---" >> obs.txt
	./nexus eval '{"Add":[{"Lit":[1.5]},{"Neg":[{"Twice":[{"Wrap":[{"Lit":[2.0]}]}]}]}]}' >> obs.txt 2>> obs.err
	echo "--- evalT ---" >> obs.txt
	./nexus evalT '{"Twice":[{"Wrap":[{"Add":[{"Lit":[3.0]},{"Neg":[{"At":[{"x":1.0,"y":2.0}]}]}]}]}]}' >> obs.txt 2>> obs.err
	echo "--- evalViaPy ---" >> obs.txt
	./nexus evalViaPy '{"Add":[{"Lit":[1.5]},{"Neg":[{"Twice":[{"Wrap":[{"Lit":[2.0]}]}]}]}]}' >> obs.txt 2>> obs.err
	echo "--- evalViaCpp ---" >> obs.txt
	./nexus evalViaCpp '{"Add":[{"Lit":[1.5]},{"Neg":[{"Twice":[{"Wrap":[{"Lit":[2.0]}]}]}]}]}' >> obs.txt 2>> obs.err
	echo "--- evalViaR ---" >> obs.txt
	./nexus evalViaR '{"Add":[{"Lit":[1.5]},{"Neg":[{"Twice":[{"Wrap":[{"Lit":[2.0]}]}]}]}]}' >> obs.txt 2>> obs.err
	echo "--- evalViaRust ---" >> obs.txt
	./nexus evalViaRust '{"Add":[{"Lit":[1.5]},{"Neg":[{"Twice":[{"Wrap":[{"Lit":[2.0]}]}]}]}]}' >> obs.txt 2>> obs.err
	echo "--- termViaCpp ---" >> obs.txt
	./nexus termViaCpp '{"Twice":[{"Wrap":[{"Add":[{"Lit":[3.0]},{"Neg":[{"At":[{"x":1.0,"y":2.0}]}]}]}]}]}' >> obs.txt 2>> obs.err
	echo "--- termViaRust ---" >> obs.txt
	./nexus termViaRust '{"Twice":[{"Wrap":[{"Add":[{"Lit":[3.0]},{"Neg":[{"At":[{"x":1.0,"y":2.0}]}]}]}]}]}' >> obs.txt 2>> obs.err
	echo "--- builtRust ---" >> obs.txt
	./nexus builtRust 5.0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
	rm -rf *-build *build.tmp*
