all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	for c in boxTodo optTodo boxBox boxList takeBox; do echo "--- $$c ---" >> obs.txt; ./nexus $$c -h 2>> obs.err | grep -E '^Return:|type:' >> obs.txt; done
	echo "--- count ---" >> obs.txt
	./nexus count -h 2>> obs.err | sed -n '/^Data Types:/,$$p' >> obs.txt
	echo "--- json-help ---" >> obs.txt
	./nexus --json-help 2>> obs.err | python3 describe.py >> obs.txt
	echo "--- run ---" >> obs.txt
	./nexus boxTodo x >> obs.txt 2>> obs.err
	./nexus count '{"Boxed":[{"value":{"title":"x"}}]}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err
	rm -rf *-build *build.tmp*
