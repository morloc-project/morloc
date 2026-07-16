all:
	rm -f *.err /tmp/eval_sugar_log.txt
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus tupleEval    >> obs.txt 2>> obs.err
	./nexus tupleDo      >> obs.txt 2>> obs.err
	./nexus appEval      >> obs.txt 2>> obs.err
	./nexus appDo        >> obs.txt 2>> obs.err
	./nexus lamEval      >> obs.txt 2>> obs.err
	./nexus lamDo        >> obs.txt 2>> obs.err
	./nexus letEval      >> obs.txt 2>> obs.err
	./nexus letDo        >> obs.txt 2>> obs.err
	./nexus guardEval    >> obs.txt 2>> obs.err
	./nexus guardDo      >> obs.txt 2>> obs.err
	./nexus nestedEval   >> obs.txt 2>> obs.err
	./nexus nestedDo     >> obs.txt 2>> obs.err
	./nexus finalEval    >> obs.txt 2>> obs.err
	./nexus finalDo      >> obs.txt 2>> obs.err
	./nexus totalLogEval >> obs.txt 2>> obs.err
	./nexus totalLogDo   >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__ /tmp/eval_sugar_log.txt
