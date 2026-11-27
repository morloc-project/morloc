all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus run 5 >> obs.txt 2>> obs.err
	grep '^BENCH ' obs.err >> obs.txt
	./nexus cppOnly 5 >> obs.txt 2>> deep.err
	grep '^BENCH ' deep.err >> obs.txt
	./nexus crossLet 5 >> obs.txt 2>> cross.err
	grep '^BENCH ' cross.err >> obs.txt
	./nexus rustBoth 5 >> obs.txt 2>> rs.err
	grep '^BENCH ' rs.err >> obs.txt
	echo "rs start=$$(grep -c '^LOG rs start' rs.err) pass=$$(grep -c '^LOG rs pass' rs.err)" >> obs.txt
	echo "unlogged=$$(grep -c '^LOG ' obs.err)" >> obs.txt
	./nexus rustFail 5 2> fail.err ; echo "exit:$$?" >> obs.txt
	echo "rs fail=$$(grep -c '^LOG rs FAILED rust' fail.err) rows=$$(grep -c '^BENCH ' fail.err)" >> obs.txt
	if morloc make -o bad bad.loc > /dev/null 2> bad.err; then echo '"UNEXPECTED-ACCEPTED"' >> obs.txt; else echo '"rejected"' >> obs.txt; fi

clean:
	rm -rf nexus bad pools __pycache__ *.err *-build
