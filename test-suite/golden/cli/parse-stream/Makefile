all:
	rm -f *.err *.done
	: > obs.txt
	seq 1 20 > big.txt
	seq 1 200000 > huge.txt
	morloc make -o nexus main.loc 2> build.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus firstC c:big.txt big.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus firstC p:big.txt big.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus firstC r:big.txt big.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus firstC rs:big.txt big.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus firstP c:big.txt big.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus firstP p:big.txt big.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus firstP rs:big.txt big.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus firstR c:big.txt big.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus firstR r:big.txt big.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus firstRs c:big.txt big.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus firstRs p:big.txt big.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus firstRs rs:big.txt big.txt >> obs.txt 2>> obs.err
	./nexus three c:ok.txt >> obs.txt 2>> obs.err
	./nexus three c:bad4.txt 2>> obs.txt || echo "exit $$?" >> obs.txt
	./nexus three c:bad8.txt >> obs.txt 2>> obs.err
	./nexus three p:bad4.txt 2>> obs.txt || echo "exit $$?" >> obs.txt
	./nexus three p:bad8.txt >> obs.txt 2>> obs.err
	./nexus three r:bad4.txt 2>> obs.txt || echo "exit $$?" >> obs.txt
	./nexus three r:bad8.txt >> obs.txt 2>> obs.err
	./nexus three rs:bad4.txt 2>> obs.txt || echo "exit $$?" >> obs.txt
	./nexus three rs:bad8.txt >> obs.txt 2>> obs.err
	cat ok.txt | ./nexus three c:- >> obs.txt 2>> obs.err
	./nexus caught c:bad4.txt 2>> obs.txt || echo "exit $$?" >> obs.txt
	./nexus caught p:bad4.txt 2>> obs.txt || echo "exit $$?" >> obs.txt
	MORLOC_CHANNEL_DEPTH=1 ./nexus early c:huge.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus early p:huge.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus early r:huge.txt >> obs.txt 2>> obs.err
	MORLOC_CHANNEL_DEPTH=1 ./nexus early rs:huge.txt >> obs.txt 2>> obs.err
	./nexus three c:die.txt > /dev/null 2>> obs.err || echo "exit $$?" >> obs.txt
	./nexus three p:die.txt > /dev/null 2>> obs.err || echo "exit $$?" >> obs.txt
	./nexus two c:big.txt p:ok.txt >> obs.txt 2>> obs.err
	./nexus two p:big.txt c:bad4.txt >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.done big.txt huge.txt
	rm -rf *-build *build.tmp*
