all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- fib0 ---" >> obs.txt
	./nexus fib 0 >> obs.txt 2>> obs.err
	echo "--- fib10 ---" >> obs.txt
	./nexus fib 10 >> obs.txt 2>> obs.err
	echo "--- fib20 ---" >> obs.txt
	./nexus fib 20 >> obs.txt 2>> obs.err
	echo "--- classifyZero ---" >> obs.txt
	./nexus classify 0 >> obs.txt 2>> obs.err
	echo "--- classifyOther ---" >> obs.txt
	./nexus classify 5 >> obs.txt 2>> obs.err
	echo "--- signPos ---" >> obs.txt
	./nexus sign pos >> obs.txt 2>> obs.err
	echo "--- signNeg ---" >> obs.txt
	./nexus sign neg >> obs.txt 2>> obs.err
	echo "--- signOther ---" >> obs.txt
	./nexus sign huh >> obs.txt 2>> obs.err
	echo "--- notBTrue ---" >> obs.txt
	./nexus notB true >> obs.txt 2>> obs.err
	echo "--- notBFalse ---" >> obs.txt
	./nexus notB false >> obs.txt 2>> obs.err
	echo "--- nandTT ---" >> obs.txt
	./nexus nand true true >> obs.txt 2>> obs.err
	echo "--- nandTF ---" >> obs.txt
	./nexus nand true false >> obs.txt 2>> obs.err
	echo "--- ack22 ---" >> obs.txt
	./nexus ack 2 2 >> obs.txt 2>> obs.err
	echo "--- ack33 ---" >> obs.txt
	./nexus ack 3 3 >> obs.txt 2>> obs.err
	echo "--- firstZeroHead ---" >> obs.txt
	./nexus firstZero '[0,9]' >> obs.txt 2>> obs.err
	echo "--- firstZeroSum ---" >> obs.txt
	./nexus firstZero '[4,5]' >> obs.txt 2>> obs.err
	echo "--- pointOrigin ---" >> obs.txt
	./nexus pointName '{"x":0,"y":0}' >> obs.txt 2>> obs.err
	echo "--- pointYAxis ---" >> obs.txt
	./nexus pointName '{"x":0,"y":7}' >> obs.txt 2>> obs.err
	echo "--- pointElse ---" >> obs.txt
	./nexus pointName '{"x":3,"y":4}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
