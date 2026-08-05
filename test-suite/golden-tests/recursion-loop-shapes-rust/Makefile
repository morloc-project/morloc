all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus sumTimes '[1,2,3]' 4 >> obs.txt 2>> obs.err
	./nexus sumTimes '[5,10,15,20]' 3 >> obs.txt 2>> obs.err
	./nexus fib 10 >> obs.txt 2>> obs.err
	./nexus fib 50 >> obs.txt 2>> obs.err
	./nexus swapSum '[1,2,3]' '[10,20,30]' 4 >> obs.txt 2>> obs.err
	./nexus swapSum '[1,2,3]' '[10,20,30]' 5 >> obs.txt 2>> obs.err
	./nexus collatzSteps 6 >> obs.txt 2>> obs.err
	./nexus collatzSteps 27 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
