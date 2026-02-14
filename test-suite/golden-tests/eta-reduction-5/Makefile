all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo0 "[1.0,2.0]" "[3.0,4.0]" >  obs.txt
	./nexus foo1 "[1.0,2.0]" "[3.0,4.0]" 2>> obs.err  >> obs.txt
	./nexus foo2 "[1.0,2.0]" "[3.0,4.0]" 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools
