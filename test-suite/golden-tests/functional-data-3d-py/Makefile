all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	> obs.txt
	./nexus bar true true 5 2>> obs.err  >> obs.txt # 10
	./nexus bar false true 5 2>> obs.err  >> obs.txt # 16
	./nexus bar false false 5 2>> obs.err  >> obs.txt # 15
	./nexus baz true 5 true 2>> obs.err  >> obs.txt # 9
	./nexus foo true 5 2>> obs.err  >> obs.txt # 20

clean:
	rm -rf nexus pools __pycache__
