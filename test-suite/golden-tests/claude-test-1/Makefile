all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus addTwo 5 > obs.txt 2> obs.err
	./nexus doubleList '[1,2,3,4]' 2>> obs.err  >> obs.txt

clean:
	rm -f nexus *err
