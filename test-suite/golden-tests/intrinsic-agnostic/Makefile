all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus test > obs.txt 2> obs.err
	cat obs-saved.txt >> obs.txt
	echo >> obs.txt

clean:
	rm -rf nexus pools *err obs-saved.txt
