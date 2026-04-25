all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus factR 5 > obs.txt 2>> obs.err
	./nexus factR 100 >> obs.txt 2>> obs.err; echo "exit:$$?" >> obs.txt

clean:
	rm -rf nexus pools *err
