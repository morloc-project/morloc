all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus foo '{"name":"Bob","info":"charming"}' | sed 's/ //g' > obs.txt 2> obs.err

clean:
	rm -rf nexus pools
