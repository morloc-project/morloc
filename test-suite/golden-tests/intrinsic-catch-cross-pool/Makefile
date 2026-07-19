all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus catchCross ""       > obs.txt      2>  obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchCross "morloc" >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt

clean:
	rm -rf nexus pools *.err
