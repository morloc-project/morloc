all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus chained 1  >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus chained 2  >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus chained 99 >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus nested 99  >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus nested 1   >> obs.txt 2>> obs.err ; echo "exit:$$?" >> obs.txt

clean:
	rm -rf nexus pools *.err
