all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus gDirect 21  >  obs.txt 2>  obs.err || echo ESCAPED >> obs.txt
	./nexus gDirect -1  >> obs.txt 2>> obs.err || echo ESCAPED >> obs.txt
	./nexus gDoWrap 21  >> obs.txt 2>> obs.err || echo ESCAPED >> obs.txt
	./nexus gDoWrap -1  >> obs.txt 2>> obs.err || echo ESCAPED >> obs.txt
	./nexus gDoForce 21 >> obs.txt 2>> obs.err || echo ESCAPED >> obs.txt
	./nexus gDoForce -1 >> obs.txt 2>> obs.err || echo ESCAPED >> obs.txt
	./nexus gDoBind 21  >> obs.txt 2>> obs.err || echo ESCAPED >> obs.txt
	./nexus gDoBind -1  >> obs.txt 2>> obs.err || echo ESCAPED >> obs.txt
	./nexus gMany 5     >> obs.txt 2>> obs.err || echo ESCAPED >> obs.txt
	./nexus gMany -1    >> obs.txt 2>> obs.err || echo ESCAPED >> obs.txt

clean:
	rm -rf nexus pools *err
