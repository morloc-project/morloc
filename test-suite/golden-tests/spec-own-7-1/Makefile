all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus grow 1000000 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus wrapped 1000000 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus kept 1000 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus given '{"Link":[1,{"Link":[2,"End"]}]}' >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus inLoop 5 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus inLambda 5 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus nonTail 5 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus maybeKept 10 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt

clean:
	rm -rf nexus nexus-* pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
