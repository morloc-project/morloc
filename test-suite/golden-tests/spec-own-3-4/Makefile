all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus dead 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus readAfter 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus sameStep 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus twoClosures 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus inLoop 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus nested 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt
	./nexus crossing 100 >> obs.txt 2>> obs.err || echo "failed" >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
