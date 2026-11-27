RUN = 2>&1; echo "exit: $$?"

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus dotA '[1,2]' '[3,4]' > obs.txt $(RUN) >> obs.txt
	./nexus dotA '[1,2]' '[3]' >> obs.txt $(RUN) >> obs.txt
	./nexus dotP '[1,2]' '[3,4]' >> obs.txt $(RUN) >> obs.txt
	./nexus dotP '[1,2]' '[3]' >> obs.txt $(RUN) >> obs.txt
	./nexus dotA -h >> obs.txt $(RUN) >> obs.txt
	./nexus dotP -h >> obs.txt $(RUN) >> obs.txt

clean:
	rm -rf nexus pools *err __pycache__
	rm -rf *-build *build.tmp*
