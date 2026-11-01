RUN = 2>&1; echo "exit: $$?"

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- shared dimension ---" > obs.txt
	./nexus mm '[[2,2],[1,2,3,4]]' '[[2,1],[1,1]]' >> obs.txt $(RUN) >> obs.txt
	./nexus mm '[[2,2],[1,2,3,4]]' '[[3,1],[1,1,1]]' >> obs.txt $(RUN) >> obs.txt
	echo "--- data length against the header ---" >> obs.txt
	./nexus mm '[[2,2],[1,2,3]]' '[[2,1],[1,1]]' >> obs.txt $(RUN) >> obs.txt
	echo "--- one dimension twice ---" >> obs.txt
	./nexus sq '[[2,2],[1,2,3,4]]' >> obs.txt $(RUN) >> obs.txt
	./nexus sq '[[2,3],[1,2,3,4,5,6]]' >> obs.txt $(RUN) >> obs.txt
	./nexus sq '[[-1,-1],[1]]' >> obs.txt $(RUN) >> obs.txt
	echo "--- literal dimensions ---" >> obs.txt
	./nexus two '[[2,3],[1,2,3,4,5,6]]' >> obs.txt $(RUN) >> obs.txt
	./nexus two '[[3,2],[1,2,3,4,5,6]]' >> obs.txt $(RUN) >> obs.txt
	./nexus sq2 '[[2,3],[1,2,3,4]]' >> obs.txt $(RUN) >> obs.txt
	echo "--- a dimension that is an expression ---" >> obs.txt
	./nexus drop1 '[[2,2],[1,2,3,4]]' >> obs.txt $(RUN) >> obs.txt
	./nexus drop1 '[[5,3],[1,2,3]]' >> obs.txt $(RUN) >> obs.txt
	./nexus drop1 '[[0,3],[]]' >> obs.txt $(RUN) >> obs.txt
	./nexus tied '[[2,1],[1,2]]' '[5]' >> obs.txt $(RUN) >> obs.txt
	./nexus tied '[[3,1],[1,2,3]]' '[5]' >> obs.txt $(RUN) >> obs.txt

clean:
	rm -rf nexus pools *err __pycache__
	rm -rf *-build *build.tmp*
