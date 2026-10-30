FIX = sed 's/file .[^:]*:/file:/'
RUN = 2>&1; echo "exit: $$?"

all:
	rm -f *.err obs.txt *.mpk *.vs
	morloc make -o nexus main.loc 2> build.err
	./nexus -f mpk mk 3 > three.mpk 2>> obs.err
	./nexus -f voidstar mk 2 > two.vs 2>> obs.err
	echo "--- shared length, json ---" > obs.txt
	./nexus same '[1,2]' '[3,4]' >> obs.txt $(RUN) >> obs.txt
	./nexus same '[1,2]' '[3]' >> obs.txt $(RUN) >> obs.txt
	echo "--- shared length, msgpack and voidstar files ---" >> obs.txt
	./nexus dot three.mpk two.vs >> obs.txt $(RUN) >> obs.txt
	./nexus dot three.mpk '[1,1,1]' >> obs.txt $(RUN) >> obs.txt
	echo "--- label in a condition ---" >> obs.txt
	./nexus roll 2 '[1,2,3]' >> obs.txt $(RUN) >> obs.txt
	./nexus roll 5 '[1,2,3]' >> obs.txt $(RUN) >> obs.txt
	./nexus roll 5 three.mpk >> obs.txt $(RUN) >> obs.txt
	echo "--- negative label in a length ---" >> obs.txt
	./nexus rep 0 7 >> obs.txt $(RUN) >> obs.txt
	./nexus rep -1 7 >> obs.txt $(RUN) >> obs.txt
	echo "--- every element of a list ---" >> obs.txt
	./nexus pairLen '[[1,2],[[3,4],[5,6]]]' >> obs.txt $(RUN) >> obs.txt
	./nexus pairLen '[[1,2],[[3,4],[5]]]' >> obs.txt $(RUN) >> obs.txt
	./nexus pairLen '[[1,2],[]]' >> obs.txt $(RUN) >> obs.txt
	echo "--- optional ---" >> obs.txt
	./nexus firstOpt null >> obs.txt $(RUN) >> obs.txt
	./nexus firstOpt '[[1],[2,3]]' >> obs.txt $(RUN) >> obs.txt
	echo "--- unrolled record ---" >> obs.txt
	./nexus opts --left '[1,2]' --right '[3,4]' >> obs.txt $(RUN) >> obs.txt
	./nexus opts --left '[1,2]' --right '[3]' >> obs.txt $(RUN) >> obs.txt
	echo "--- records in a list ---" >> obs.txt
	./nexus deep ok.json '[5]' >> obs.txt $(RUN) >> obs.txt
	./nexus deep ragged.json '[5]' >> obs.txt $(RUN) >> obs.txt
	./nexus deep '[]' '[5,6]' >> obs.txt $(RUN) >> obs.txt
	echo "--- a length that is an expression ---" >> obs.txt
	./nexus grow '[1,2]' '[1,2,3]' >> obs.txt $(RUN) >> obs.txt
	./nexus grow '[1,2]' '[1]' >> obs.txt $(RUN) >> obs.txt
	echo "--- a label on a string ---" >> obs.txt
	./nexus named x '[1,2]' '[3,4]' >> obs.txt $(RUN) >> obs.txt
	./nexus named x '[1,2]' '[3]' >> obs.txt $(RUN) >> obs.txt

clean:
	rm -rf nexus pools *err __pycache__ *.mpk *.vs
	rm -rf *-build *build.tmp*
