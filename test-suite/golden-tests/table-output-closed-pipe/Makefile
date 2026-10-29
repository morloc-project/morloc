all:
	rm -f *.err obs.txt rc
	morloc make -o nexus main.loc 2> build.err
	for f in csv tsv; do \
	  { ./nexus -f $$f rows 200000 2>> obs.err; echo $$? > rc; } | head -c 1 > /dev/null; \
	  echo "$$f closed pipe: $$(cat rc)" >> obs.txt; \
	done
	./nexus -f csv rows 200000 > /dev/full 2>> obs.err; echo "csv full disk: $$?" >> obs.txt
	./nexus -f csv rows 1 > /dev/full 2>> obs.err; echo "small csv full disk: $$?" >> obs.txt

clean:
	rm -rf nexus pools *-build *build.tmp* rc __pycache__
