all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	@echo "--- oneRef ---" >> obs.txt
	./nexus oneRef hi >> obs.txt 2>> obs.err
	./nexus oneRef "" >> obs.txt 2>> obs.err
	@echo "--- twoRefs ---" >> obs.txt
	./nexus twoRefs hi '[1,2]' >> obs.txt 2>> obs.err
	./nexus twoRefs "" '[1,2]' >> obs.txt 2>> obs.err
	@echo "--- mixed ---" >> obs.txt
	./nexus mixed 5 hi >> obs.txt 2>> obs.err
	./nexus mixed 5 "" >> obs.txt 2>> obs.err
	@echo "--- copyOnly ---" >> obs.txt
	./nexus copyOnly 7 >> obs.txt 2>> obs.err
	./nexus copyOnly 0 >> obs.txt 2>> obs.err
	@echo "--- listRef ---" >> obs.txt
	./nexus listRef '[1,2]' >> obs.txt 2>> obs.err
	./nexus listRef '[]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
