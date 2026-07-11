all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	printf 'a\t5\nb\t25\nc\t30\n' > entries.tsv
	./nexus tally entries.tsv                        >> obs.txt 2>> /dev/null
	if ./nexus count_entries entries.tsv >> obs.txt 2>> /dev/null; then \
		echo "BAD: count_entries accepted" >> obs.txt; \
	else \
		echo "OK" >> obs.txt; \
	fi
	./nexus tally -y entries.tsv                     >> obs.txt 2>> obs.err
	./nexus tally --shout entries.tsv                >> obs.txt 2>> obs.err
	if ./nexus mlcp_count_entries_shout entries.tsv >> obs.txt 2>> /dev/null; then \
		echo "BAD: internal name accepted" >> obs.txt; \
	else \
		echo "OK" >> obs.txt; \
	fi
	if ./nexus mlcp_tally_shout entries.tsv >> obs.txt 2>> /dev/null; then \
		echo "BAD: pre-fix mangling name accepted" >> obs.txt; \
	else \
		echo "OK" >> obs.txt; \
	fi

clean:
	rm -rf nexus pools __pycache__ *.err entries.tsv
