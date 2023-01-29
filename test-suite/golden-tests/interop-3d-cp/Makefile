all:
	rm -f obs.txt
	morloc make -v main.loc > log
	./nexus.pl foo 2 [1,2,3] [4,5,6] > obs.txt
	# [(1*2 + 1 + 4), ...]
	# [8, 11, 15]

clean:
	rm -f nexus* pool* *pdf log
