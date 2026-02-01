all:
	morloc make main.loc 2
	./nexus foo '"asdf"' 3.14 > obs.txt

.PHONY: clean
clean:
	rm -rf pool* nexus* __pycache__
