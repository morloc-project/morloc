all:
	rm -f obs.txt
	morloc make main.loc
	./nexus price --spot 100 --strike 105 --volatility 0.2 --paths 100 --steps 50 > obs.txt
	./nexus analyzeDist --spot 100 --paths 100 >> obs.txt
	./nexus paths --paths 5 --steps 10 >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
