all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	# below buffer
	python make_test_data.py 4065 > test-data.json
	./nexus.py pid test-data.json | wc -c >> obs.txt
	./nexus.py cid test-data.json | wc -c >> obs.txt
	./nexus.py rid test-data.json | wc -c >> obs.txt
	# above buffer
	python make_test_data.py 4066 > test-data.json
	./nexus.py pid test-data.json | wc -c >> obs.txt
	./nexus.py cid test-data.json | wc -c >> obs.txt
	./nexus.py rid test-data.json | wc -c >> obs.txt
	# above mesg size
	python make_test_data.py 1048577 > test-data.json
	./nexus.py pid test-data.json | wc -c >> obs.txt
	./nexus.py cid test-data.json | wc -c >> obs.txt
	./nexus.py rid test-data.json | wc -c >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__ data* 
