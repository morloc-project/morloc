all:
	rm -f obs.txt
	morloc make foo.loc 
	# below buffer
	python3 make_test_data.py 4065 > test-data.json
	./nexus pid test-data.json | wc -c >> obs.txt
	./nexus cid test-data.json | wc -c >> obs.txt
	./nexus rid test-data.json | wc -c >> obs.txt
	# above buffer
	python3 make_test_data.py 4066 > test-data.json
	./nexus pid test-data.json | wc -c >> obs.txt
	./nexus cid test-data.json | wc -c >> obs.txt
	./nexus rid test-data.json | wc -c >> obs.txt
	# above mesg size
	python3 make_test_data.py 1048577 > test-data.json
	./nexus pid test-data.json | wc -c >> obs.txt
	./nexus cid test-data.json | wc -c >> obs.txt
	./nexus rid test-data.json | wc -c >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__ data* 
