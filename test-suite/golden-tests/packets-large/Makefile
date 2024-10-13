all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	python make_test_data.py 4096 > data.json
	./nexus.py pid data.json | wc -c >> obs.txt
	./nexus.py cid data.json | wc -c >> obs.txt
	./nexus.py rid data.json | wc -c >> obs.txt
	python make_test_data.py 4097 > data.json
	./nexus.py pid data.json | wc -c >> obs.txt
	./nexus.py cid data.json | wc -c >> obs.txt
	./nexus.py rid data.json | wc -c >> obs.txt
	python make_test_data.py 4097 > data.json
	./nexus.py pid data.json | wc -c >> obs.txt
	./nexus.py cid data.json | wc -c >> obs.txt
	./nexus.py rid data.json | wc -c >> obs.txt
	python make_test_data.py 65536 > data.json
	./nexus.py pid data.json | wc -c >> obs.txt
	./nexus.py cid data.json | wc -c >> obs.txt
	./nexus.py rid data.json | wc -c >> obs.txt
	python make_test_data.py 65537 > data.json
	./nexus.py pid data.json | wc -c >> obs.txt
	./nexus.py cid data.json | wc -c >> obs.txt
	./nexus.py rid data.json | wc -c >> obs.txt
	python make_test_data.py 1048577 > data.json
	./nexus.py pid data.json | wc -c >> obs.txt
	./nexus.py cid data.json | wc -c >> obs.txt
	./nexus.py rid data.json | wc -c >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__ data* 
