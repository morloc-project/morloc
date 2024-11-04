all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	# below buffer interop
	python make_test_data.py 4065 > test-data.json
	./nexus.py pfoo test-data.json | wc -c >> obs.txt
	./nexus.py cfoo test-data.json | wc -c >> obs.txt
	./nexus.py rfoo test-data.json | wc -c >> obs.txt
	# above buffer interop
	python make_test_data.py 4066 > test-data.json
	./nexus.py pfoo test-data.json | wc -c >> obs.txt
	./nexus.py cfoo test-data.json | wc -c >> obs.txt
	./nexus.py rfoo test-data.json | wc -c >> obs.txt
	# at mesg size
	python make_test_data.py 65536 > test-data.json
	./nexus.py pfoo test-data.json | wc -c >> obs.txt
	./nexus.py cfoo test-data.json | wc -c >> obs.txt
	./nexus.py rfoo test-data.json | wc -c >> obs.txt
	# at mesg size + 1
	python make_test_data.py 65537 > test-data.json
	./nexus.py pfoo test-data.json | wc -c >> obs.txt
	./nexus.py cfoo test-data.json | wc -c >> obs.txt
	./nexus.py rfoo test-data.json | wc -c >> obs.txt
	# above mesg size
	python make_test_data.py 1048577 > test-data.json
	./nexus.py pfoo test-data.json | wc -c >> obs.txt
	./nexus.py cfoo test-data.json | wc -c >> obs.txt
	./nexus.py rfoo test-data.json | wc -c >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__ data* test-data*
