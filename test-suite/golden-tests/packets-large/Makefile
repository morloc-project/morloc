all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	# below buffer
	python3 make_test_data.py 4065 > test-data.json
	./nexus pid test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus cid test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus rid test-data.json | wc -c 2>> obs.err  >> obs.txt
	# above buffer
	python3 make_test_data.py 4066 > test-data.json
	./nexus pid test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus cid test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus rid test-data.json | wc -c 2>> obs.err  >> obs.txt
	# above mesg size
	python3 make_test_data.py 1048577 > test-data.json
	./nexus pid test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus cid test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus rid test-data.json | wc -c 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools __pycache__ data* 
