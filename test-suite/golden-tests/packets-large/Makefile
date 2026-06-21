all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	# below buffer
	python3 make_test_data.py 4065 > test-data.txt
	./nexus pid test-data.txt | wc -c 2>> obs.err  >> obs.txt
	./nexus cid test-data.txt | wc -c 2>> obs.err  >> obs.txt
	./nexus rid test-data.txt | wc -c 2>> obs.err  >> obs.txt
	# above buffer
	python3 make_test_data.py 4066 > test-data.txt
	./nexus pid test-data.txt | wc -c 2>> obs.err  >> obs.txt
	./nexus cid test-data.txt | wc -c 2>> obs.err  >> obs.txt
	./nexus rid test-data.txt | wc -c 2>> obs.err  >> obs.txt
	# above mesg size
	python3 make_test_data.py 1048577 > test-data.txt
	./nexus pid test-data.txt | wc -c 2>> obs.err  >> obs.txt
	./nexus cid test-data.txt | wc -c 2>> obs.err  >> obs.txt
	./nexus rid test-data.txt | wc -c 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools __pycache__ data* test-data.txt
