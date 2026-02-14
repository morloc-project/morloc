all:
	rm -f *.err obs.txt
	morloc make -o nexus foo.loc 2> build.err
	# below buffer interop
	python3 make_test_data.py 4065 > test-data.json
	./nexus pfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus cfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus rfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	# above buffer interop
	python3 make_test_data.py 4066 > test-data.json
	./nexus pfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus cfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus rfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	# at mesg size
	python3 make_test_data.py 65536 > test-data.json
	./nexus pfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus cfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus rfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	# at mesg size + 1
	python3 make_test_data.py 65537 > test-data.json
	./nexus pfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus cfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus rfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	# above mesg size
	python3 make_test_data.py 1048577 > test-data.json
	./nexus pfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus cfoo test-data.json | wc -c 2>> obs.err  >> obs.txt
	./nexus rfoo test-data.json | wc -c 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools __pycache__ data* test-data*
