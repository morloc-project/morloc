all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	# below buffer interop
	python3 make_test_data.py 4065 > test-data.json
	./nexus pfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	./nexus cfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	./nexus rfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	# above buffer interop
	python3 make_test_data.py 4066 > test-data.json
	./nexus pfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	./nexus cfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	./nexus rfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	# at mesg size
	python3 make_test_data.py 65536 > test-data.json
	./nexus pfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	./nexus cfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	./nexus rfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	# at mesg size + 1
	python3 make_test_data.py 65537 > test-data.json
	./nexus pfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	./nexus cfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	./nexus rfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	# above mesg size
	python3 make_test_data.py 1048577 > test-data.json
	./nexus pfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	./nexus cfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt
	./nexus rfoo test-data.json 2>> obs.err | wc -c | tr -d ' ' >> obs.txt

clean:
	rm -rf nexus pools __pycache__ data* test-data*
	rm -rf *-build *build.tmp*
