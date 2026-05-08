all:
	rm -f *.err obs.txt input_open.csv input_open.json input_sniff.csv input_poly.json
	morloc make -o nexus main.loc 2> build.err
	printf 'x,y,z\n1,alpha,1.5\n2,beta,2.5\n3,gamma,3.5\n4,delta,4.5\n' > input_open.csv
	printf '[{"x":1,"y":"alpha","z":1.5},{"x":2,"y":"beta","z":2.5},{"x":3,"y":"gamma","z":3.5},{"x":4,"y":"delta","z":4.5}]' > input_open.json
	printf 'x,y\n1,1\n2,2\n3,3\n4,4\n5,5\n6,5.5\n7,6.5\n' > input_sniff.csv
	printf '[{"a":1,"b":true,"c":1.1},{"a":2,"b":false,"c":2.2},{"a":3,"b":true,"c":3.3},{"a":4,"b":false,"c":4.4},{"a":5,"b":true,"c":5.5}]' > input_poly.json
	{ \
	  echo '--- open_csv ---';         ./nexus open_csv input_open.csv; \
	  echo '--- open_json ---';        ./nexus open_json input_open.json; \
	  echo '--- sniff_promote ---';    ./nexus sniff_promote input_sniff.csv; \
	  echo '--- polymorphic_json ---'; ./nexus polymorphic_json input_poly.json; \
	} > obs.txt 2> obs.err

clean:
	rm -rf nexus pools input_open.csv input_open.json input_sniff.csv input_poly.json *err __pycache__
