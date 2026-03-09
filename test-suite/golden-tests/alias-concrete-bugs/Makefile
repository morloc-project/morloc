all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- bug1_nested_alias_py ---" > obs.txt
	./nexus bug1_nested_alias_py >> obs.txt 2>> obs.err
	echo "--- bug1_nested_alias_cpp ---" >> obs.txt
	./nexus bug1_nested_alias_cpp >> obs.txt 2>> obs.err
	echo "--- bug2_int64_eq_cpp ---" >> obs.txt
	./nexus bug2_int64_eq_cpp >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
