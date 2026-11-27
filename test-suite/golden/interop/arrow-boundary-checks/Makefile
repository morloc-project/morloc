CMDS = lossyCast exactCast nullsNotOptional nullsOptional widened dictText dictInts reordered kindlessIntoTyped numericIds

all:
	rm -f *.err obs.txt snappy.parquet *.csv
	morloc make -o nexus main.loc 2> build.err
	for c in $(CMDS); do echo "--- $$c ---" >> obs.txt; ./nexus $$c 1 >> obs.txt 2>> obs.err || head -2 obs.err | tail -1 >> obs.txt; : > obs.err; done
	./nexus writeSnappy snappy.parquet > /dev/null 2>> obs.err
	echo "--- parquetHead ---" >> obs.txt
	./nexus parquetHead snappy.parquet >> obs.txt 2>> obs.err
	printf 'name,age\nalice,30\nbob,41\n' > byname.csv
	echo "--- csvByName ---" >> obs.txt
	./nexus csvByName byname.csv >> obs.txt 2>> obs.err
	printf 'first,last\nada,lovelace\n' > sametyped.csv
	echo "--- csvSameTyped ---" >> obs.txt
	./nexus csvSameTyped sametyped.csv >> obs.txt 2>> obs.err
	echo "--- narrowed ---" >> obs.txt
	./nexus narrowed '[{"x": 255}]' >> obs.txt 2>> obs.err
	./nexus narrowed '[{"x": 300}]' >> obs.txt 2>> obs.err || head -1 obs.err >> obs.txt; : > obs.err

clean:
	rm -rf nexus pools *.err __pycache__ snappy.parquet *.csv
	rm -rf *-build *build.tmp*
