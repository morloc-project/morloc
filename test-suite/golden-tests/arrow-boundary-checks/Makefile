CMDS = lossyCast exactCast nullsNotOptional nullsOptional widened dictText dictInts reordered kindlessIntoTyped

all:
	rm -f *.err obs.txt snappy.parquet
	morloc make -o nexus main.loc 2> build.err
	for c in $(CMDS); do echo "--- $$c ---" >> obs.txt; ./nexus $$c 1 >> obs.txt 2>> obs.err || head -2 obs.err | tail -1 >> obs.txt; : > obs.err; done
	./nexus writeSnappy snappy.parquet > /dev/null 2>> obs.err
	echo "--- parquetHead ---" >> obs.txt
	./nexus parquetHead snappy.parquet >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err __pycache__ snappy.parquet
	rm -rf *-build *build.tmp*
