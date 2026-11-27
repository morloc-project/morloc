all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- phantom Nat, any length ---" > obs.txt
	./nexus bufLen '[1,2]' >> obs.txt 2>&1 || true
	echo "--- two phantom Nats, any shape ---" >> obs.txt
	./nexus gridLen '[[1,2,3]]' >> obs.txt 2>&1 || true
	echo "--- Vector 0 given 0 ---" >> obs.txt
	./nexus vecLen0 '[]' >> obs.txt 2>&1 || true
	echo "--- Vector 0 given 1 ---" >> obs.txt
	./nexus vecLen0 '[1]' >> obs.txt 2>&1 || true
	echo "--- nested, right shape ---" >> obs.txt
	./nexus nestedLen '[[1,2,3],[4,5,6]]' >> obs.txt 2>&1 || true
	echo "--- nested, short inner ---" >> obs.txt
	./nexus nestedLen '[[1,2,3],[4,5]]' >> obs.txt 2>&1 || true
	echo "--- pool, computed length, short ---" >> obs.txt
	./nexus poolSq '[1,2,3]' >> obs.txt 2>&1 || true
	echo "--- pool, alias with computed length, short ---" >> obs.txt
	./nexus poolSqAlias '[1,2,3]' >> obs.txt 2>&1 || true
	echo "--- nexus, alias, right shape ---" >> obs.txt
	./nexus gastAlias '[[1,2,3],[4,5,6]]' >> obs.txt 2>&1 || true
	echo "--- nexus, alias, short inner ---" >> obs.txt
	./nexus gastAlias '[[1],[2]]' >> obs.txt 2>&1 || true
	echo "--- nexus, newtype of Vector, long ---" >> obs.txt
	./nexus gastNew '[1,2,3]' >> obs.txt 2>&1 || true
	echo "--- nexus, list of Vector, long element ---" >> obs.txt
	./nexus gastList '[[1,2],[1,2,3]]' >> obs.txt 2>&1 || true
	echo "--- nexus, record field, long ---" >> obs.txt
	./nexus gastRec '{"xs":[1,2,3]}' >> obs.txt 2>&1 || true
	echo "--- nexus, variable outer, short inner ---" >> obs.txt
	./nexus gastOuterVar '[[1,2]]' >> obs.txt 2>&1 || true
	echo "--- nexus, alias with computed length, short ---" >> obs.txt
	./nexus gastSq '[1,2,3]' >> obs.txt 2>&1 || true
	echo "--- nexus, Matrix data long ---" >> obs.txt
	./nexus gastMat '[[2,3],[1,2,3,4,5,6,7]]' >> obs.txt 2>&1 || true

clean:
	rm -rf nexus pools *err __pycache__
	rm -rf *-build *build.tmp*
