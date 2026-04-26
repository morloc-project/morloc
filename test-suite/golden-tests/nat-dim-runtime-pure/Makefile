all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- correct 2x3 ---" > obs.txt
	./nexus foo '[[1,2,3],[4,5,6]]' >> obs.txt 2>> obs.err
	echo "--- wrong inner dim (2x4 instead of 2x3) ---" >> obs.txt
	./nexus foo '[[1,2,3,4],[5,6,7,8]]' >> obs.txt 2>&1 || true
	echo "--- wrong outer dim (3x3 instead of 2x3) ---" >> obs.txt
	./nexus foo '[[1,2,3],[4,5,6],[7,8,9]]' >> obs.txt 2>&1 || true

clean:
	rm -rf nexus pools *err __pycache__
