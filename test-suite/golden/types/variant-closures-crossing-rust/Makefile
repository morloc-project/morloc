all:
	: > obs.txt
	rm -f *.err
	morloc make -X rust:lto=thin -o py-rust py-rust.loc 2>> build.err
	echo "--- py-rust ---" >> obs.txt
	./py-rust one 3 >> obs.txt 2>> obs.err
	./py-rust two 3 >> obs.txt 2>> obs.err
	./py-rust many 3 >> obs.txt 2>> obs.err
	./py-rust ops 3 >> obs.txt 2>> obs.err
	./py-rust boxed 3 >> obs.txt 2>> obs.err
	./py-rust plain 3 >> obs.txt 2>> obs.err
	./py-rust nada 3 >> obs.txt 2>> obs.err
	morloc make -o rust-py rust-py.loc 2>> build.err
	echo "--- rust-py ---" >> obs.txt
	./rust-py one 3 >> obs.txt 2>> obs.err
	./rust-py two 3 >> obs.txt 2>> obs.err
	./rust-py many 3 >> obs.txt 2>> obs.err
	./rust-py ops 3 >> obs.txt 2>> obs.err
	./rust-py boxed 3 >> obs.txt 2>> obs.err
	./rust-py plain 3 >> obs.txt 2>> obs.err
	./rust-py nada 3 >> obs.txt 2>> obs.err
	morloc make -o cpp-rust cpp-rust.loc 2>> build.err
	echo "--- cpp-rust ---" >> obs.txt
	./cpp-rust one 3 >> obs.txt 2>> obs.err
	./cpp-rust two 3 >> obs.txt 2>> obs.err
	./cpp-rust many 3 >> obs.txt 2>> obs.err
	./cpp-rust ops 3 >> obs.txt 2>> obs.err
	./cpp-rust boxed 3 >> obs.txt 2>> obs.err
	./cpp-rust plain 3 >> obs.txt 2>> obs.err
	./cpp-rust nada 3 >> obs.txt 2>> obs.err
	morloc make -o rust-cpp rust-cpp.loc 2>> build.err
	echo "--- rust-cpp ---" >> obs.txt
	./rust-cpp one 3 >> obs.txt 2>> obs.err
	./rust-cpp two 3 >> obs.txt 2>> obs.err
	./rust-cpp many 3 >> obs.txt 2>> obs.err
	./rust-cpp ops 3 >> obs.txt 2>> obs.err
	./rust-cpp boxed 3 >> obs.txt 2>> obs.err
	./rust-cpp plain 3 >> obs.txt 2>> obs.err
	./rust-cpp nada 3 >> obs.txt 2>> obs.err

clean:
	rm -rf py-rust rust-py cpp-rust rust-cpp __pycache__ *.err
	rm -rf *-build *build.tmp*
