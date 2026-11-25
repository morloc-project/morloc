all:
	rm -rf *.err obs.txt out out-py out-bad
	morloc types main.loc -o out 2> build.err
	ls out > obs.txt
	g++ -std=c++20 -I out/cpp -o use-cpp use.cpp 2>> build.err
	./use-cpp >> obs.txt 2>> obs.err
	python3 use.py out/py >> obs.txt 2>> obs.err
	mkdir -p out/rust/examples && cp use.rs out/rust/examples/use.rs
	cargo build -q --offline --release --manifest-path out/rust/Cargo.toml --example use --target-dir out/target 2>> build.err
	./out/target/release/examples/use >> obs.txt 2>> obs.err
	morloc types main.loc -o out-py --lang py 2>> build.err
	ls out-py >> obs.txt
	if morloc types main.loc -o out-bad --lang python 2> lang.err; then echo UNEXPECTED >> obs.txt; else echo refused >> obs.txt; fi

clean:
	rm -rf out out-py out-bad use-cpp __pycache__ *err *-build *build.tmp*
