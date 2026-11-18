all:
	rm -rf *.err obs.txt gen
	morloc make -X rust:lto=thin -o nexus main.loc 2> build.err
	./nexus total '{"v":[1.0,2.0,3.5],"n":2}' >> obs.txt 2>> obs.err
	grep -c "pub v: ndarray::Array1<f64>" nexus-build/pools/types/rust/mlc/types/_local/main.rs >> obs.txt 2>> obs.err
	morloc types main.loc -o gen 2>> build.err
	grep -A1 "^\[dependencies\]" gen/rust/Cargo.toml >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools gen *.err *-build *build.tmp*
