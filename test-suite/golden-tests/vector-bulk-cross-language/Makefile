FNS = u8RustToCpp u8CppToRust i32RustToCpp i32CppToRust f32RustToCpp f32CppToRust realRustToCpp realCppToRust boolRustToCpp boolCppToRust

all:
	rm -f *.err obs.txt
	morloc make -X rust:lto=thin -o nexus main.loc 2> build.err
	for f in $(FNS); do echo "$$f $$(./nexus $$f 1000003 2>> obs.err)" >> obs.txt; done
	for f in nanRustToCpp nanCppToRust nanVecRustToCpp nanVecCppToRust; do echo "$$f $$(./nexus $$f 3 2>> obs.err)" >> obs.txt; done

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -rf *-build *build.tmp*
