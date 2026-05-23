all:
	rm -f *.err obs.txt
	# Use a timeout to keep the suite responsive if a future
	# regression reintroduces the codegen hang. Post-fix this
	# completes in normal build time.
	-timeout 20s morloc make -o nexus main.loc 2> build.err
	# === Plain Int/Str payload literals (agnostic via nexus) ===
	./nexus pairLit               >  obs.txt 2>  obs.err
	./nexus btreeLit              >> obs.txt 2>> obs.err
	./nexus pair2Lit              >> obs.txt 2>> obs.err
	./nexus llTupleLit            >> obs.txt 2>> obs.err
	./nexus roseLit               >> obs.txt 2>> obs.err
	./nexus patLit                >> obs.txt 2>> obs.err
	./nexus containerLit          >> obs.txt 2>> obs.err
	# === @lang-tagged literals: Pair Str across 4 contexts ===
	./nexus pairLangAgnostic      >> obs.txt 2>> obs.err
	./nexus pairLangPy            >> obs.txt 2>> obs.err
	./nexus pairLangCpp           >> obs.txt 2>> obs.err
	./nexus pairLangR             >> obs.txt 2>> obs.err
	# === @lang-tagged literals: BTree Str across 4 contexts ===
	./nexus btreeLangAgnostic     >> obs.txt 2>> obs.err
	./nexus btreeLangPy           >> obs.txt 2>> obs.err
	./nexus btreeLangCpp          >> obs.txt 2>> obs.err
	./nexus btreeLangR            >> obs.txt 2>> obs.err
	# === @lang-tagged literals: Rose Str across 4 contexts ===
	./nexus roseLangAgnostic      >> obs.txt 2>> obs.err
	./nexus roseLangPy            >> obs.txt 2>> obs.err
	./nexus roseLangCpp           >> obs.txt 2>> obs.err
	./nexus roseLangR             >> obs.txt 2>> obs.err
	# === @lang-tagged literals: Container Str (Py-only) ===
	./nexus containerLangAgnostic >> obs.txt 2>> obs.err
	./nexus containerLangPy       >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *err diag.log
