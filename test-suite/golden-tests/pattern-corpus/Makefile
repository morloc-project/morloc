MHOME := $${MORLOC_HOME:-$$HOME/.local/share/morloc}
NEXUS := $(MHOME)/bin/morloc-nexus

# Pattern-walker corpus: cross-check morloc's compile-time pattern
# desugar against `morloc-nexus view --pattern`. Every case pipes the
# original data (emitted as a data packet via `-f packet`) through
# `view --pattern` and byte-compares the msgpack output against the
# morloc-native extract that `@savem` produced inside the function.
# One line of obs.txt per case: `<fn> <pattern> OK|DIFFER`.
#
# Group separator note: morloc source uses `,` (`.(.a,.b)`) while the
# runtime walker's CLI accepts `;` (`.(.a;.b)`); each context uses its
# own delimiter, and the `recSliceGroup` case exercises both.

# Helper: run one case. $(1) = function name; $(2) = --pattern string.
define run_case
	./prog -f packet -o orig-$(1).packet @ $(1) ref-$(1).mpk 2>> obs.err
	cat orig-$(1).packet | $(NEXUS) view --pattern '$(2)' - -f mpk > obs-$(1).mpk 2>> obs.err
	@if cmp -s ref-$(1).mpk obs-$(1).mpk; then \
	    printf '%-24s %-24s OK\n'     '$(1)' '$(2)' >> obs.txt; \
	 else \
	    printf '%-24s %-24s DIFFER\n' '$(1)' '$(2)' >> obs.txt; \
	 fi

endef

all:
	rm -f *.err
	: > obs.txt
	morloc make -o prog main.loc 2> build.err
	$(call run_case,flatIdx,.[2])
	$(call run_case,flatIdxNeg,.[-1])
	$(call run_case,flatSlice,.[3:7])
	$(call run_case,pairsIdxField0,.[2].0)
	$(call run_case,pairsSliceIdx0,.[0:2].0)
	$(call run_case,pairsSliceIdx1,.[0:2].1)
	$(call run_case,recSliceName,.[0:2].name)
	$(call run_case,recSliceAge,.[0:3].age)
	$(call run_case,recIdxName,.[1].name)
	$(call run_case,nestedLabel,.label)
	$(call run_case,nestedMemberName,.members.[0].name)
	$(call run_case,chainedFieldOnList,.[0:3].inner.name)
	$(call run_case,nestedSliceMemberFirst,.[0:2].members.[0])
	$(call run_case,nestedSliceOuter,.[0:2])
	$(call run_case,nestedSliceBoth,.[:].[:])
	$(call run_case,nestedSliceInner,.[0:2].[1:3])
	$(call run_case,sliceIndexInner,.[:].[0])
	$(call run_case,sliceIndexNeg,.[:].[-1])
	$(call run_case,recSliceGroupPair,.[0:2].(.name;.age))
	$(call run_case,tripleSliceIndex,.[:].[:].[0])

clean:
	rm -rf prog pools __pycache__ *.err
	rm -f ref-*.mpk orig-*.packet obs-*.mpk
