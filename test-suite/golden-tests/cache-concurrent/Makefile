# Concurrent cache-store stress across all three pool languages.
# For each language, fire N parallel nexus invocations of the same
# labeled cached manifold with the same input. Every invocation
# races to compute the same cache key and call cache_store. The
# atomic rename in write_atomic must keep the on-disk file
# uncorrupted: every worker must return the correct result, no
# process may crash, and after the dust settles exactly one packet
# per language (3 total) must remain.

INPUT := '[1,2,3.5]'
N := 20

define RUN
	@for i in $$(seq 1 $(N)); do \
	  MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo$(1) $(INPUT) > obs.run.$(1).$$i.out 2> obs.run.$(1).$$i.err & \
	done; wait
	@echo "outputs_correct_$(1): $$(grep -lx '6.5' obs.run.$(1).*.out 2>/dev/null | wc -l)" >> obs.txt
	@echo "stderr_empty_$(1):    $$(find obs.run.$(1).*.err -size 0 2>/dev/null | wc -l)" >> obs.txt
	@cat obs.run.$(1).*.err > obs.err.$(1)
	@rm -f obs.run.$(1).*

endef

all:
	rm -f *.err obs.txt obs.run.*
	rm -rf cache
	mkdir cache
	morloc make -o nexus main.loc 2> build.err
	$(call RUN,P)
	$(call RUN,R)
	$(call RUN,C)
	@echo "a_packets:    $$(ls cache/a/*.dat 2>/dev/null | wc -l)" >> obs.txt
	@echo "dats:         $$(ls cache/data/*.dat 2>/dev/null | wc -l)" >> obs.txt
	@echo "leftover_tmps: $$(find cache -name '*.tmp.*' 2>/dev/null | wc -l)" >> obs.txt
	cat obs.err.* > obs.err
	rm -f obs.err.*

clean:
	rm -rf nexus pools cache *.err obs.run.*
