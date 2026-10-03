N1 = 20
N2 = 200
LIST = 24000
RUN = MORLOC_INLINE_SIZE=0 MORLOC_SHM_STATS

all:
	rm -f *.err obs.txt peak-*
	for P in cpp-py py-cpp cpp-r r-cpp cpp-rust rust-cpp; do \
	  morloc make -o nexus-$$P $$P.loc 2>> build.err; \
	  for N in $(N1) $(N2); do \
	    got=$$($(RUN)=$$PWD/peak-$$P-$$N ./nexus-$$P steady $$N 2>> obs.err); \
	    want=$$(( 3000 * N * (N - 1) / 2 + 4498500 * N )); \
	    [ "$$got" = "$$want" ] || echo "$$P n=$$N wrong total: $$got (want $$want)" >> obs.txt; \
	  done; \
	  a=$$(cat peak-$$P-$(N1)); b=$$(cat peak-$$P-$(N2)); \
	  echo "$$P steady=$$([ $$((b - a)) -le $$((4 * $(LIST))) ] && echo yes || echo "no ($$a -> $$b)")" >> obs.txt; \
	done

clean:
	rm -rf nexus-* peak-* pools *.err __pycache__
	rm -rf *-build *build.tmp*
