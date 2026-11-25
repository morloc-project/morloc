N1 = 20
N2 = 200
BATCH = 8000
RUN = MORLOC_WRITE_BUFFER_BYTES=65536 MORLOC_INLINE_SIZE=0 MORLOC_SHM_STATS

all:
	rm -f *.err obs.txt peak-*
	morloc make -o make-streams cpp.loc 2> build.err
	./make-streams make $(N1) s1.mlc 2>> obs.err
	./make-streams make $(N2) s2.mlc 2>> obs.err
	for L in cpp py r rust; do \
	  morloc make -o nexus-$$L $$L.loc 2>> build.err; \
	  $(RUN)=$$PWD/peak-$$L-1 ./nexus-$$L consume s1.mlc > /dev/null 2>> obs.err || echo "$$L consume s1 failed" >> obs.txt; \
	  $(RUN)=$$PWD/peak-$$L-2 ./nexus-$$L consume s2.mlc > /dev/null 2>> obs.err || echo "$$L consume s2 failed" >> obs.txt; \
	  $(RUN)=$$PWD/peak-$$L-c1 ./nexus-$$L consumeAll s1.mlc > /dev/null 2>> obs.err || echo "$$L consumeAll s1 failed" >> obs.txt; \
	  $(RUN)=$$PWD/peak-$$L-c2 ./nexus-$$L consumeAll s2.mlc > /dev/null 2>> obs.err || echo "$$L consumeAll s2 failed" >> obs.txt; \
	  a=$$(cat peak-$$L-1); b=$$(cat peak-$$L-2); c=$$(cat peak-$$L-c1); d=$$(cat peak-$$L-c2); \
	  echo "$$L drain-steady=$$([ $$((b - a)) -le $$((4 * $(BATCH))) ] && echo yes || echo "no ($$a -> $$b)")" >> obs.txt; \
	  echo "$$L control-grows=$$([ $$((d - c)) -gt $$(( ($(N2) - $(N1)) * $(BATCH) / 2 )) ] && echo yes || echo "no ($$c -> $$d)")" >> obs.txt; \
	done

clean:
	rm -rf nexus-* make-streams *.mlc peak-* pools *.err __pycache__
	rm -rf *-build *build.tmp*
