all:
	rm -f *.err obs.txt stats.txt
	morloc make -o nexus main.loc 2> build.err
	for f in held heldRust heldR heldPy heldPy:thread; do \
	  b=$$(env $$(case $$f in *:thread) echo MORLOC_PY_POOL=thread;; esac) MORLOC_SHM_STATS=$(CURDIR)/stats.txt ./nexus $${f%:*} 4000000 2>> obs.err); \
	  echo "$$f released=$$([ "$$b" -ge 0 ] && [ "$$b" -lt 4000000 ] && echo yes || echo "no ($$b bytes held)")" >> obs.txt; \
	done

clean:
	rm -rf nexus pools *.err __pycache__ stats.txt .RData
	rm -rf *-build *build.tmp*
