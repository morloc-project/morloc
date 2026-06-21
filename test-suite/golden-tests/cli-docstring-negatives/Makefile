all:
	rm -f obs.txt build.err
	@for f in $$(ls *.loc 2>/dev/null | sort); do \
	  rm -rf nexus pools .morloc-build; \
	  morloc make -o nexus $$f > /dev/null 2>&1; \
	  echo "$$f $$?" >> obs.txt; \
	done

clean:
	rm -rf nexus pools .morloc-build build.err
