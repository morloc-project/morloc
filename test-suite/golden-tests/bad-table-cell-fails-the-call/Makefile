all:
	rm -f *.err obs.txt bad.csv
	MORLOC_LANG_PARAMS=rust:lto=off morloc make -o nexus main.loc 2> build.err
	printf 'x,y\nzz,a\n' > bad.csv
	for f in viaPy viaCpp viaR viaRust; do \
	  ./nexus $$f bad.csv > /dev/null 2> run.err; echo "$$f exit $$?" >> obs.txt; \
	  grep -o '"zz" is not Int' run.err | head -1 >> obs.txt; \
	  echo "internal: $$(grep -c 'internal error' run.err)" >> obs.txt; \
	done

clean:
	rm -rf nexus pools *-build *build.tmp* bad.csv run.err
