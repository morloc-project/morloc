# Each reject case prints: verdict, internal-error count, location, and
# whether the error is the undetermined-type one.
REJECT = morloc make -o reject $(1) 2> build.err && echo "UNEXPECTED_SUCCESS" >> obs.txt || echo "build_rejected" >> obs.txt; \
	grep -c "morloc: \|CallStack" build.err >> obs.txt || true; \
	grep -o "^$(1):[0-9:-]*" build.err >> obs.txt || true; \
	grep -c "not determined" build.err >> obs.txt || true

all:
	rm -f *.err obs.txt
	$(call REJECT,main.loc)
	$(call REJECT,slice.loc)
	$(call REJECT,shared.loc)
	$(call REJECT,cpp.loc)
	morloc make -o nexus accept.loc 2>> build.err
	./nexus f '["a","b","c"]' 1 2>> obs.err >> obs.txt
	./nexus g '["a","b","c"]' 1 2>> obs.err >> obs.txt

clean:
	rm -rf reject nexus pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
