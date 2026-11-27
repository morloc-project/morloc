# Terminal actions of imported commands: the action entries and the replay
# entries synthesized in the defining module must be rooted under the name
# the root exports, whether that is the command's own name or an alias, and
# two same-named commands from different modules must stay distinct.
#
# twoSites streams from two @collect sites, so its actions run only on its
# saved output (a replay entry); ts (sub's `one`) has a single site, so its
# action also runs on a fresh run; value returns a value.

define run
	printf -- '--- %s ---\n' "$(1)" >> obs.txt
	./nexus $(1) >> obs.txt 2>> obs.err
endef

all:
	: > obs.txt
	rm -f *.err *.out
	morloc make -o nexus main.loc 2> build.err
	$(call run,twoSites)
	$(call run,twoSites --batches=b.out)
	cat b.out >> obs.txt
	$(call run,twoSites --no-stdout --batches=n.out)
	cat n.out >> obs.txt
	$(call run,value 3)
	$(call run,value 3 --show)
	$(call run,value 3 --show=v.out)
	cat v.out >> obs.txt
	$(call run,ts)
	$(call run,ts --batches=t.out)
	cat t.out >> obs.txt
	$(call run,other)
	$(call run,other --batches=o.out)
	cat o.out >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err *.out
	rm -rf *-build *build.tmp*
