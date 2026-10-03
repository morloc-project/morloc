# Actions on a command with one tail @collect per branch run on a fresh run
# (the manifest names an `entry`); a command with an arm that streams nothing
# keeps a replay entry only.

define run
	printf -- '--- %s ---\n' "$(1)" >> obs.txt
	./nexus $(1) >> obs.txt 2>> obs.err
endef

all:
	: > obs.txt
	rm -f *.err *.out
	morloc make -o nexus main.loc 2> build.err
	python3 -c 'import json; [print(c["name"], t["long"], t["entry"] is not None) for c in json.load(open("nexus-build/manifest.json"))["commands"] for t in c["terminals"]]' >> obs.txt
	$(call run,branchy a)
	$(call run,branchy 3)
	$(call run,branchy x)
	$(call run,branchy a --all)
	$(call run,branchy 3 --all)
	$(call run,branchy x --folded)
	$(call run,branchy a --folded)
	$(call run,branchy 3 --all=a.out --folded=f.out)
	cat a.out f.out >> obs.txt
	$(call run,partial true --all)
	$(call run,partial false --all)

clean:
	rm -rf nexus pools __pycache__ *.err *.out
	rm -rf *-build *build.tmp*
