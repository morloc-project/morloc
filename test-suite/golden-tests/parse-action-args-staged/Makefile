define run
	printf -- '--- %s ---\n' "$(1)" >> obs.txt
	./nexus $(1) >> obs.txt 2>> obs.err
endef

all:
	: > obs.txt
	rm -f *.err *.out
	cp n.txt n.orig
	morloc make -o nexus main.loc 2> build.err
	$(call run,total lines:n.txt x= --tagged=a.out)
	cat a.out >> obs.txt
	$(call run,total lines:n.txt x= --listed=b.out --no-stdout)
	cat b.out >> obs.txt
	printf -- '--- total [4,5] x= --listed=c.out ---\n' >> obs.txt
	./nexus total '[4,5]' x= --listed=c.out >> obs.txt 2>> obs.err
	cat c.out >> obs.txt
	printf -- '--- stdin ---\n' >> obs.txt
	./nexus total lines:- x= --listed=d.out < n.txt >> obs.txt 2>> obs.err
	cat d.out >> obs.txt
	$(call run,trio lines:n.txt y= lines:m.txt --first=e1.out --mid=e2.out --third=e3.out --no-stdout)
	cat e1.out e2.out e3.out >> obs.txt
	$(call run,sizeOf lines:n.txt --where=f.out)
	cat f.out >> obs.txt
	./nexus mk d.out > /dev/null 2>> obs.err
	$(call run,plainSize d.out --where=h.out)
	cat h.out >> obs.txt
	$(call run,emit lines:n.txt)
	$(call run,emit lines:n.txt --show=g.out)
	cat g.out >> obs.txt
	cmp -s n.txt n.orig && echo 'input unchanged' >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err *.out n.orig
	rm -rf *-build *build.tmp*
