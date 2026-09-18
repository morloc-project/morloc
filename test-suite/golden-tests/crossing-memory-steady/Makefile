SHORT = 50
LONG = 4000

# Answering eighty times as many calls must not leave the pool larger.
all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	short=$$(./nexus steady $(SHORT) 2>> obs.err); \
	long=$$(./nexus steady $(LONG) 2>> obs.err); \
	echo "holds-steady=$$([ "$$short" = "$$long" ] && echo yes || echo "no ($$short vs $$long)")" > obs.txt

clean:
	rm -rf nexus pools *.err __pycache__
	rm -rf *-build *build.tmp*
