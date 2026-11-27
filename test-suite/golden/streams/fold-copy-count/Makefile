N1 = 5
N2 = 25

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	a=$$(./nexus run $(N1) --n 2>> obs.err); b=$$(./nexus run $(N2) --n 2>> obs.err); \
	  echo "fold-allocs-per-batch=$$(( (b - a) / ($(N2) - $(N1)) )) rem=$$(( (b - a) % ($(N2) - $(N1)) ))" > obs.txt

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
