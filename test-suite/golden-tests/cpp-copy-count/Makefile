N1 = 5
N2 = 25

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	a=$$(./nexus run 1 $(N1) 2>> obs.err); b=$$(./nexus run 1 $(N2) 2>> obs.err); \
	  echo "allocs-per-batch=$$(( (b - a) / ($(N2) - $(N1)) )) rem=$$(( (b - a) % ($(N2) - $(N1)) ))" > obs.txt
	a=$$(./nexus runWrite w.dat $(N1) 2>> obs.err); b=$$(./nexus runWrite w.dat $(N2) 2>> obs.err); \
	  echo "write-allocs-per-batch=$$(( (b - a) / ($(N2) - $(N1)) )) rem=$$(( (b - a) % ($(N2) - $(N1)) ))" >> obs.txt
	a=$$(./nexus runParts 1 $(N1) 2>> obs.err); b=$$(./nexus runParts 1 $(N2) 2>> obs.err); \
	  echo "parts-allocs-per-batch=$$(( (b - a) / ($(N2) - $(N1)) )) rem=$$(( (b - a) % ($(N2) - $(N1)) ))" >> obs.txt

clean:
	rm -rf nexus pools *.err w.dat
	rm -rf *-build *build.tmp*
