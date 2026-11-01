all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus rows > obs.txt 2> obs.err
	./nexus countRows '[{"chrom":"c1","pos":7,"qual":0.5},{"chrom":"c1","pos":8,"qual":0.25},{"chrom":"c2","pos":9,"qual":0.75}]' >> obs.txt 2>> obs.err
	./nexus cols >> obs.txt 2>> obs.err
	./nexus wide >> obs.txt 2>> obs.err
	./nexus narrow '[{"chrom":"c1","pos":7,"qual":0.5},{"chrom":"c1","pos":8,"qual":0.25},{"chrom":"c2","pos":9,"qual":0.75}]' >> obs.txt 2>> obs.err
	./nexus viaSchema '[{"chrom":"c1","pos":7,"qual":0.5},{"chrom":"c1","pos":8,"qual":0.25},{"chrom":"c2","pos":9,"qual":0.75}]' >> obs.txt 2>> obs.err

clean:
	rm -f *.err nexus
	rm -rf *-build *build.tmp*
