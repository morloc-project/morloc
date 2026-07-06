all:
	rm -f *.err obs.txt effRec.stderr
	morloc make -o nexus main.loc 2> build.err
	./nexus effRec 5 2 >> obs.txt 2> effRec.stderr
	@grep -c "^noisy:5$$" effRec.stderr >> obs.txt

clean:
	rm -rf nexus pools *.err effRec.stderr
