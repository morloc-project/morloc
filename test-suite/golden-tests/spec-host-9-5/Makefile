REFUSED = morloc make -o bad $(1).loc 2> $(1).err || true; \
	if grep -q "$(2)" $(1).err; then echo "OK: $(1) refused" >> obs.txt; else echo "UNEXPECTED: $(1)" >> obs.txt; fi

all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus latentHome 5 >> obs.txt 2>> obs.err
	./nexus groupHome 5 >> obs.txt 2>> obs.err
	./nexus pairHome 4 >> obs.txt 2>> obs.err
	./nexus unusedHome 5 >> obs.txt 2>> obs.err
	./nexus helperPy 1 >> obs.txt 2>> obs.err
	./nexus helperCpp 1 >> obs.txt 2>> obs.err
	./nexus helperMorloc 1 >> obs.txt 2>> obs.err
	./nexus feedLet 2 >> obs.txt 2>> obs.err
	./nexus reopened 1 >> obs.txt 2>> obs.err
	./nexus pickHome 5 >> obs.txt 2>> obs.err
	./nexus siblings 2 >> obs.txt 2>> obs.err
	./nexus recordHome 3 >> obs.txt 2>> obs.err
	./nexus listHome 3 >> obs.txt 2>> obs.err
	./nexus aliasHome 5 >> obs.txt 2>> obs.err
	$(call REFUSED,bad-latent,cannot leave the pool that made it)
	$(call REFUSED,bad-pair,cannot leave the pool that made it)
	$(call REFUSED,bad-helper,cannot leave the pool that made it)
	$(call REFUSED,bad-feed,cannot leave the pool that made it)
	$(call REFUSED,bad-cache,labeled call that caches or logs)
	$(call REFUSED,bad-hash,a value an intrinsic)
	$(call REFUSED,bad-pick,cannot leave the pool that made it)
	$(call REFUSED,bad-export,cannot leave the pool that made it)
	$(call REFUSED,bad-record,cannot leave the pool that made it)
	$(call REFUSED,bad-list,cannot leave the pool that made it)
	$(call REFUSED,bad-alias,cannot leave the pool that made it)
	$(call REFUSED,bad-returned,cannot leave the pool that made it)
	$(call REFUSED,bad-box,cannot leave the pool that made it)

clean:
	rm -rf nexus bad pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
