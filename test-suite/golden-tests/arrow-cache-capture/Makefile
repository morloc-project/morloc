all:
	rm -f *.err obs.txt *.calls
	rm -rf cache
	mkdir cache
	morloc make --inline-size 0 -o nexus main.loc 2> build.err
	echo "--- cachedResult ---" >> obs.txt
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus cachedResult 3 >> obs.txt 2>> obs.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus cachedResult 3 >> obs.txt 2>> obs.err
	echo "--- cachedArg ---" >> obs.txt
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus cachedArg 3 >> obs.txt 2>> obs.err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus cachedArg 3 >> obs.txt 2>> obs.err
	@echo "mk calls: $$(wc -l < mk.calls | tr -d ' ')" >> obs.txt
	@echo "describe calls: $$(wc -l < describe.calls | tr -d ' ')" >> obs.txt
	@echo "dats: $$(ls cache/data/*.dat 2>/dev/null | wc -l | tr -d ' ')" >> obs.txt
	echo "--- captured ---" >> obs.txt
	./nexus captured 3 >> obs.txt 2>> obs.err
	echo "--- capturedCpp ---" >> obs.txt
	./nexus capturedCpp 3 >> obs.txt 2>> obs.err
	echo "--- capturedList ---" >> obs.txt
	./nexus capturedList 4 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools cache *.err *.calls __pycache__
	rm -rf *-build *build.tmp*
