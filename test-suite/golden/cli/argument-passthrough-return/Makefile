all:
	rm -f *.err obs.txt
	morloc make -o cpp cpp.loc 2> build.err
	morloc make -o rust rust.loc 2>> build.err
	morloc make --no-shm -o cppfile cpp.loc 2>> build.err
	morloc make --no-shm -o rustfile rust.loc 2>> build.err
	./cpp passInt 5 >> obs.txt 2>> obs.err
	./cpp passStr hello 4 >> obs.txt 2>> obs.err
	./cpp passList [1,2,3] 2 >> obs.txt 2>> obs.err
	MORLOC_INLINE_SIZE=0 ./cpp passStr hello 4 >> obs.txt 2>> obs.err
	MORLOC_INLINE_SIZE=0 ./cpp passList [1,2,3] 2 >> obs.txt 2>> obs.err
	./rust passInt 5 >> obs.txt 2>> obs.err
	./rust passStr hello 4 >> obs.txt 2>> obs.err
	./rust passList [1,2,3] 2 >> obs.txt 2>> obs.err
	MORLOC_INLINE_SIZE=0 ./rust passStr hello 4 >> obs.txt 2>> obs.err
	MORLOC_INLINE_SIZE=0 ./rust passList [1,2,3] 2 >> obs.txt 2>> obs.err
	MORLOC_INLINE_SIZE=0 ./cppfile passList [1,2,3] 2 >> obs.txt 2>> obs.err
	MORLOC_INLINE_SIZE=0 ./rustfile passList [1,2,3] 2 >> obs.txt 2>> obs.err

clean:
	rm -rf cpp rust cppfile rustfile pools *.err __pycache__
	rm -rf *-build *build.tmp*
