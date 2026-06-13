all:
	rm -f *.err obs.txt
	morloc make -o alg     alg.loc     >  /dev/null 2>  build.err
	morloc make -o syscfg  syscfg.loc  >  /dev/null 2>> build.err
	morloc make -o literal literal.loc >  /dev/null 2>> build.err
	morloc make -o neg     neg.loc     >  /dev/null 2>> build.err
	@: > obs.txt
	echo alg >> obs.txt 2>  obs.err
	./alg [1,2,3] 2>> obs.err  >> obs.txt
	./alg foo [1,2,3] 2>> obs.err  >> obs.txt
	./alg --alg-config=algconf.json [1,2,3] 2>> obs.err  >> obs.txt
	./alg foo --alg-config=algconf.json [1,2,3] 2>> obs.err  >> obs.txt
	./alg --alg-config='{"m":100,"n":1000}' [1,2,3] 2>> obs.err  >> obs.txt
	./alg --alg-config=algconf-partial.json [1,2,3] 2>> obs.err  >> obs.txt
	./alg -m 5 [1,2,3] 2>> obs.err  >> obs.txt
	./alg -m 5 -n 2 [1,2,3] 2>> obs.err  >> obs.txt
	./alg --alg-config=algconf.json -m 0 -n 0 [1,2,3] 2>> obs.err  >> obs.txt
	echo syscfg >> obs.txt 2>> obs.err
	./syscfg 2>> obs.err  >> obs.txt
	./syscfg bif 2>> obs.err  >> obs.txt
	./syscfg --clean 2>> obs.err  >> obs.txt
	./syscfg --sys-config sysconf.json 2>> obs.err  >> obs.txt
	./syscfg --sys-config sysconf.json --no-clean 2>> obs.err  >> obs.txt
	echo literal >> obs.txt 2>> obs.err
	./literal 2>> obs.err  >> obs.txt
	./literal greet 2>> obs.err  >> obs.txt
	./literal -y a 2>> obs.err  >> obs.txt
	./literal --yolo hello 2>> obs.err  >> obs.txt
	echo neg >> obs.txt 2>> obs.err
	./neg [1,2,3] 2>> obs.err  >> obs.txt
	./neg calc [1,2,3] 2>> obs.err  >> obs.txt
	./neg -m -5 [1,2,3] 2>> obs.err  >> obs.txt
	./neg [-1,-2,-3] 2>> obs.err  >> obs.txt
	./neg -m -5 [-1,-2,-3] 2>> obs.err  >> obs.txt
	./neg calc -m -5 [1,2,3] 2>> obs.err  >> obs.txt

clean:
	rm -rf alg syscfg literal neg pools __pycache__
