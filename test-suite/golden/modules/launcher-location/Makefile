all:
	: > obs.txt
	rm -rf *.err "sp ace" link elsewhere deep up
	morloc make -o locprog main.loc 2> build.err
	./locprog 1 >> obs.txt 2>> obs.err
	sh locprog 2 >> obs.txt 2>> obs.err
	cd / && "$(CURDIR)/locprog" 3 >> "$(CURDIR)/obs.txt" 2>> "$(CURDIR)/obs.err"
	mkdir "sp ace" && cp -R locprog locprog-build "sp ace/"
	cd "sp ace" && ./locprog 4 >> ../obs.txt 2>> ../obs.err
	cd "sp ace" && ../locprog 5 >> ../obs.txt 2>> ../obs.err
	./"sp ace"/locprog 6 >> obs.txt 2>> obs.err
	cd / && PATH="$(CURDIR)/sp ace:$$PATH" locprog 7 >> "$(CURDIR)/obs.txt" 2>> "$(CURDIR)/obs.err"
	ln -s "sp ace" link
	link/locprog 8 >> obs.txt 2>> obs.err
	sh -c "$$(sed -n '/^self=/,/^d=/p' locprog)"'; echo "$$d"' /locprog >> obs.txt
	mkdir -p elsewhere deep/inner
	ln -s ../locprog elsewhere/one
	ln -s one elsewhere/two
	ln -s "$(CURDIR)/locprog" elsewhere/abs
	ln -s deep/inner up
	ln -s ../../locprog deep/inner/back
	elsewhere/one 10 >> obs.txt 2>> obs.err
	elsewhere/two 11 >> obs.txt 2>> obs.err
	elsewhere/abs 12 >> obs.txt 2>> obs.err
	up/back 13 >> obs.txt 2>> obs.err
	cd elsewhere && sh two 14 >> ../obs.txt 2>> ../obs.err
	cd / && PATH="$(CURDIR)/elsewhere:$$PATH" two 15 >> "$(CURDIR)/obs.txt" 2>> "$(CURDIR)/obs.err"
	morloc-nexus daemon elsewhere/two -h 2>> obs.err | head -n 1 >> obs.txt

clean:
	rm -rf locprog "sp ace" link elsewhere deep up *.err
	rm -rf *-build *build.tmp*
