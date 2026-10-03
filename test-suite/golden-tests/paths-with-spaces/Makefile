all:
	rm -rf *.err "a dir"
	: > obs.txt
	mkdir -p "a dir"
	cp main.loc twice.hpp "a dir"/
	cd "a dir" && morloc make -o nexus main.loc 2> ../build.err
	"a dir"/nexus twice 21 >> obs.txt 2>> obs.err

clean:
	rm -rf "a dir" *.err
