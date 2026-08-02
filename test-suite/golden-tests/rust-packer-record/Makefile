all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus roundRec '{"w":42,"s":"hi","n":9}'  >> obs.txt 2>> obs.err
	./nexus getN     '{"w":7,"s":"x","n":100}'  >> obs.txt 2>> obs.err
	./nexus getW     '{"w":55,"s":"y","n":1}'   >> obs.txt 2>> obs.err
	./nexus setW     '{"w":1,"s":"a","n":2}' 99 >> obs.txt 2>> obs.err
	./nexus setS     '{"w":1,"s":"a","n":2}' zz >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
