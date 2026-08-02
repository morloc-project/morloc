all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus greet     'Ada' 'London'              >> obs.txt 2>> obs.err
	./nexus tupIdx    '["x",[10,20,30]]' 1        >> obs.txt 2>> obs.err
	./nexus tupSlice  '["x",[10,20,30,40]]' 1 3   >> obs.txt 2>> obs.err
	./nexus tupGroup  '[7,"mid",9]'               >> obs.txt 2>> obs.err
	./nexus setX      '{"x":0,"y":"hi","z":8}' 99 >> obs.txt 2>> obs.err
	./nexus setTup    '[1,"old",3]' new           >> obs.txt 2>> obs.err
	./nexus setNested '{"a":5,"p":["orig",7]}' rewritten >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
