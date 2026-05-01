all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus echo '[{"x":0,"y":"a"},{"x":1,"y":"b"},{"x":2,"y":"c"}]' > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__
