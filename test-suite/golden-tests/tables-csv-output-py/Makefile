all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus echo '[{"x":42,"y":"alpha"},{"x":17,"y":"beta"}]' --output-form csv > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__
