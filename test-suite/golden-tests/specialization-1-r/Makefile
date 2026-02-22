all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus length '"yolo my fomo"' > obs.txt 2> obs.err
	./nexus upper '"yolo my fomo"' 2>> obs.err  >> obs.txt

clean:
	rm -rf nexus pools __pycache__
