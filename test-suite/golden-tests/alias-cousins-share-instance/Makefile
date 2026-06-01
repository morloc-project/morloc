all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- test_result ---" > obs.txt
	./nexus test_result >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
