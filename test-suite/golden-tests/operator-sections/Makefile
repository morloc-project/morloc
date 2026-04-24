all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- test_mul ---" > obs.txt
	./nexus test_mul >> obs.txt 2>> obs.err
	echo "--- test_add ---" >> obs.txt
	./nexus test_add >> obs.txt 2>> obs.err
	echo "--- test_gt ---" >> obs.txt
	./nexus test_gt >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
