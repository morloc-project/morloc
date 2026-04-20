all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- greeting ---" > obs.txt
	./nexus --print greeting >> obs.txt 2>> obs.err
	echo "--- multiline ---" >> obs.txt
	./nexus --print multiline >> obs.txt 2>> obs.err
	echo "--- number ---" >> obs.txt
	./nexus --print number >> obs.txt 2>> obs.err
	echo "--- flag ---" >> obs.txt
	./nexus --print flag >> obs.txt 2>> obs.err
	echo "--- items ---" >> obs.txt
	./nexus --print items >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
