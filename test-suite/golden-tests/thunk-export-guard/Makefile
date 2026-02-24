all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- guardThunk 5 ---" > obs.txt
	./nexus guardThunk 5 >> obs.txt 2>> obs.err
	echo "--- guardThunk 0 ---" >> obs.txt
	./nexus guardThunk 0 >> obs.txt 2>> obs.err
	echo "--- guardThunk -3 ---" >> obs.txt
	./nexus guardThunk -- -3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
