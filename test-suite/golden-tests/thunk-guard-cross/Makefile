all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- guardCross 5 ---" > obs.txt
	./nexus guardCross 5 >> obs.txt 2>> obs.err
	echo "--- guardCross 0 ---" >> obs.txt
	./nexus guardCross 0 >> obs.txt 2>> obs.err
	echo "--- guardCross -3 ---" >> obs.txt
	./nexus guardCross -- -3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
