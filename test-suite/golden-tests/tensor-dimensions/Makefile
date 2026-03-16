all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- t1d ---" > obs.txt
	./nexus t1d >> obs.txt 2>> obs.err
	echo "--- t2d ---" >> obs.txt
	./nexus t2d >> obs.txt 2>> obs.err
	echo "--- t3d ---" >> obs.txt
	./nexus t3d >> obs.txt 2>> obs.err
	echo "--- t4d ---" >> obs.txt
	./nexus t4d >> obs.txt 2>> obs.err
	echo "--- t5d ---" >> obs.txt
	./nexus t5d >> obs.txt 2>> obs.err
	echo "--- testSum1d ---" >> obs.txt
	./nexus testSum1d >> obs.txt 2>> obs.err
	echo "--- testSum2d ---" >> obs.txt
	./nexus testSum2d >> obs.txt 2>> obs.err
	echo "--- testSum3d ---" >> obs.txt
	./nexus testSum3d >> obs.txt 2>> obs.err
	echo "--- testSum4d ---" >> obs.txt
	./nexus testSum4d >> obs.txt 2>> obs.err
	echo "--- testSum5d ---" >> obs.txt
	./nexus testSum5d >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err __pycache__
