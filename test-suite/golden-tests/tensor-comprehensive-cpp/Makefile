all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- vec1d ---" > obs.txt
	./nexus vec1d >> obs.txt 2>> obs.err
	echo "--- mat2d ---" >> obs.txt
	./nexus mat2d >> obs.txt 2>> obs.err
	echo "--- ten3d ---" >> obs.txt
	./nexus ten3d >> obs.txt 2>> obs.err
	echo "--- emptyVec ---" >> obs.txt
	./nexus emptyVec >> obs.txt 2>> obs.err
	echo "--- singleElem ---" >> obs.txt
	./nexus singleElem >> obs.txt 2>> obs.err
	echo "--- oneByOne ---" >> obs.txt
	./nexus oneByOne >> obs.txt 2>> obs.err
	echo "--- intVec ---" >> obs.txt
	./nexus intVec >> obs.txt 2>> obs.err
	echo "--- f32Vec ---" >> obs.txt
	./nexus f32Vec >> obs.txt 2>> obs.err
	echo "--- testSumMat ---" >> obs.txt
	./nexus testSumMat >> obs.txt 2>> obs.err
	echo "--- testSumVec ---" >> obs.txt
	./nexus testSumVec >> obs.txt 2>> obs.err
	echo "--- testSum3d ---" >> obs.txt
	./nexus testSum3d >> obs.txt 2>> obs.err
	echo "--- largeVec ---" >> obs.txt
	./nexus largeVec >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *err
