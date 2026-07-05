all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- sliceIdx0 ---" > obs.txt
	./nexus sliceIdx0 >> obs.txt 2>> obs.err
	echo "--- sliceIdx1 ---" >> obs.txt
	./nexus sliceIdx1 >> obs.txt 2>> obs.err
	echo "--- fullSliceIdx0 ---" >> obs.txt
	./nexus fullSliceIdx0 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err *.manifest
