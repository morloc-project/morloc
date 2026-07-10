all:
	rm -f *.err obs.txt *.stderr
	morloc make -o nexus main.loc 2> build.err
	./nexus cppFromPy 5 >> obs.txt 2> v1.stderr
	@grep -c "^py:5$$" v1.stderr >> obs.txt
	./nexus pyFromCpp 5 >> obs.txt 2> v2.stderr
	@grep -c "^cpp:5$$" v2.stderr >> obs.txt
	./nexus rFromCpp 5 >> obs.txt 2> v3.stderr
	@grep -c "^cpp:5$$" v3.stderr >> obs.txt
	./nexus cppFromPyMulti 5 3 >> obs.txt 2> v4.stderr
	@grep -c "^py2:5,3$$" v4.stderr >> obs.txt
	./nexus cppFromPyEffErr 5 >> obs.txt 2> v5.stderr
	@grep -c "^pyerr:5$$" v5.stderr >> obs.txt

clean:
	rm -rf nexus pools *.err *.stderr
