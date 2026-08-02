all:
	: > obs.txt
	-morloc make -o nexus main.loc 2> build.err
	@grep -q "morloc signature" build.err && echo "rejected" >> obs.txt || echo "not-rejected" >> obs.txt

clean:
	rm -rf nexus pools build.err __pycache__
