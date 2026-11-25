all:
	rm -f *.err
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	# Full three-stage pipeline: producer -> doubler -> checkSum = 110.
	@echo "=== producer | doubler | checkSum ===" >> obs.txt
	./nexus -f packet @ producer 2>> obs.err | ./nexus -f packet @ doubler 2>> obs.err | ./nexus checkSum >> obs.txt 2>> obs.err
	# Two-stage variant skips the transform: producer -> checkSum = 55.
	@echo "=== producer | checkSum ===" >> obs.txt
	./nexus -f packet @ producer 2>> obs.err | ./nexus checkSum >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
