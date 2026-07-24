all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	# Full three-stage pipeline: producer -> doubler -> checkSum.
	@echo "=== producer | doubler | checkSum ===" >> obs.txt
	./nexus -f packet @ producer 2>> obs.err | ./nexus -f packet @ doubler 2>> obs.err | ./nexus checkSum >> obs.txt 2>> obs.err
	# Two-stage variant, skipping the transform, sanity-checks that
	# doubler was actually doing something: producer -> checkSum = 55.
	@echo "=== producer | checkSum ===" >> obs.txt
	./nexus -f packet @ producer 2>> obs.err | ./nexus checkSum >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
