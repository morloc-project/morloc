all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- sysReport ---" > obs.txt
	./nexus sysReport >> obs.txt 2>> obs.err
	echo "--- envCal ---" >> obs.txt
	./nexus envCal >> obs.txt 2>> obs.err
	echo "--- cpuLoad ---" >> obs.txt
	./nexus cpuLoad >> obs.txt 2>> obs.err
	echo "--- multiQuery ---" >> obs.txt
	./nexus multiQuery >> obs.txt 2>> obs.err
	echo "--- sharedQuery ---" >> obs.txt
	./nexus sharedQuery >> obs.txt 2>> obs.err
	echo "--- pipeline ---" >> obs.txt
	./nexus pipeline >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
