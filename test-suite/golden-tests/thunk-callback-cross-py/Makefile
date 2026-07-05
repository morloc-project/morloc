all:
	rm -f *.err tap_log.txt
	: > obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- bareCross ---" >> obs.txt
	./nexus bareCross >> obs.txt 2>> obs.err
	rm -f tap_log.txt
	echo "--- lambdaCross ---" >> obs.txt
	./nexus lambdaCross >> obs.txt 2>> obs.err
	rm -f tap_log.txt
	echo "--- bareCrossMulti ---" >> obs.txt
	./nexus bareCrossMulti >> obs.txt 2>> obs.err
	rm -f tap_log.txt
	echo "--- lambdaCrossMulti ---" >> obs.txt
	./nexus lambdaCrossMulti >> obs.txt 2>> obs.err
	rm -f tap_log.txt

clean:
	rm -rf nexus pools log __pycache__ tap_log.txt
