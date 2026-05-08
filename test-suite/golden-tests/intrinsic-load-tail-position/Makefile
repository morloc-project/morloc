all:
	rm -f *.err obs.txt
	rm -f /tmp/load-tail-bin.dat /tmp/load-tail-msg.dat /tmp/load-tail-json.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus roundTripBin  42 '"/tmp/load-tail-bin.dat"'  >  obs.txt 2>  obs.err
	./nexus roundTripMsg  43 '"/tmp/load-tail-msg.dat"'  >> obs.txt 2>> obs.err
	./nexus roundTripJson 44 '"/tmp/load-tail-json.dat"' >> obs.txt 2>> obs.err
	rm -f /tmp/load-tail-bin.dat /tmp/load-tail-msg.dat /tmp/load-tail-json.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/load-tail-bin.dat /tmp/load-tail-msg.dat /tmp/load-tail-json.dat
