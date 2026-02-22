all:
	rm -f *.err obs.txt
	morloc make -o tavern tavern.loc 2> build.err
	morloc make -o combat combat.loc 2>> build.err
	./tavern randomClass > obs.txt 2> obs.err
	./combat rollDice >> obs.txt 2>> obs.err

clean:
	rm -rf tavern combat pools __pycache__ *.manifest
