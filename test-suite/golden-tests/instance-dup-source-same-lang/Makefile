all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2>&1 \
	  | grep -F "Ambiguous source declarations for 'foo'" > obs.txt || true

clean:
	rm -rf nexus pools __pycache__
