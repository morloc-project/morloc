all:
	: > obs.txt
	rm -f *.err
	rm -f /tmp/save-compressed-*.packet
	morloc make -o nexus main.loc 2> build.err
	./nexus --keep-null binIntLvl0  42 '"/tmp/save-compressed-int-lvl0.packet"' >> obs.txt 2>> obs.err
	./nexus --keep-null binIntLvl1  43 '"/tmp/save-compressed-int-lvl1.packet"' >> obs.txt 2>> obs.err
	./nexus --keep-null binIntLvl5  44 '"/tmp/save-compressed-int-lvl5.packet"' >> obs.txt 2>> obs.err
	./nexus --keep-null binIntLvl9  45 '"/tmp/save-compressed-int-lvl9.packet"' >> obs.txt 2>> obs.err
	./nexus --keep-null binListLvl0 '[1,2,3,4,5,6,7,8,9,10]' '"/tmp/save-compressed-list-lvl0.packet"' >> obs.txt 2>> obs.err
	./nexus --keep-null binListLvl5 '[1,2,3,4,5,6,7,8,9,10]' '"/tmp/save-compressed-list-lvl5.packet"' >> obs.txt 2>> obs.err
	echo "=== compression bytes (offset 15: data-command compression slot) ===" >> obs.txt
	echo -n "int-lvl0: "  >> obs.txt; od -An -tx1 -j 15 -N 1 /tmp/save-compressed-int-lvl0.packet  | tr -d ' ' >> obs.txt
	echo -n "int-lvl1: "  >> obs.txt; od -An -tx1 -j 15 -N 1 /tmp/save-compressed-int-lvl1.packet  | tr -d ' ' >> obs.txt
	echo -n "int-lvl5: "  >> obs.txt; od -An -tx1 -j 15 -N 1 /tmp/save-compressed-int-lvl5.packet  | tr -d ' ' >> obs.txt
	echo -n "int-lvl9: "  >> obs.txt; od -An -tx1 -j 15 -N 1 /tmp/save-compressed-int-lvl9.packet  | tr -d ' ' >> obs.txt
	echo -n "list-lvl0: " >> obs.txt; od -An -tx1 -j 15 -N 1 /tmp/save-compressed-list-lvl0.packet | tr -d ' ' >> obs.txt
	echo -n "list-lvl5: " >> obs.txt; od -An -tx1 -j 15 -N 1 /tmp/save-compressed-list-lvl5.packet | tr -d ' ' >> obs.txt
	rm -f /tmp/save-compressed-*.packet

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/save-compressed-*.packet
