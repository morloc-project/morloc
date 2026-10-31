all:
	rm -f *.err obs.txt
	rm -rf tmp_release tmp_debug
	morloc make --no-shm --inline-size 0 --tmpdir $$PWD/tmp_release -o release main.loc 2>> build.err
	morloc make --debug --no-shm --inline-size 0 --tmpdir $$PWD/tmp_debug -o debug main.loc 2>> build.err
	echo "--- release build: not checked ---" > obs.txt
	./release pass 7 >> obs.txt 2>> obs.err; echo "exit: $$?" >> obs.txt
	echo "--- debug build: a defect ---" >> obs.txt
	./debug pass 7 > /dev/null 2> debug.err; echo "exit: $$?" >> obs.txt
	grep -c "a value the program built has the wrong length" debug.err >> obs.txt
	grep -c "exited with status 70" debug.err >> obs.txt

clean:
	rm -rf release debug pools *.err __pycache__ tmp_release tmp_debug
	rm -rf *-build *build.tmp*
