all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err

	# single-language [U8] round-trips. No wire crossing for the byte
	# array; checks that each language's encode/echo/decode chain works
	# inside one pool.
	echo "=== single-language round-trips ===" >> obs.txt
	./nexus pyStr  "See you in the stars my friend" >> obs.txt 2>> obs.err
	./nexus cppStr "hello world" >> obs.txt 2>> obs.err
	./nexus rStr   "hello world" >> obs.txt 2>> obs.err

	# two-language [U8] pipelines. Each forces one wire crossing for
	# the byte array between the named language pair, in both directions.
	echo "=== two-language pipelines ===" >> obs.txt
	./nexus pyCpp "hello world" >> obs.txt 2>> obs.err
	./nexus pyR   "hello world" >> obs.txt 2>> obs.err
	./nexus cppPy "hello world" >> obs.txt 2>> obs.err
	./nexus cppR  "hello world" >> obs.txt 2>> obs.err
	./nexus rPy   "hello world" >> obs.txt 2>> obs.err
	./nexus rCpp  "hello world" >> obs.txt 2>> obs.err

	# three-language pipelines: two wire crossings in sequence, every
	# language touches the byte array.
	echo "=== three-language pipelines ===" >> obs.txt
	./nexus pyCppR "hello world" >> obs.txt 2>> obs.err
	./nexus pyRCpp "hello world" >> obs.txt 2>> obs.err

	# native runtime-type witnesses: each pool reports the runtime type
	# it actually observed for the [U8] argument.
	echo "=== native runtime types ===" >> obs.txt
	./nexus nativePy  "abc" >> obs.txt 2>> obs.err
	./nexus nativeCpp "abc" >> obs.txt 2>> obs.err
	./nexus nativeR   "abc" >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
