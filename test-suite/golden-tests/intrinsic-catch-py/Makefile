all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	./nexus catchBare                    > obs.txt      2>  obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchInDo "hi"               >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchInDo "hello"            >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchMixedIO ""              >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchMixedIO "abc"           >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchForeignRaise ""         >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchForeignRaise "morloc"   >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchBangFallible ""         >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchBangFallible "hello"    >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchReadAsArg "42" 100      >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchReadAsArg "bad" 100     >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchThrowAsArg 100          >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchForeignAsArg "" 100     >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchForeignAsArg "abc" 100  >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchAsBothArgs "42" "58"    >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchAsBothArgs "bad" "58"   >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchInListArg "42"          >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchInListArg "bad"         >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchInTupleArg "17"         >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt
	./nexus catchReadRoundTrip 55        >> obs.txt     2>> obs.err ; echo "exit:$$?" >> obs.txt

clean:
	rm -rf nexus pools *.err
