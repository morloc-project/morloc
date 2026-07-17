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

clean:
	rm -rf nexus pools *.err
