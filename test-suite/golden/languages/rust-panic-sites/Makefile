all:
	rm -f *.err
	: > obs.txt
	morloc make -X rust:lto=thin -o nexus main.loc 2> build.err
	./nexus steps 2 >> obs.txt 2> obs.err
	./nexus safeSteps 0 >> obs.txt 2>> obs.err
	./nexus mapSteps '[2,5]' >> obs.txt 2>> obs.err
	./nexus idx '[1,2,3]' 9 2> idx.err ; echo "exit:$$?" >> obs.txt
	./nexus steps 0 2> steps.err ; echo "exit:$$?" >> obs.txt
	./nexus mapSteps '[2,0]' 2> map.err ; echo "exit:$$?" >> obs.txt
	cat idx.err steps.err map.err | grep -c "Rust code panicked" >> obs.txt || true
	cat idx.err steps.err map.err | grep -c "Pool process exited" >> obs.txt || true

clean:
	rm -rf nexus pools *.err
	rm -rf *-build *build.tmp*
