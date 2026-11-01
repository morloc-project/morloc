all:
	rm -f *.err obs.txt
	morloc typecheck main.loc 2> build.err > /dev/null
	grep 'documents nothing' build.err | sed 's/.*main.loc:/main.loc:/' > obs.txt
	morloc typecheck script.loc 2> script.err > /dev/null
	grep 'documents nothing' script.err | sed 's/.*script.loc:/script.loc:/' >> obs.txt

clean:
	rm -f *.err
