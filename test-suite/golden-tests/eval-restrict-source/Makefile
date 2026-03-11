all:
	rm -f obs.txt obs.err build.err
	morloc eval 'source Py from "x.py" ("f" as f)' 2> build.err || true
	grep -c "source statements are not allowed in eval mode" build.err > obs.txt
	morloc eval 'class Foo a where; foo :: a -> a' 2>> build.err || true
	grep -c "class declarations are not allowed in eval mode" build.err >> obs.txt
	morloc eval 'instance Foo Int where; source Py from "x.py" ("foo" as foo)' 2>> build.err || true
	grep -c "instance declarations are not allowed in eval mode" build.err >> obs.txt

clean:
	rm -f obs.err build.err
