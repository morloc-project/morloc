all:
	rm -f obs.txt obs.err build.err build5.err
	morloc eval -e 'source Py from "x.py" ("f" as f)' 2> build.err || true
	grep -c "source statements are not allowed in eval mode" build.err > obs.txt
	morloc eval -e 'class Foo a where; foo :: a -> a' 2>> build.err || true
	grep -c "class declarations are not allowed in eval mode" build.err >> obs.txt
	morloc eval -e 'instance Foo Int where; source Py from "x.py" ("foo" as foo)' 2>> build.err || true
	grep -c "instance declarations are not allowed in eval mode" build.err >> obs.txt
	@# eval is the API sandbox: local module imports are blocked so a
	@# user cannot reach uploaded code. `morloc make` is the escape hatch.
	morloc eval -e 'import .locmod; foo' 2>> build.err || true
	grep -c "is not permitted in eval mode" build.err >> obs.txt
	@# --allow-local-modules (dev opt-out) flips the eval-mode block off:
	@# this run fails for a different reason, not the eval-mode guard.
	morloc eval --allow-local-modules -e 'import .locmod; foo' 2> build5.err || true
	grep -c "is not permitted in eval mode" build5.err >> obs.txt

clean:
	rm -f obs.err build.err build5.err
