# The offset handler `tagOff` (U64 -> ...) and the IFile handler `sumFile` are
# imported from `hmod`, so their signatures are not in main's local module.
# Terminal-action synthesis must consult imported signatures to detect them;
# otherwise it synthesizes the wrong expansion and the build fails. `streamNums`
# is the only user-facing command, so the nexus uses the single-command `@` form.
all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	printf -- '--- tag-off (imported offset handler; offsets 0,3) ---\n' >> obs.txt
	./nexus -f jsonl @ --tag-off >> obs.txt 2>> obs.err
	printf -- '--- summary (imported IFile handler; @flen=4) ---\n' >> obs.txt
	./nexus -f json @ --summary >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
