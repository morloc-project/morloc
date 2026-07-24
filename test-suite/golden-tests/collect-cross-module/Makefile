# A streaming formatter command (`streamNums`, --tagged) defined in a SUBMODULE
# (sub.loc) and imported by name into the root (main.loc). The terminal-action
# handler is synthesized and exported in sub.loc, but program roots come only
# from the root module's export list, so it must be rooted alongside its parent
# command or codegen prunes it. `streamNums` is the only command, so the nexus
# uses the single-command `@` form.
#
# produce emits mk 0 = [1,0] then mk 1 = [11,10]; the stream element is Int, so
# -f jsonl prints one integer per line. --tagged appends each batch's size (2),
# so tagged output carries an extra `2` after each batch. If the handler is
# pruned, --tagged falls back to the untagged default and the `2`s vanish.
all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	printf -- '--- default (untagged): 1 0 11 10 ---\n' >> obs.txt
	./nexus -f jsonl @ >> obs.txt 2>> obs.err
	printf -- '--- tagged (batch size appended): 1 0 2 11 10 2 ---\n' >> obs.txt
	./nexus -f jsonl @ --tagged >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
