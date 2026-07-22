all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	printf -- '--- whole render: text (joined verbatim) ---\n' >> obs.txt
	./nexus @ --joined >> obs.txt 2>> obs.err
	printf -- '--- whole render: binary (Vector U8 as byte values) ---\n' >> obs.txt
	./nexus @ --bytes 2>> obs.err | od -An -tu1 | tr -s ' ' | sed 's/^ //' >> obs.txt

clean:
	rm -rf nexus pools __pycache__ *.err
