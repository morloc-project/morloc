all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	printf -- '--- Vector U8 return (json array) ---\n' >> obs.txt
	./nexus -f json @ '[65,66,67]' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err
