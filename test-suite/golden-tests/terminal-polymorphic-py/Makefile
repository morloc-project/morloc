all:
	: > obs.txt
	rm -f *.err
	morloc make -o nexus main.loc 2> build.err
	# --- default (JSON) and --all path for each export ---
	printf -- '--- getNames default ---\n' >> obs.txt
	./nexus getNames >> obs.txt 2>> obs.err
	printf -- '--- getNames -a ---\n' >> obs.txt
	./nexus getNames -a >> obs.txt 2>> obs.err
	printf -- '--- getNames --all ---\n' >> obs.txt
	./nexus getNames --all >> obs.txt 2>> obs.err
	printf -- '--- getPrimes default ---\n' >> obs.txt
	./nexus getPrimes >> obs.txt 2>> obs.err
	printf -- '--- getPrimes -a ---\n' >> obs.txt
	./nexus getPrimes -a >> obs.txt 2>> obs.err
	printf -- '--- getPrimes --all ---\n' >> obs.txt
	./nexus getPrimes --all >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__
