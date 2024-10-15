all:
	rm -f obs.txt
	morloc make -v foo.loc > log
	# # below buffer
	./nexus.py pid 4065bit.json | wc -c >> obs.txt
	./nexus.py cid 4065bit.json | wc -c >> obs.txt
	./nexus.py rid 4065bit.json | wc -c >> obs.txt
	# above buffer
	./nexus.py pid 4066bit.json | wc -c >> obs.txt
	./nexus.py cid 4066bit.json | wc -c >> obs.txt
	./nexus.py rid 4066bit.json | wc -c >> obs.txt
	# below buffer interop
	./nexus.py pfoo 4065bit.json | wc -c >> obs.txt
	./nexus.py cfoo 4065bit.json | wc -c >> obs.txt
	./nexus.py rfoo 4065bit.json | wc -c >> obs.txt
	# above buffer interop
	./nexus.py pfoo 4066bit.json | wc -c >> obs.txt
	./nexus.py cfoo 4066bit.json | wc -c >> obs.txt
	./nexus.py rfoo 4066bit.json | wc -c >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
