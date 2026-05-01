all:
	rm -f *.err obs.txt input.parquet
	python3 mkfixture.py
	morloc make -o nexus main.loc 2> build.err
	./nexus echo input.parquet > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__ input.parquet
