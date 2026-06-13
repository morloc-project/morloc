all:
	rm -f *.err obs.txt input.csv input.parquet input.arrow
	python3 mkfixture.py
	printf 'x,y\n42,alpha\n17,beta\n99,gamma\n' > input.csv
	morloc make -o nexus main.loc 2> build.err
	{ \
	  echo '--- csv_input ---';     ./nexus echo input.csv; \
	  echo '--- json_input ---';    ./nexus echo '[{"x":0,"y":"a"},{"x":1,"y":"b"},{"x":2,"y":"c"}]'; \
	  echo '--- parquet_input ---'; ./nexus echo input.parquet; \
	  echo '--- arrow_input ---';   ./nexus echo input.arrow; \
	  echo '--- csv_output ---';    ./nexus --output-form csv echo '[{"x":42,"y":"alpha"},{"x":17,"y":"beta"}]'; \
	} > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__ input.csv input.parquet input.arrow
