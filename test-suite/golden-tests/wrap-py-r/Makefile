all:
	Rscript -e 'remove.packages("rfoo")'
	morloc wrap R rfoo foo.loc
	./test.R > obs.txt	
	diff exp.txt obs.txt
	Rscript -e 'remove.packages()'

clean:
	rm -rf foo-R
