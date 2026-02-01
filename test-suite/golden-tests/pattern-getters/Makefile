all:
	morloc make main.loc 
	./nexus testr   >  obs.txt
	./nexus testpy  >> obs.txt
	./nexus testcpp >> obs.txt

clean:
	rm -rf nexus* pool* __pycache__
