all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	{ \
	  echo '--- mvp_kindless ---'; \
	  ./nexus mvp_kindless 100 3; \
	  echo '--- mvp_typed ---'; \
	  ./nexus mvp_typed 100 3; \
	  echo '--- mvp_polymorphic ---'; \
	  ./nexus mvp_polymorphic 5; \
	} > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__
