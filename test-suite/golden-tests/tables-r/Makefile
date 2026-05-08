all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	{ \
	  echo '--- mvp ---';        ./nexus mvp 5; \
	  echo '--- rbound ---';     ./nexus rbound 3; \
	  echo '--- cbound ---';     ./nexus cbound 3; \
	  echo '--- sliced ---';     ./nexus sliced 5; \
	  echo '--- sorted ---';     ./nexus sorted 5; \
	  echo '--- picked ---';     ./nexus picked 4; \
	  echo '--- reversed_ ---';  ./nexus reversed_ 4; \
	} > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__
