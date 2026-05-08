all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	{ \
	  echo '--- sliced ---';        ./nexus sliced 5; \
	  echo '--- sorted ---';        ./nexus sorted 5; \
	  echo '--- picked ---';        ./nexus picked 4; \
	  echo '--- reversed_ ---';     ./nexus reversed_ 4; \
	  echo '--- rbound ---';        ./nexus rbound 3; \
	  echo '--- cbound ---';        ./nexus cbound 3; \
	  echo '--- added_dropped ---'; ./nexus added_dropped 3; \
	  echo '--- lit_selected ---';  ./nexus lit_selected 3; \
	} > obs.txt 2> obs.err

clean:
	rm -rf nexus pools *err __pycache__
