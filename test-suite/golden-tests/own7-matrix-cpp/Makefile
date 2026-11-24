all:
	rm -f *.err obs.txt run.txt oracle.txt asan.txt
	morloc make -o nexus main.loc 2> build.err
	MORLOC_NO_MOVES=1 morloc make -o oracle main.loc 2> oracle-build.err
	CXX="$${CXX:-g++} -fsanitize=address,undefined -fno-sanitize-recover=all" morloc make -o asan main.loc 2> asan-build.err
	echo "c000 $$(./nexus c000 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c000 $$(./oracle c000 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c000 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c000 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c001 $$(./nexus c001 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c001 $$(./oracle c001 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c001 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c001 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c002 $$(./nexus c002 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c002 $$(./oracle c002 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c002 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c002 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c003 $$(./nexus c003 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c003 $$(./oracle c003 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c003 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c003 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c004 $$(./nexus c004 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c004 $$(./oracle c004 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c004 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c004 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c005 $$(./nexus c005 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c005 $$(./oracle c005 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c005 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c005 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c006 $$(./nexus c006 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c006 $$(./oracle c006 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c006 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c006 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c007 $$(./nexus c007 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c007 $$(./oracle c007 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c007 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c007 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c008 $$(./nexus c008 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c008 $$(./oracle c008 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c008 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c008 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c009 $$(./nexus c009 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c009 $$(./oracle c009 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c009 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c009 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c010 $$(./nexus c010 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c010 $$(./oracle c010 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c010 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c010 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c011 $$(./nexus c011 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c011 $$(./oracle c011 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c011 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c011 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c012 $$(./nexus c012 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c012 $$(./oracle c012 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c012 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c012 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c013 $$(./nexus c013 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c013 $$(./oracle c013 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c013 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c013 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c014 $$(./nexus c014 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c014 $$(./oracle c014 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c014 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c014 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c015 $$(./nexus c015 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c015 $$(./oracle c015 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c015 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c015 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c016 $$(./nexus c016 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c016 $$(./oracle c016 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c016 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c016 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c017 $$(./nexus c017 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c017 $$(./oracle c017 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c017 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c017 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c018 $$(./nexus c018 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c018 $$(./oracle c018 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c018 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c018 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c019 $$(./nexus c019 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c019 $$(./oracle c019 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c019 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c019 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c020 $$(./nexus c020 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c020 $$(./oracle c020 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c020 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c020 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c021 $$(./nexus c021 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c021 $$(./oracle c021 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c021 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c021 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c022 $$(./nexus c022 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c022 $$(./oracle c022 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c022 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c022 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c023 $$(./nexus c023 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c023 $$(./oracle c023 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c023 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c023 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c024 $$(./nexus c024 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c024 $$(./oracle c024 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c024 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c024 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c025 $$(./nexus c025 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c025 $$(./oracle c025 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c025 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c025 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c026 $$(./nexus c026 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c026 $$(./oracle c026 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c026 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c026 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c027 $$(./nexus c027 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c027 $$(./oracle c027 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c027 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c027 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c028 $$(./nexus c028 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c028 $$(./oracle c028 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c028 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c028 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c029 $$(./nexus c029 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c029 $$(./oracle c029 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c029 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c029 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c030 $$(./nexus c030 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c030 $$(./oracle c030 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c030 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c030 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c031 $$(./nexus c031 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c031 $$(./oracle c031 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c031 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c031 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c032 $$(./nexus c032 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c032 $$(./oracle c032 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c032 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c032 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c033 $$(./nexus c033 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c033 $$(./oracle c033 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c033 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c033 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c034 $$(./nexus c034 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c034 $$(./oracle c034 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c034 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c034 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c035 $$(./nexus c035 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c035 $$(./oracle c035 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c035 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c035 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c036 $$(./nexus c036 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c036 $$(./oracle c036 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c036 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c036 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c037 $$(./nexus c037 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c037 $$(./oracle c037 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c037 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c037 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c038 $$(./nexus c038 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c038 $$(./oracle c038 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c038 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c038 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c039 $$(./nexus c039 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c039 $$(./oracle c039 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c039 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c039 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c040 $$(./nexus c040 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c040 $$(./oracle c040 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c040 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c040 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c041 $$(./nexus c041 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c041 $$(./oracle c041 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c041 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c041 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c042 $$(./nexus c042 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c042 $$(./oracle c042 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c042 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c042 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c043 $$(./nexus c043 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c043 $$(./oracle c043 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c043 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c043 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c044 $$(./nexus c044 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c044 $$(./oracle c044 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c044 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c044 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c045 $$(./nexus c045 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c045 $$(./oracle c045 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c045 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c045 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c046 $$(./nexus c046 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c046 $$(./oracle c046 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c046 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c046 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c047 $$(./nexus c047 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c047 $$(./oracle c047 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c047 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c047 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c048 $$(./nexus c048 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c048 $$(./oracle c048 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c048 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c048 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c049 $$(./nexus c049 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c049 $$(./oracle c049 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c049 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c049 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c050 $$(./nexus c050 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c050 $$(./oracle c050 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c050 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c050 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c051 $$(./nexus c051 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c051 $$(./oracle c051 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c051 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c051 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c052 $$(./nexus c052 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c052 $$(./oracle c052 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c052 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c052 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c053 $$(./nexus c053 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c053 $$(./oracle c053 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c053 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c053 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c054 $$(./nexus c054 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c054 $$(./oracle c054 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c054 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c054 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c055 $$(./nexus c055 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c055 $$(./oracle c055 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c055 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c055 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c056 $$(./nexus c056 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c056 $$(./oracle c056 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c056 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c056 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c057 $$(./nexus c057 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c057 $$(./oracle c057 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c057 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c057 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c058 $$(./nexus c058 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c058 $$(./oracle c058 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c058 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c058 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c059 $$(./nexus c059 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c059 $$(./oracle c059 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c059 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c059 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c060 $$(./nexus c060 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c060 $$(./oracle c060 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c060 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c060 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c061 $$(./nexus c061 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c061 $$(./oracle c061 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c061 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c061 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c062 $$(./nexus c062 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c062 $$(./oracle c062 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c062 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c062 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c063 $$(./nexus c063 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c063 $$(./oracle c063 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c063 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c063 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c064 $$(./nexus c064 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c064 $$(./oracle c064 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c064 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c064 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c065 $$(./nexus c065 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c065 $$(./oracle c065 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c065 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c065 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c066 $$(./nexus c066 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c066 $$(./oracle c066 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c066 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c066 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c067 $$(./nexus c067 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c067 $$(./oracle c067 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c067 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c067 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c068 $$(./nexus c068 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c068 $$(./oracle c068 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c068 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c068 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c069 $$(./nexus c069 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c069 $$(./oracle c069 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c069 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c069 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c070 $$(./nexus c070 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c070 $$(./oracle c070 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c070 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c070 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c071 $$(./nexus c071 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c071 $$(./oracle c071 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c071 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c071 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c072 $$(./nexus c072 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c072 $$(./oracle c072 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c072 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c072 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c073 $$(./nexus c073 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c073 $$(./oracle c073 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c073 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c073 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c074 $$(./nexus c074 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c074 $$(./oracle c074 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c074 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c074 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c075 $$(./nexus c075 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c075 $$(./oracle c075 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c075 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c075 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c076 $$(./nexus c076 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c076 $$(./oracle c076 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c076 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c076 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c077 $$(./nexus c077 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c077 $$(./oracle c077 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c077 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c077 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c078 $$(./nexus c078 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c078 $$(./oracle c078 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c078 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c078 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c079 $$(./nexus c079 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c079 $$(./oracle c079 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c079 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c079 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c080 $$(./nexus c080 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c080 $$(./oracle c080 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c080 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c080 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c081 $$(./nexus c081 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c081 $$(./oracle c081 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c081 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c081 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c082 $$(./nexus c082 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c082 $$(./oracle c082 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c082 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c082 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c083 $$(./nexus c083 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c083 $$(./oracle c083 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c083 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c083 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c084 $$(./nexus c084 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c084 $$(./oracle c084 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c084 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c084 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c085 $$(./nexus c085 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c085 $$(./oracle c085 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c085 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c085 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c086 $$(./nexus c086 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c086 $$(./oracle c086 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c086 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c086 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c087 $$(./nexus c087 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c087 $$(./oracle c087 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c087 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c087 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c088 $$(./nexus c088 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c088 $$(./oracle c088 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c088 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c088 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c089 $$(./nexus c089 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c089 $$(./oracle c089 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c089 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c089 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c090 $$(./nexus c090 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c090 $$(./oracle c090 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c090 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c090 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c091 $$(./nexus c091 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c091 $$(./oracle c091 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c091 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c091 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c092 $$(./nexus c092 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c092 $$(./oracle c092 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c092 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c092 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c093 $$(./nexus c093 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c093 $$(./oracle c093 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c093 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c093 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c094 $$(./nexus c094 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c094 $$(./oracle c094 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c094 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c094 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c095 $$(./nexus c095 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c095 $$(./oracle c095 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c095 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c095 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c096 $$(./nexus c096 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c096 $$(./oracle c096 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c096 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c096 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c097 $$(./nexus c097 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c097 $$(./oracle c097 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c097 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c097 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c098 $$(./nexus c098 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c098 $$(./oracle c098 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c098 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c098 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c099 $$(./nexus c099 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c099 $$(./oracle c099 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c099 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c099 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c100 $$(./nexus c100 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c100 $$(./oracle c100 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c100 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c100 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c101 $$(./nexus c101 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c101 $$(./oracle c101 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c101 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c101 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c102 $$(./nexus c102 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c102 $$(./oracle c102 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c102 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c102 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c103 $$(./nexus c103 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c103 $$(./oracle c103 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c103 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c103 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c104 $$(./nexus c104 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c104 $$(./oracle c104 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c104 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c104 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c105 $$(./nexus c105 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c105 $$(./oracle c105 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c105 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c105 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c106 $$(./nexus c106 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c106 $$(./oracle c106 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c106 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c106 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c107 $$(./nexus c107 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c107 $$(./oracle c107 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c107 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c107 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c108 $$(./nexus c108 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c108 $$(./oracle c108 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c108 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c108 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c109 $$(./nexus c109 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c109 $$(./oracle c109 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c109 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c109 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c110 $$(./nexus c110 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c110 $$(./oracle c110 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c110 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c110 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c111 $$(./nexus c111 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c111 $$(./oracle c111 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c111 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c111 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c112 $$(./nexus c112 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c112 $$(./oracle c112 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c112 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c112 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c113 $$(./nexus c113 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c113 $$(./oracle c113 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c113 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c113 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c114 $$(./nexus c114 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c114 $$(./oracle c114 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c114 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c114 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c115 $$(./nexus c115 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c115 $$(./oracle c115 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c115 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c115 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c116 $$(./nexus c116 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c116 $$(./oracle c116 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c116 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c116 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c117 $$(./nexus c117 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c117 $$(./oracle c117 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c117 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c117 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c118 $$(./nexus c118 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c118 $$(./oracle c118 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c118 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c118 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c119 $$(./nexus c119 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c119 $$(./oracle c119 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c119 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c119 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c120 $$(./nexus c120 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c120 $$(./oracle c120 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c120 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c120 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c121 $$(./nexus c121 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c121 $$(./oracle c121 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c121 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c121 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c122 $$(./nexus c122 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c122 $$(./oracle c122 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c122 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c122 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c123 $$(./nexus c123 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c123 $$(./oracle c123 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c123 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c123 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c124 $$(./nexus c124 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c124 $$(./oracle c124 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c124 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c124 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c125 $$(./nexus c125 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c125 $$(./oracle c125 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c125 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c125 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c126 $$(./nexus c126 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c126 $$(./oracle c126 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c126 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c126 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c127 $$(./nexus c127 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c127 $$(./oracle c127 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c127 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c127 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c128 $$(./nexus c128 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c128 $$(./oracle c128 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c128 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c128 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c129 $$(./nexus c129 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c129 $$(./oracle c129 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c129 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c129 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c130 $$(./nexus c130 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c130 $$(./oracle c130 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c130 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c130 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c131 $$(./nexus c131 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c131 $$(./oracle c131 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c131 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c131 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c132 $$(./nexus c132 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c132 $$(./oracle c132 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c132 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c132 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c133 $$(./nexus c133 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c133 $$(./oracle c133 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c133 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c133 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c134 $$(./nexus c134 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c134 $$(./oracle c134 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c134 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c134 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c135 $$(./nexus c135 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c135 $$(./oracle c135 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c135 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c135 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c136 $$(./nexus c136 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c136 $$(./oracle c136 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c136 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c136 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c137 $$(./nexus c137 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c137 $$(./oracle c137 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c137 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c137 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c138 $$(./nexus c138 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c138 $$(./oracle c138 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c138 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c138 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c139 $$(./nexus c139 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c139 $$(./oracle c139 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c139 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c139 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c140 $$(./nexus c140 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c140 $$(./oracle c140 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c140 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c140 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c141 $$(./nexus c141 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c141 $$(./oracle c141 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c141 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c141 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c142 $$(./nexus c142 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c142 $$(./oracle c142 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c142 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c142 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c143 $$(./nexus c143 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c143 $$(./oracle c143 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c143 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c143 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c144 $$(./nexus c144 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c144 $$(./oracle c144 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c144 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c144 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c145 $$(./nexus c145 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c145 $$(./oracle c145 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c145 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c145 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c146 $$(./nexus c146 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c146 $$(./oracle c146 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c146 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c146 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c147 $$(./nexus c147 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c147 $$(./oracle c147 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c147 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c147 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c148 $$(./nexus c148 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c148 $$(./oracle c148 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c148 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c148 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c149 $$(./nexus c149 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c149 $$(./oracle c149 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c149 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c149 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c150 $$(./nexus c150 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c150 $$(./oracle c150 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c150 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c150 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c151 $$(./nexus c151 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c151 $$(./oracle c151 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c151 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c151 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c152 $$(./nexus c152 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c152 $$(./oracle c152 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c152 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c152 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c153 $$(./nexus c153 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c153 $$(./oracle c153 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c153 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c153 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c154 $$(./nexus c154 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c154 $$(./oracle c154 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c154 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c154 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c155 $$(./nexus c155 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c155 $$(./oracle c155 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c155 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c155 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c156 $$(./nexus c156 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c156 $$(./oracle c156 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c156 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c156 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c157 $$(./nexus c157 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c157 $$(./oracle c157 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c157 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c157 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c158 $$(./nexus c158 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c158 $$(./oracle c158 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c158 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c158 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c159 $$(./nexus c159 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c159 $$(./oracle c159 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c159 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c159 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c160 $$(./nexus c160 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c160 $$(./oracle c160 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c160 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c160 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c161 $$(./nexus c161 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c161 $$(./oracle c161 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c161 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c161 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c162 $$(./nexus c162 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c162 $$(./oracle c162 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c162 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c162 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c163 $$(./nexus c163 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c163 $$(./oracle c163 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c163 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c163 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c164 $$(./nexus c164 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c164 $$(./oracle c164 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c164 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c164 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c165 $$(./nexus c165 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c165 $$(./oracle c165 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c165 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c165 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c166 $$(./nexus c166 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c166 $$(./oracle c166 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c166 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c166 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c167 $$(./nexus c167 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c167 $$(./oracle c167 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c167 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c167 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c168 $$(./nexus c168 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c168 $$(./oracle c168 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c168 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c168 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c169 $$(./nexus c169 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c169 $$(./oracle c169 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c169 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c169 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c170 $$(./nexus c170 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c170 $$(./oracle c170 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c170 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c170 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c171 $$(./nexus c171 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c171 $$(./oracle c171 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c171 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c171 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c172 $$(./nexus c172 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c172 $$(./oracle c172 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c172 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c172 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c173 $$(./nexus c173 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c173 $$(./oracle c173 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c173 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c173 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c174 $$(./nexus c174 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c174 $$(./oracle c174 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c174 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c174 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c175 $$(./nexus c175 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c175 $$(./oracle c175 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c175 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c175 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c176 $$(./nexus c176 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c176 $$(./oracle c176 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c176 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c176 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c177 $$(./nexus c177 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c177 $$(./oracle c177 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c177 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c177 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c178 $$(./nexus c178 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c178 $$(./oracle c178 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c178 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c178 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c179 $$(./nexus c179 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c179 $$(./oracle c179 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c179 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c179 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c180 $$(./nexus c180 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c180 $$(./oracle c180 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c180 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c180 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c181 $$(./nexus c181 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c181 $$(./oracle c181 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c181 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c181 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c182 $$(./nexus c182 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c182 $$(./oracle c182 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c182 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c182 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c183 $$(./nexus c183 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c183 $$(./oracle c183 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c183 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c183 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c184 $$(./nexus c184 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c184 $$(./oracle c184 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c184 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c184 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c185 $$(./nexus c185 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c185 $$(./oracle c185 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c185 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c185 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c186 $$(./nexus c186 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c186 $$(./oracle c186 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c186 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c186 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c187 $$(./nexus c187 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c187 $$(./oracle c187 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c187 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c187 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c188 $$(./nexus c188 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c188 $$(./oracle c188 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c188 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c188 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c189 $$(./nexus c189 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c189 $$(./oracle c189 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c189 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c189 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c190 $$(./nexus c190 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c190 $$(./oracle c190 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c190 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c190 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c191 $$(./nexus c191 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c191 $$(./oracle c191 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c191 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c191 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c192 $$(./nexus c192 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c192 $$(./oracle c192 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c192 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c192 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c193 $$(./nexus c193 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c193 $$(./oracle c193 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c193 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c193 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c194 $$(./nexus c194 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c194 $$(./oracle c194 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c194 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c194 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c195 $$(./nexus c195 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c195 $$(./oracle c195 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c195 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c195 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c196 $$(./nexus c196 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c196 $$(./oracle c196 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c196 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c196 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c197 $$(./nexus c197 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c197 $$(./oracle c197 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c197 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c197 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c198 $$(./nexus c198 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c198 $$(./oracle c198 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c198 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c198 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c199 $$(./nexus c199 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c199 $$(./oracle c199 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c199 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c199 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c200 $$(./nexus c200 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c200 $$(./oracle c200 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c200 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c200 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c201 $$(./nexus c201 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c201 $$(./oracle c201 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c201 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c201 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c202 $$(./nexus c202 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c202 $$(./oracle c202 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c202 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c202 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c203 $$(./nexus c203 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c203 $$(./oracle c203 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c203 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c203 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c204 $$(./nexus c204 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c204 $$(./oracle c204 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c204 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c204 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c205 $$(./nexus c205 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c205 $$(./oracle c205 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c205 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c205 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c206 $$(./nexus c206 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c206 $$(./oracle c206 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c206 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c206 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c207 $$(./nexus c207 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c207 $$(./oracle c207 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c207 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c207 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c208 $$(./nexus c208 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c208 $$(./oracle c208 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c208 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c208 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c209 $$(./nexus c209 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c209 $$(./oracle c209 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c209 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c209 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c210 $$(./nexus c210 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c210 $$(./oracle c210 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c210 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c210 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c211 $$(./nexus c211 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c211 $$(./oracle c211 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c211 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c211 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c212 $$(./nexus c212 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c212 $$(./oracle c212 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c212 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c212 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c213 $$(./nexus c213 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c213 $$(./oracle c213 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c213 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c213 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c214 $$(./nexus c214 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c214 $$(./oracle c214 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c214 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c214 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c215 $$(./nexus c215 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c215 $$(./oracle c215 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c215 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c215 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c216 $$(./nexus c216 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c216 $$(./oracle c216 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c216 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c216 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c217 $$(./nexus c217 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c217 $$(./oracle c217 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c217 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c217 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c218 $$(./nexus c218 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c218 $$(./oracle c218 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c218 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c218 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c219 $$(./nexus c219 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c219 $$(./oracle c219 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c219 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c219 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c220 $$(./nexus c220 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c220 $$(./oracle c220 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c220 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c220 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c221 $$(./nexus c221 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c221 $$(./oracle c221 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c221 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c221 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c222 $$(./nexus c222 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c222 $$(./oracle c222 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c222 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c222 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c223 $$(./nexus c223 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c223 $$(./oracle c223 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c223 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c223 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c224 $$(./nexus c224 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c224 $$(./oracle c224 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c224 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c224 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c225 $$(./nexus c225 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c225 $$(./oracle c225 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c225 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c225 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c226 $$(./nexus c226 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c226 $$(./oracle c226 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c226 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c226 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c227 $$(./nexus c227 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c227 $$(./oracle c227 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c227 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c227 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c228 $$(./nexus c228 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c228 $$(./oracle c228 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c228 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c228 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c229 $$(./nexus c229 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c229 $$(./oracle c229 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c229 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c229 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c230 $$(./nexus c230 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c230 $$(./oracle c230 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c230 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c230 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c231 $$(./nexus c231 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c231 $$(./oracle c231 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c231 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c231 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c232 $$(./nexus c232 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c232 $$(./oracle c232 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c232 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c232 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c233 $$(./nexus c233 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c233 $$(./oracle c233 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c233 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c233 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c234 $$(./nexus c234 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c234 $$(./oracle c234 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c234 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c234 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c235 $$(./nexus c235 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c235 $$(./oracle c235 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c235 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c235 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c236 $$(./nexus c236 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c236 $$(./oracle c236 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c236 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c236 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c237 $$(./nexus c237 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c237 $$(./oracle c237 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c237 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c237 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c238 $$(./nexus c238 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c238 $$(./oracle c238 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c238 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c238 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c239 $$(./nexus c239 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c239 $$(./oracle c239 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c239 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c239 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c240 $$(./nexus c240 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c240 $$(./oracle c240 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c240 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c240 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c241 $$(./nexus c241 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c241 $$(./oracle c241 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c241 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c241 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c242 $$(./nexus c242 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c242 $$(./oracle c242 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c242 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c242 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c243 $$(./nexus c243 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c243 $$(./oracle c243 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c243 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c243 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c244 $$(./nexus c244 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c244 $$(./oracle c244 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c244 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c244 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c245 $$(./nexus c245 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c245 $$(./oracle c245 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c245 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c245 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c246 $$(./nexus c246 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c246 $$(./oracle c246 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c246 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c246 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c247 $$(./nexus c247 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c247 $$(./oracle c247 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c247 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c247 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c248 $$(./nexus c248 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c248 $$(./oracle c248 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c248 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c248 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c249 $$(./nexus c249 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c249 $$(./oracle c249 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c249 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c249 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c250 $$(./nexus c250 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c250 $$(./oracle c250 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c250 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c250 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c251 $$(./nexus c251 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c251 $$(./oracle c251 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c251 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c251 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c252 $$(./nexus c252 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c252 $$(./oracle c252 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c252 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c252 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c253 $$(./nexus c253 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c253 $$(./oracle c253 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c253 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c253 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c254 $$(./nexus c254 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c254 $$(./oracle c254 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c254 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c254 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c255 $$(./nexus c255 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c255 $$(./oracle c255 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c255 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c255 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c256 $$(./nexus c256 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c256 $$(./oracle c256 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c256 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c256 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c257 $$(./nexus c257 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c257 $$(./oracle c257 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c257 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c257 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c258 $$(./nexus c258 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c258 $$(./oracle c258 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c258 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c258 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c259 $$(./nexus c259 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c259 $$(./oracle c259 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c259 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c259 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c260 $$(./nexus c260 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c260 $$(./oracle c260 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c260 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c260 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c261 $$(./nexus c261 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c261 $$(./oracle c261 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c261 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c261 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c262 $$(./nexus c262 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c262 $$(./oracle c262 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c262 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c262 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c263 $$(./nexus c263 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c263 $$(./oracle c263 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c263 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c263 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c264 $$(./nexus c264 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c264 $$(./oracle c264 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c264 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c264 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c265 $$(./nexus c265 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c265 $$(./oracle c265 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c265 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c265 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c266 $$(./nexus c266 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c266 $$(./oracle c266 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c266 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c266 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c267 $$(./nexus c267 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c267 $$(./oracle c267 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c267 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c267 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c268 $$(./nexus c268 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c268 $$(./oracle c268 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c268 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c268 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c269 $$(./nexus c269 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c269 $$(./oracle c269 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c269 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c269 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c270 $$(./nexus c270 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c270 $$(./oracle c270 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c270 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c270 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c271 $$(./nexus c271 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c271 $$(./oracle c271 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c271 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c271 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c272 $$(./nexus c272 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c272 $$(./oracle c272 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c272 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c272 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c273 $$(./nexus c273 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c273 $$(./oracle c273 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c273 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c273 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c274 $$(./nexus c274 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c274 $$(./oracle c274 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c274 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c274 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c275 $$(./nexus c275 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c275 $$(./oracle c275 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c275 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c275 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c276 $$(./nexus c276 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c276 $$(./oracle c276 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c276 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c276 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c277 $$(./nexus c277 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c277 $$(./oracle c277 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c277 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c277 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c278 $$(./nexus c278 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c278 $$(./oracle c278 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c278 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c278 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c279 $$(./nexus c279 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c279 $$(./oracle c279 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c279 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c279 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c280 $$(./nexus c280 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c280 $$(./oracle c280 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c280 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c280 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c281 $$(./nexus c281 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c281 $$(./oracle c281 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c281 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c281 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c282 $$(./nexus c282 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c282 $$(./oracle c282 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c282 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c282 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c283 $$(./nexus c283 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c283 $$(./oracle c283 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c283 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c283 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c284 $$(./nexus c284 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c284 $$(./oracle c284 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c284 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c284 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c285 $$(./nexus c285 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c285 $$(./oracle c285 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c285 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c285 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c286 $$(./nexus c286 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c286 $$(./oracle c286 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c286 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c286 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c287 $$(./nexus c287 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c287 $$(./oracle c287 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c287 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c287 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c288 $$(./nexus c288 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c288 $$(./oracle c288 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c288 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c288 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c289 $$(./nexus c289 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c289 $$(./oracle c289 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c289 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c289 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c290 $$(./nexus c290 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c290 $$(./oracle c290 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c290 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c290 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c291 $$(./nexus c291 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c291 $$(./oracle c291 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c291 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c291 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c292 $$(./nexus c292 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c292 $$(./oracle c292 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c292 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c292 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c293 $$(./nexus c293 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c293 $$(./oracle c293 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c293 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c293 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c294 $$(./nexus c294 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c294 $$(./oracle c294 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c294 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c294 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c295 $$(./nexus c295 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c295 $$(./oracle c295 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c295 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c295 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c296 $$(./nexus c296 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c296 $$(./oracle c296 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c296 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c296 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c297 $$(./nexus c297 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c297 $$(./oracle c297 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c297 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c297 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c298 $$(./nexus c298 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c298 $$(./oracle c298 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c298 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c298 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c299 $$(./nexus c299 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c299 $$(./oracle c299 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c299 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c299 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c300 $$(./nexus c300 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c300 $$(./oracle c300 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c300 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c300 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c301 $$(./nexus c301 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c301 $$(./oracle c301 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c301 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c301 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c302 $$(./nexus c302 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c302 $$(./oracle c302 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c302 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c302 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c303 $$(./nexus c303 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c303 $$(./oracle c303 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c303 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c303 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c304 $$(./nexus c304 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c304 $$(./oracle c304 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c304 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c304 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c305 $$(./nexus c305 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c305 $$(./oracle c305 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c305 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c305 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c306 $$(./nexus c306 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c306 $$(./oracle c306 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c306 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c306 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c307 $$(./nexus c307 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c307 $$(./oracle c307 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c307 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c307 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c308 $$(./nexus c308 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c308 $$(./oracle c308 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c308 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c308 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c309 $$(./nexus c309 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c309 $$(./oracle c309 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c309 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c309 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c310 $$(./nexus c310 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c310 $$(./oracle c310 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c310 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c310 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c311 $$(./nexus c311 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c311 $$(./oracle c311 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c311 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c311 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c312 $$(./nexus c312 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c312 $$(./oracle c312 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c312 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c312 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c313 $$(./nexus c313 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c313 $$(./oracle c313 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c313 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c313 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c314 $$(./nexus c314 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c314 $$(./oracle c314 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c314 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c314 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c315 $$(./nexus c315 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c315 $$(./oracle c315 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c315 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c315 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c316 $$(./nexus c316 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c316 $$(./oracle c316 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c316 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c316 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c317 $$(./nexus c317 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c317 $$(./oracle c317 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c317 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c317 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c318 $$(./nexus c318 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c318 $$(./oracle c318 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c318 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c318 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c319 $$(./nexus c319 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c319 $$(./oracle c319 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c319 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c319 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c320 $$(./nexus c320 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c320 $$(./oracle c320 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c320 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c320 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c321 $$(./nexus c321 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c321 $$(./oracle c321 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c321 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c321 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c322 $$(./nexus c322 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c322 $$(./oracle c322 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c322 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c322 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c323 $$(./nexus c323 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c323 $$(./oracle c323 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c323 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c323 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c324 $$(./nexus c324 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c324 $$(./oracle c324 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c324 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c324 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c325 $$(./nexus c325 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c325 $$(./oracle c325 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c325 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c325 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c326 $$(./nexus c326 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c326 $$(./oracle c326 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c326 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c326 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c327 $$(./nexus c327 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c327 $$(./oracle c327 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c327 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c327 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c328 $$(./nexus c328 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c328 $$(./oracle c328 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c328 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c328 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c329 $$(./nexus c329 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c329 $$(./oracle c329 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c329 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c329 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c330 $$(./nexus c330 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c330 $$(./oracle c330 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c330 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c330 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c331 $$(./nexus c331 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c331 $$(./oracle c331 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c331 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c331 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c332 $$(./nexus c332 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c332 $$(./oracle c332 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c332 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c332 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c333 $$(./nexus c333 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c333 $$(./oracle c333 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c333 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c333 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c334 $$(./nexus c334 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c334 $$(./oracle c334 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c334 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c334 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c335 $$(./nexus c335 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c335 $$(./oracle c335 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c335 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c335 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c336 $$(./nexus c336 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c336 $$(./oracle c336 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c336 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c336 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c337 $$(./nexus c337 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c337 $$(./oracle c337 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c337 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c337 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c338 $$(./nexus c338 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c338 $$(./oracle c338 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c338 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c338 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c339 $$(./nexus c339 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c339 $$(./oracle c339 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c339 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c339 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c340 $$(./nexus c340 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c340 $$(./oracle c340 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c340 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c340 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c341 $$(./nexus c341 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c341 $$(./oracle c341 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c341 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c341 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c342 $$(./nexus c342 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c342 $$(./oracle c342 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c342 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c342 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c343 $$(./nexus c343 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c343 $$(./oracle c343 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c343 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c343 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c344 $$(./nexus c344 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c344 $$(./oracle c344 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c344 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c344 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c345 $$(./nexus c345 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c345 $$(./oracle c345 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c345 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c345 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c346 $$(./nexus c346 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c346 $$(./oracle c346 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c346 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c346 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c347 $$(./nexus c347 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c347 $$(./oracle c347 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c347 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c347 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c348 $$(./nexus c348 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c348 $$(./oracle c348 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c348 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c348 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c349 $$(./nexus c349 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c349 $$(./oracle c349 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c349 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c349 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c350 $$(./nexus c350 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c350 $$(./oracle c350 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c350 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c350 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c351 $$(./nexus c351 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c351 $$(./oracle c351 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c351 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c351 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c352 $$(./nexus c352 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c352 $$(./oracle c352 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c352 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c352 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c353 $$(./nexus c353 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c353 $$(./oracle c353 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c353 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c353 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c354 $$(./nexus c354 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c354 $$(./oracle c354 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c354 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c354 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c355 $$(./nexus c355 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c355 $$(./oracle c355 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c355 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c355 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c356 $$(./nexus c356 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c356 $$(./oracle c356 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c356 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c356 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c357 $$(./nexus c357 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c357 $$(./oracle c357 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c357 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c357 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c358 $$(./nexus c358 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c358 $$(./oracle c358 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c358 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c358 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c359 $$(./nexus c359 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c359 $$(./oracle c359 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c359 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c359 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c360 $$(./nexus c360 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c360 $$(./oracle c360 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c360 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c360 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c361 $$(./nexus c361 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c361 $$(./oracle c361 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c361 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c361 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c362 $$(./nexus c362 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c362 $$(./oracle c362 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c362 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c362 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c363 $$(./nexus c363 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c363 $$(./oracle c363 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c363 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c363 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c364 $$(./nexus c364 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c364 $$(./oracle c364 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c364 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c364 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c365 $$(./nexus c365 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c365 $$(./oracle c365 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c365 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c365 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c366 $$(./nexus c366 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c366 $$(./oracle c366 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c366 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c366 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c367 $$(./nexus c367 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c367 $$(./oracle c367 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c367 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c367 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c368 $$(./nexus c368 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c368 $$(./oracle c368 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c368 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c368 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c369 $$(./nexus c369 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c369 $$(./oracle c369 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c369 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c369 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c370 $$(./nexus c370 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c370 $$(./oracle c370 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c370 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c370 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c371 $$(./nexus c371 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c371 $$(./oracle c371 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c371 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c371 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c372 $$(./nexus c372 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c372 $$(./oracle c372 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c372 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c372 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c373 $$(./nexus c373 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c373 $$(./oracle c373 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c373 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c373 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c374 $$(./nexus c374 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c374 $$(./oracle c374 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c374 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c374 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c375 $$(./nexus c375 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c375 $$(./oracle c375 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c375 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c375 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c376 $$(./nexus c376 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c376 $$(./oracle c376 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c376 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c376 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c377 $$(./nexus c377 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c377 $$(./oracle c377 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c377 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c377 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c378 $$(./nexus c378 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c378 $$(./oracle c378 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c378 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c378 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c379 $$(./nexus c379 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c379 $$(./oracle c379 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c379 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c379 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c380 $$(./nexus c380 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c380 $$(./oracle c380 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c380 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c380 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c381 $$(./nexus c381 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c381 $$(./oracle c381 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c381 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c381 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c382 $$(./nexus c382 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c382 $$(./oracle c382 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c382 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c382 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c383 $$(./nexus c383 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c383 $$(./oracle c383 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c383 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c383 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c384 $$(./nexus c384 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c384 $$(./oracle c384 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c384 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c384 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c385 $$(./nexus c385 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c385 $$(./oracle c385 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c385 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c385 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c386 $$(./nexus c386 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c386 $$(./oracle c386 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c386 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c386 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c387 $$(./nexus c387 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c387 $$(./oracle c387 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c387 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c387 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c388 $$(./nexus c388 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c388 $$(./oracle c388 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c388 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c388 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c389 $$(./nexus c389 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c389 $$(./oracle c389 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c389 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c389 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c390 $$(./nexus c390 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c390 $$(./oracle c390 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c390 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c390 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c391 $$(./nexus c391 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c391 $$(./oracle c391 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c391 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c391 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c392 $$(./nexus c392 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c392 $$(./oracle c392 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c392 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c392 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c393 $$(./nexus c393 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c393 $$(./oracle c393 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c393 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c393 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c394 $$(./nexus c394 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c394 $$(./oracle c394 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c394 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c394 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c395 $$(./nexus c395 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c395 $$(./oracle c395 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c395 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c395 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c396 $$(./nexus c396 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c396 $$(./oracle c396 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c396 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c396 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c397 $$(./nexus c397 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c397 $$(./oracle c397 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c397 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c397 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c398 $$(./nexus c398 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c398 $$(./oracle c398 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c398 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c398 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c399 $$(./nexus c399 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c399 $$(./oracle c399 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c399 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c399 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c400 $$(./nexus c400 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c400 $$(./oracle c400 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c400 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c400 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c401 $$(./nexus c401 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c401 $$(./oracle c401 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c401 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c401 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c402 $$(./nexus c402 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c402 $$(./oracle c402 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c402 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c402 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c403 $$(./nexus c403 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c403 $$(./oracle c403 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c403 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c403 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c404 $$(./nexus c404 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c404 $$(./oracle c404 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c404 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c404 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c405 $$(./nexus c405 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c405 $$(./oracle c405 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c405 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c405 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c406 $$(./nexus c406 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c406 $$(./oracle c406 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c406 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c406 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c407 $$(./nexus c407 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c407 $$(./oracle c407 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c407 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c407 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c408 $$(./nexus c408 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c408 $$(./oracle c408 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c408 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c408 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c409 $$(./nexus c409 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c409 $$(./oracle c409 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c409 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c409 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c410 $$(./nexus c410 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c410 $$(./oracle c410 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c410 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c410 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c411 $$(./nexus c411 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c411 $$(./oracle c411 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c411 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c411 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c412 $$(./nexus c412 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c412 $$(./oracle c412 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c412 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c412 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c413 $$(./nexus c413 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c413 $$(./oracle c413 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c413 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c413 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c414 $$(./nexus c414 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c414 $$(./oracle c414 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c414 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c414 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c415 $$(./nexus c415 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c415 $$(./oracle c415 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c415 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c415 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c416 $$(./nexus c416 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c416 $$(./oracle c416 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c416 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c416 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c417 $$(./nexus c417 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c417 $$(./oracle c417 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c417 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c417 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c418 $$(./nexus c418 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c418 $$(./oracle c418 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c418 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c418 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c419 $$(./nexus c419 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c419 $$(./oracle c419 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c419 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c419 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c420 $$(./nexus c420 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c420 $$(./oracle c420 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c420 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c420 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c421 $$(./nexus c421 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c421 $$(./oracle c421 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c421 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c421 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c422 $$(./nexus c422 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c422 $$(./oracle c422 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c422 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c422 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c423 $$(./nexus c423 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c423 $$(./oracle c423 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c423 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c423 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c424 $$(./nexus c424 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c424 $$(./oracle c424 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c424 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c424 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c425 $$(./nexus c425 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c425 $$(./oracle c425 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c425 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c425 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c426 $$(./nexus c426 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c426 $$(./oracle c426 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c426 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c426 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c427 $$(./nexus c427 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c427 $$(./oracle c427 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c427 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c427 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c428 $$(./nexus c428 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c428 $$(./oracle c428 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c428 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c428 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c429 $$(./nexus c429 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c429 $$(./oracle c429 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c429 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c429 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c430 $$(./nexus c430 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c430 $$(./oracle c430 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c430 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c430 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c431 $$(./nexus c431 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c431 $$(./oracle c431 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c431 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c431 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c432 $$(./nexus c432 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c432 $$(./oracle c432 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c432 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c432 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c433 $$(./nexus c433 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c433 $$(./oracle c433 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c433 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c433 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c434 $$(./nexus c434 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c434 $$(./oracle c434 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c434 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c434 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c435 $$(./nexus c435 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c435 $$(./oracle c435 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c435 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c435 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c436 $$(./nexus c436 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c436 $$(./oracle c436 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c436 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c436 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c437 $$(./nexus c437 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c437 $$(./oracle c437 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c437 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c437 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c438 $$(./nexus c438 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c438 $$(./oracle c438 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c438 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c438 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c439 $$(./nexus c439 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c439 $$(./oracle c439 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c439 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c439 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c440 $$(./nexus c440 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c440 $$(./oracle c440 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c440 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c440 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c441 $$(./nexus c441 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c441 $$(./oracle c441 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c441 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c441 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c442 $$(./nexus c442 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c442 $$(./oracle c442 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c442 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c442 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c443 $$(./nexus c443 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c443 $$(./oracle c443 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c443 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c443 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c444 $$(./nexus c444 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c444 $$(./oracle c444 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c444 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c444 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c445 $$(./nexus c445 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c445 $$(./oracle c445 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c445 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c445 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c446 $$(./nexus c446 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c446 $$(./oracle c446 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c446 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c446 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c447 $$(./nexus c447 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c447 $$(./oracle c447 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c447 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c447 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c448 $$(./nexus c448 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c448 $$(./oracle c448 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c448 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c448 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c449 $$(./nexus c449 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c449 $$(./oracle c449 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c449 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c449 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c450 $$(./nexus c450 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c450 $$(./oracle c450 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c450 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c450 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c451 $$(./nexus c451 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c451 $$(./oracle c451 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c451 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c451 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c452 $$(./nexus c452 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c452 $$(./oracle c452 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c452 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c452 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c453 $$(./nexus c453 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c453 $$(./oracle c453 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c453 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c453 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c454 $$(./nexus c454 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c454 $$(./oracle c454 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c454 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c454 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c455 $$(./nexus c455 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c455 $$(./oracle c455 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c455 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c455 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c456 $$(./nexus c456 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c456 $$(./oracle c456 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c456 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c456 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c457 $$(./nexus c457 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c457 $$(./oracle c457 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c457 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c457 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c458 $$(./nexus c458 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c458 $$(./oracle c458 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c458 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c458 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c459 $$(./nexus c459 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c459 $$(./oracle c459 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c459 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c459 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c460 $$(./nexus c460 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c460 $$(./oracle c460 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c460 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c460 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c461 $$(./nexus c461 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c461 $$(./oracle c461 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c461 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c461 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c462 $$(./nexus c462 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c462 $$(./oracle c462 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c462 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c462 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c463 $$(./nexus c463 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c463 $$(./oracle c463 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c463 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c463 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c464 $$(./nexus c464 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c464 $$(./oracle c464 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c464 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c464 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c465 $$(./nexus c465 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c465 $$(./oracle c465 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c465 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c465 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c466 $$(./nexus c466 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c466 $$(./oracle c466 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c466 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c466 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c467 $$(./nexus c467 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c467 $$(./oracle c467 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c467 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c467 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c468 $$(./nexus c468 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c468 $$(./oracle c468 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c468 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c468 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c469 $$(./nexus c469 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c469 $$(./oracle c469 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c469 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c469 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c470 $$(./nexus c470 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c470 $$(./oracle c470 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c470 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c470 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c471 $$(./nexus c471 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c471 $$(./oracle c471 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c471 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c471 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c472 $$(./nexus c472 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c472 $$(./oracle c472 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c472 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c472 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c473 $$(./nexus c473 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c473 $$(./oracle c473 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c473 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c473 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c474 $$(./nexus c474 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c474 $$(./oracle c474 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c474 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c474 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c475 $$(./nexus c475 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c475 $$(./oracle c475 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c475 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c475 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c476 $$(./nexus c476 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c476 $$(./oracle c476 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c476 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c476 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c477 $$(./nexus c477 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c477 $$(./oracle c477 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c477 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c477 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c478 $$(./nexus c478 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c478 $$(./oracle c478 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c478 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c478 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c479 $$(./nexus c479 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c479 $$(./oracle c479 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c479 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c479 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c480 $$(./nexus c480 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c480 $$(./oracle c480 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c480 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c480 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c481 $$(./nexus c481 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c481 $$(./oracle c481 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c481 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c481 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c482 $$(./nexus c482 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c482 $$(./oracle c482 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c482 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c482 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c483 $$(./nexus c483 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c483 $$(./oracle c483 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c483 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c483 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c484 $$(./nexus c484 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c484 $$(./oracle c484 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c484 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c484 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c485 $$(./nexus c485 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c485 $$(./oracle c485 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c485 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c485 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c486 $$(./nexus c486 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c486 $$(./oracle c486 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c486 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c486 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c487 $$(./nexus c487 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c487 $$(./oracle c487 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c487 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c487 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c488 $$(./nexus c488 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c488 $$(./oracle c488 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c488 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c488 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c489 $$(./nexus c489 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c489 $$(./oracle c489 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c489 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c489 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c490 $$(./nexus c490 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c490 $$(./oracle c490 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c490 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c490 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c491 $$(./nexus c491 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c491 $$(./oracle c491 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c491 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c491 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c492 $$(./nexus c492 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c492 $$(./oracle c492 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c492 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c492 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c493 $$(./nexus c493 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c493 $$(./oracle c493 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c493 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c493 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c494 $$(./nexus c494 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c494 $$(./oracle c494 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c494 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c494 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c495 $$(./nexus c495 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c495 $$(./oracle c495 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c495 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c495 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c496 $$(./nexus c496 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c496 $$(./oracle c496 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c496 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c496 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c497 $$(./nexus c497 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c497 $$(./oracle c497 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c497 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c497 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c498 $$(./nexus c498 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c498 $$(./oracle c498 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c498 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c498 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c499 $$(./nexus c499 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c499 $$(./oracle c499 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c499 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c499 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c500 $$(./nexus c500 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c500 $$(./oracle c500 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c500 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c500 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c501 $$(./nexus c501 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c501 $$(./oracle c501 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c501 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c501 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c502 $$(./nexus c502 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c502 $$(./oracle c502 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c502 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c502 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c503 $$(./nexus c503 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c503 $$(./oracle c503 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c503 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c503 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c504 $$(./nexus c504 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c504 $$(./oracle c504 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c504 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c504 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c505 $$(./nexus c505 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c505 $$(./oracle c505 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c505 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c505 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c506 $$(./nexus c506 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c506 $$(./oracle c506 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c506 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c506 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c507 $$(./nexus c507 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c507 $$(./oracle c507 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c507 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c507 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c508 $$(./nexus c508 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c508 $$(./oracle c508 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c508 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c508 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c509 $$(./nexus c509 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c509 $$(./oracle c509 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c509 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c509 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c510 $$(./nexus c510 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c510 $$(./oracle c510 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c510 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c510 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c511 $$(./nexus c511 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c511 $$(./oracle c511 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c511 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c511 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c512 $$(./nexus c512 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c512 $$(./oracle c512 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c512 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c512 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c513 $$(./nexus c513 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c513 $$(./oracle c513 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c513 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c513 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c514 $$(./nexus c514 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c514 $$(./oracle c514 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c514 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c514 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c515 $$(./nexus c515 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c515 $$(./oracle c515 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c515 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c515 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c516 $$(./nexus c516 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c516 $$(./oracle c516 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c516 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c516 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c517 $$(./nexus c517 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c517 $$(./oracle c517 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c517 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c517 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c518 $$(./nexus c518 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c518 $$(./oracle c518 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c518 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c518 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c519 $$(./nexus c519 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c519 $$(./oracle c519 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c519 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c519 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c520 $$(./nexus c520 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c520 $$(./oracle c520 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c520 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c520 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c521 $$(./nexus c521 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c521 $$(./oracle c521 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c521 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c521 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c522 $$(./nexus c522 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c522 $$(./oracle c522 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c522 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c522 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c523 $$(./nexus c523 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c523 $$(./oracle c523 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c523 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c523 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c524 $$(./nexus c524 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c524 $$(./oracle c524 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c524 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c524 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c525 $$(./nexus c525 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c525 $$(./oracle c525 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c525 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c525 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c526 $$(./nexus c526 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c526 $$(./oracle c526 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c526 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c526 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c527 $$(./nexus c527 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c527 $$(./oracle c527 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c527 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c527 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c528 $$(./nexus c528 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c528 $$(./oracle c528 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c528 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c528 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c529 $$(./nexus c529 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c529 $$(./oracle c529 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c529 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c529 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c530 $$(./nexus c530 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c530 $$(./oracle c530 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c530 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c530 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c531 $$(./nexus c531 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c531 $$(./oracle c531 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c531 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c531 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c532 $$(./nexus c532 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c532 $$(./oracle c532 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c532 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c532 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c533 $$(./nexus c533 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c533 $$(./oracle c533 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c533 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c533 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c534 $$(./nexus c534 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c534 $$(./oracle c534 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c534 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c534 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c535 $$(./nexus c535 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c535 $$(./oracle c535 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c535 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c535 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c536 $$(./nexus c536 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c536 $$(./oracle c536 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c536 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c536 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c537 $$(./nexus c537 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c537 $$(./oracle c537 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c537 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c537 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c538 $$(./nexus c538 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c538 $$(./oracle c538 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c538 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c538 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c539 $$(./nexus c539 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c539 $$(./oracle c539 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c539 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c539 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c540 $$(./nexus c540 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c540 $$(./oracle c540 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c540 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c540 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c541 $$(./nexus c541 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c541 $$(./oracle c541 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c541 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c541 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c542 $$(./nexus c542 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c542 $$(./oracle c542 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c542 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c542 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c543 $$(./nexus c543 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c543 $$(./oracle c543 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c543 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c543 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c544 $$(./nexus c544 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c544 $$(./oracle c544 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c544 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c544 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c545 $$(./nexus c545 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c545 $$(./oracle c545 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c545 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c545 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c546 $$(./nexus c546 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c546 $$(./oracle c546 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c546 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c546 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c547 $$(./nexus c547 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c547 $$(./oracle c547 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c547 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c547 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c548 $$(./nexus c548 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c548 $$(./oracle c548 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c548 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c548 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c549 $$(./nexus c549 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c549 $$(./oracle c549 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c549 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c549 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c550 $$(./nexus c550 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c550 $$(./oracle c550 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c550 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c550 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c551 $$(./nexus c551 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c551 $$(./oracle c551 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c551 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c551 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c552 $$(./nexus c552 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c552 $$(./oracle c552 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c552 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c552 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c553 $$(./nexus c553 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c553 $$(./oracle c553 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c553 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c553 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c554 $$(./nexus c554 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c554 $$(./oracle c554 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c554 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c554 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c555 $$(./nexus c555 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c555 $$(./oracle c555 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c555 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c555 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c556 $$(./nexus c556 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c556 $$(./oracle c556 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c556 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c556 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c557 $$(./nexus c557 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c557 $$(./oracle c557 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c557 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c557 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c558 $$(./nexus c558 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c558 $$(./oracle c558 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c558 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c558 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c559 $$(./nexus c559 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c559 $$(./oracle c559 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c559 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c559 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c560 $$(./nexus c560 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c560 $$(./oracle c560 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c560 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c560 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c561 $$(./nexus c561 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c561 $$(./oracle c561 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c561 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c561 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c562 $$(./nexus c562 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c562 $$(./oracle c562 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c562 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c562 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c563 $$(./nexus c563 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c563 $$(./oracle c563 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c563 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c563 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c564 $$(./nexus c564 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c564 $$(./oracle c564 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c564 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c564 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c565 $$(./nexus c565 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c565 $$(./oracle c565 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c565 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c565 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c566 $$(./nexus c566 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c566 $$(./oracle c566 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c566 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c566 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c567 $$(./nexus c567 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c567 $$(./oracle c567 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c567 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c567 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c568 $$(./nexus c568 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c568 $$(./oracle c568 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c568 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c568 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c569 $$(./nexus c569 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c569 $$(./oracle c569 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c569 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c569 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c570 $$(./nexus c570 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c570 $$(./oracle c570 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c570 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c570 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c571 $$(./nexus c571 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c571 $$(./oracle c571 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c571 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c571 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c572 $$(./nexus c572 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c572 $$(./oracle c572 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c572 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c572 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c573 $$(./nexus c573 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c573 $$(./oracle c573 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c573 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c573 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c574 $$(./nexus c574 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c574 $$(./oracle c574 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c574 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c574 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c575 $$(./nexus c575 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c575 $$(./oracle c575 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c575 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c575 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c576 $$(./nexus c576 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c576 $$(./oracle c576 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c576 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c576 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c577 $$(./nexus c577 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c577 $$(./oracle c577 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c577 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c577 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c578 $$(./nexus c578 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c578 $$(./oracle c578 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c578 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c578 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c579 $$(./nexus c579 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c579 $$(./oracle c579 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c579 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c579 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c580 $$(./nexus c580 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c580 $$(./oracle c580 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c580 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c580 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c581 $$(./nexus c581 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c581 $$(./oracle c581 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c581 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c581 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c582 $$(./nexus c582 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c582 $$(./oracle c582 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c582 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c582 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c583 $$(./nexus c583 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c583 $$(./oracle c583 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c583 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c583 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c584 $$(./nexus c584 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c584 $$(./oracle c584 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c584 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c584 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c585 $$(./nexus c585 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c585 $$(./oracle c585 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c585 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c585 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c586 $$(./nexus c586 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c586 $$(./oracle c586 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c586 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c586 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c587 $$(./nexus c587 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c587 $$(./oracle c587 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c587 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c587 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c588 $$(./nexus c588 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c588 $$(./oracle c588 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c588 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c588 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c589 $$(./nexus c589 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c589 $$(./oracle c589 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c589 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c589 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c590 $$(./nexus c590 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c590 $$(./oracle c590 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c590 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c590 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c591 $$(./nexus c591 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c591 $$(./oracle c591 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c591 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c591 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c592 $$(./nexus c592 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c592 $$(./oracle c592 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c592 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c592 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c593 $$(./nexus c593 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c593 $$(./oracle c593 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c593 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c593 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c594 $$(./nexus c594 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c594 $$(./oracle c594 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c594 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c594 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c595 $$(./nexus c595 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c595 $$(./oracle c595 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c595 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c595 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c596 $$(./nexus c596 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c596 $$(./oracle c596 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c596 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c596 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c597 $$(./nexus c597 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c597 $$(./oracle c597 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c597 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c597 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c598 $$(./nexus c598 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c598 $$(./oracle c598 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c598 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c598 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c599 $$(./nexus c599 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c599 $$(./oracle c599 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c599 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c599 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c600 $$(./nexus c600 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c600 $$(./oracle c600 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c600 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c600 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c601 $$(./nexus c601 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c601 $$(./oracle c601 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c601 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c601 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c602 $$(./nexus c602 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c602 $$(./oracle c602 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c602 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c602 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c603 $$(./nexus c603 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c603 $$(./oracle c603 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c603 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c603 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c604 $$(./nexus c604 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c604 $$(./oracle c604 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c604 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c604 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c605 $$(./nexus c605 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c605 $$(./oracle c605 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c605 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c605 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c606 $$(./nexus c606 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c606 $$(./oracle c606 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c606 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c606 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c607 $$(./nexus c607 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c607 $$(./oracle c607 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c607 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c607 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c608 $$(./nexus c608 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c608 $$(./oracle c608 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c608 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c608 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c609 $$(./nexus c609 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c609 $$(./oracle c609 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c609 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c609 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c610 $$(./nexus c610 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c610 $$(./oracle c610 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c610 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c610 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c611 $$(./nexus c611 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c611 $$(./oracle c611 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c611 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c611 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c612 $$(./nexus c612 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c612 $$(./oracle c612 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c612 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c612 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c613 $$(./nexus c613 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c613 $$(./oracle c613 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c613 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c613 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c614 $$(./nexus c614 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c614 $$(./oracle c614 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c614 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c614 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c615 $$(./nexus c615 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c615 $$(./oracle c615 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c615 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c615 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c616 $$(./nexus c616 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c616 $$(./oracle c616 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c616 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c616 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c617 $$(./nexus c617 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c617 $$(./oracle c617 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c617 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c617 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c618 $$(./nexus c618 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c618 $$(./oracle c618 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c618 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c618 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c619 $$(./nexus c619 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c619 $$(./oracle c619 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c619 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c619 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c620 $$(./nexus c620 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c620 $$(./oracle c620 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c620 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c620 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c621 $$(./nexus c621 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c621 $$(./oracle c621 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c621 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c621 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c622 $$(./nexus c622 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c622 $$(./oracle c622 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c622 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c622 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c623 $$(./nexus c623 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c623 $$(./oracle c623 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c623 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c623 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c624 $$(./nexus c624 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c624 $$(./oracle c624 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c624 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c624 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c625 $$(./nexus c625 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c625 $$(./oracle c625 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c625 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c625 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c626 $$(./nexus c626 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c626 $$(./oracle c626 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c626 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c626 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c627 $$(./nexus c627 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c627 $$(./oracle c627 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c627 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c627 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c628 $$(./nexus c628 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c628 $$(./oracle c628 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c628 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c628 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c629 $$(./nexus c629 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c629 $$(./oracle c629 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c629 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c629 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c630 $$(./nexus c630 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c630 $$(./oracle c630 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c630 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c630 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c631 $$(./nexus c631 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c631 $$(./oracle c631 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c631 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c631 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c632 $$(./nexus c632 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c632 $$(./oracle c632 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c632 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c632 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c633 $$(./nexus c633 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c633 $$(./oracle c633 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c633 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c633 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c634 $$(./nexus c634 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c634 $$(./oracle c634 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c634 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c634 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c635 $$(./nexus c635 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c635 $$(./oracle c635 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c635 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c635 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c636 $$(./nexus c636 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c636 $$(./oracle c636 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c636 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c636 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c637 $$(./nexus c637 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c637 $$(./oracle c637 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c637 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c637 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c638 $$(./nexus c638 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c638 $$(./oracle c638 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c638 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c638 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c639 $$(./nexus c639 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c639 $$(./oracle c639 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c639 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c639 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c640 $$(./nexus c640 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c640 $$(./oracle c640 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c640 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c640 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c641 $$(./nexus c641 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c641 $$(./oracle c641 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c641 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c641 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c642 $$(./nexus c642 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c642 $$(./oracle c642 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c642 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c642 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c643 $$(./nexus c643 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c643 $$(./oracle c643 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c643 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c643 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c644 $$(./nexus c644 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c644 $$(./oracle c644 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c644 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c644 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c645 $$(./nexus c645 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c645 $$(./oracle c645 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c645 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c645 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c646 $$(./nexus c646 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c646 $$(./oracle c646 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c646 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c646 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c647 $$(./nexus c647 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c647 $$(./oracle c647 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c647 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c647 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c648 $$(./nexus c648 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c648 $$(./oracle c648 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c648 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c648 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c649 $$(./nexus c649 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c649 $$(./oracle c649 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c649 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c649 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c650 $$(./nexus c650 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c650 $$(./oracle c650 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c650 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c650 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c651 $$(./nexus c651 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c651 $$(./oracle c651 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c651 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c651 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c652 $$(./nexus c652 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c652 $$(./oracle c652 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c652 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c652 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c653 $$(./nexus c653 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c653 $$(./oracle c653 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c653 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c653 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c654 $$(./nexus c654 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c654 $$(./oracle c654 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c654 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c654 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c655 $$(./nexus c655 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c655 $$(./oracle c655 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c655 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c655 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c656 $$(./nexus c656 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c656 $$(./oracle c656 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c656 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c656 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c657 $$(./nexus c657 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c657 $$(./oracle c657 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c657 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c657 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c658 $$(./nexus c658 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c658 $$(./oracle c658 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c658 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c658 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c659 $$(./nexus c659 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c659 $$(./oracle c659 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c659 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c659 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c660 $$(./nexus c660 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c660 $$(./oracle c660 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c660 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c660 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c661 $$(./nexus c661 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c661 $$(./oracle c661 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c661 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c661 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c662 $$(./nexus c662 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c662 $$(./oracle c662 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c662 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c662 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c663 $$(./nexus c663 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c663 $$(./oracle c663 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c663 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c663 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c664 $$(./nexus c664 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c664 $$(./oracle c664 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c664 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c664 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c665 $$(./nexus c665 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c665 $$(./oracle c665 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c665 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c665 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c666 $$(./nexus c666 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c666 $$(./oracle c666 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c666 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c666 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c667 $$(./nexus c667 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c667 $$(./oracle c667 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c667 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c667 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c668 $$(./nexus c668 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c668 $$(./oracle c668 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c668 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c668 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c669 $$(./nexus c669 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c669 $$(./oracle c669 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c669 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c669 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c670 $$(./nexus c670 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c670 $$(./oracle c670 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c670 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c670 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c671 $$(./nexus c671 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c671 $$(./oracle c671 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c671 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c671 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c672 $$(./nexus c672 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c672 $$(./oracle c672 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c672 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c672 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c673 $$(./nexus c673 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c673 $$(./oracle c673 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c673 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c673 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c674 $$(./nexus c674 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c674 $$(./oracle c674 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c674 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c674 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c675 $$(./nexus c675 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c675 $$(./oracle c675 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c675 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c675 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c676 $$(./nexus c676 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c676 $$(./oracle c676 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c676 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c676 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c677 $$(./nexus c677 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c677 $$(./oracle c677 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c677 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c677 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c678 $$(./nexus c678 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c678 $$(./oracle c678 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c678 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c678 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c679 $$(./nexus c679 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c679 $$(./oracle c679 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c679 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c679 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c680 $$(./nexus c680 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c680 $$(./oracle c680 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c680 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c680 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c681 $$(./nexus c681 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c681 $$(./oracle c681 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c681 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c681 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c682 $$(./nexus c682 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c682 $$(./oracle c682 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c682 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c682 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c683 $$(./nexus c683 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c683 $$(./oracle c683 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c683 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c683 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c684 $$(./nexus c684 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c684 $$(./oracle c684 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c684 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c684 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c685 $$(./nexus c685 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c685 $$(./oracle c685 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c685 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c685 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c686 $$(./nexus c686 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c686 $$(./oracle c686 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c686 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c686 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c687 $$(./nexus c687 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c687 $$(./oracle c687 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c687 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c687 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c688 $$(./nexus c688 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c688 $$(./oracle c688 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c688 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c688 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c689 $$(./nexus c689 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c689 $$(./oracle c689 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c689 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c689 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c690 $$(./nexus c690 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c690 $$(./oracle c690 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c690 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c690 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c691 $$(./nexus c691 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c691 $$(./oracle c691 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c691 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c691 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c692 $$(./nexus c692 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c692 $$(./oracle c692 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c692 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c692 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c693 $$(./nexus c693 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c693 $$(./oracle c693 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c693 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c693 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c694 $$(./nexus c694 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c694 $$(./oracle c694 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c694 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c694 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c695 $$(./nexus c695 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c695 $$(./oracle c695 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c695 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c695 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c696 $$(./nexus c696 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c696 $$(./oracle c696 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c696 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c696 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c697 $$(./nexus c697 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c697 $$(./oracle c697 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c697 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c697 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c698 $$(./nexus c698 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c698 $$(./oracle c698 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c698 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c698 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c699 $$(./nexus c699 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c699 $$(./oracle c699 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c699 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c699 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c700 $$(./nexus c700 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c700 $$(./oracle c700 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c700 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c700 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c701 $$(./nexus c701 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c701 $$(./oracle c701 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c701 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c701 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c702 $$(./nexus c702 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c702 $$(./oracle c702 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c702 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c702 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c703 $$(./nexus c703 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c703 $$(./oracle c703 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c703 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c703 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c704 $$(./nexus c704 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c704 $$(./oracle c704 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c704 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c704 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c705 $$(./nexus c705 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c705 $$(./oracle c705 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c705 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c705 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c706 $$(./nexus c706 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c706 $$(./oracle c706 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c706 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c706 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c707 $$(./nexus c707 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c707 $$(./oracle c707 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c707 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c707 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c708 $$(./nexus c708 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c708 $$(./oracle c708 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c708 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c708 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c709 $$(./nexus c709 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c709 $$(./oracle c709 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c709 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c709 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c710 $$(./nexus c710 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c710 $$(./oracle c710 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c710 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c710 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c711 $$(./nexus c711 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c711 $$(./oracle c711 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c711 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c711 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c712 $$(./nexus c712 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c712 $$(./oracle c712 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c712 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c712 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c713 $$(./nexus c713 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c713 $$(./oracle c713 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c713 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c713 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c714 $$(./nexus c714 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c714 $$(./oracle c714 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c714 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c714 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c715 $$(./nexus c715 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c715 $$(./oracle c715 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c715 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c715 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c716 $$(./nexus c716 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c716 $$(./oracle c716 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c716 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c716 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c717 $$(./nexus c717 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c717 $$(./oracle c717 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c717 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c717 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c718 $$(./nexus c718 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c718 $$(./oracle c718 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c718 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c718 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c719 $$(./nexus c719 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c719 $$(./oracle c719 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c719 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c719 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c720 $$(./nexus c720 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c720 $$(./oracle c720 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c720 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c720 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c721 $$(./nexus c721 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c721 $$(./oracle c721 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c721 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c721 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c722 $$(./nexus c722 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c722 $$(./oracle c722 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c722 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c722 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c723 $$(./nexus c723 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c723 $$(./oracle c723 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c723 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c723 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c724 $$(./nexus c724 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c724 $$(./oracle c724 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c724 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c724 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c725 $$(./nexus c725 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c725 $$(./oracle c725 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c725 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c725 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c726 $$(./nexus c726 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c726 $$(./oracle c726 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c726 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c726 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c727 $$(./nexus c727 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c727 $$(./oracle c727 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c727 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c727 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c728 $$(./nexus c728 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c728 $$(./oracle c728 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c728 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c728 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c729 $$(./nexus c729 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c729 $$(./oracle c729 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c729 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c729 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c730 $$(./nexus c730 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c730 $$(./oracle c730 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c730 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c730 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c731 $$(./nexus c731 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c731 $$(./oracle c731 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c731 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c731 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c732 $$(./nexus c732 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c732 $$(./oracle c732 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c732 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c732 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c733 $$(./nexus c733 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c733 $$(./oracle c733 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c733 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c733 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c734 $$(./nexus c734 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c734 $$(./oracle c734 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c734 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c734 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c735 $$(./nexus c735 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c735 $$(./oracle c735 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c735 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c735 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c736 $$(./nexus c736 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c736 $$(./oracle c736 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c736 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c736 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c737 $$(./nexus c737 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c737 $$(./oracle c737 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c737 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c737 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c738 $$(./nexus c738 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c738 $$(./oracle c738 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c738 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c738 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c739 $$(./nexus c739 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c739 $$(./oracle c739 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c739 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c739 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c740 $$(./nexus c740 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c740 $$(./oracle c740 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c740 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c740 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c741 $$(./nexus c741 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c741 $$(./oracle c741 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c741 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c741 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c742 $$(./nexus c742 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c742 $$(./oracle c742 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c742 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c742 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c743 $$(./nexus c743 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c743 $$(./oracle c743 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c743 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c743 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c744 $$(./nexus c744 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c744 $$(./oracle c744 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c744 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c744 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c745 $$(./nexus c745 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c745 $$(./oracle c745 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c745 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c745 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c746 $$(./nexus c746 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c746 $$(./oracle c746 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c746 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c746 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c747 $$(./nexus c747 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c747 $$(./oracle c747 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c747 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c747 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c748 $$(./nexus c748 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c748 $$(./oracle c748 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c748 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c748 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c749 $$(./nexus c749 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c749 $$(./oracle c749 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c749 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c749 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c750 $$(./nexus c750 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c750 $$(./oracle c750 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c750 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c750 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c751 $$(./nexus c751 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c751 $$(./oracle c751 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c751 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c751 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c752 $$(./nexus c752 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c752 $$(./oracle c752 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c752 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c752 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c753 $$(./nexus c753 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c753 $$(./oracle c753 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c753 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c753 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c754 $$(./nexus c754 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c754 $$(./oracle c754 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c754 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c754 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c755 $$(./nexus c755 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c755 $$(./oracle c755 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c755 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c755 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c756 $$(./nexus c756 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c756 $$(./oracle c756 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c756 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c756 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c757 $$(./nexus c757 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c757 $$(./oracle c757 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c757 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c757 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c758 $$(./nexus c758 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c758 $$(./oracle c758 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c758 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c758 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c759 $$(./nexus c759 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c759 $$(./oracle c759 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c759 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c759 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c760 $$(./nexus c760 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c760 $$(./oracle c760 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c760 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c760 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c761 $$(./nexus c761 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c761 $$(./oracle c761 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c761 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c761 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c762 $$(./nexus c762 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c762 $$(./oracle c762 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c762 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c762 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c763 $$(./nexus c763 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c763 $$(./oracle c763 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c763 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c763 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c764 $$(./nexus c764 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c764 $$(./oracle c764 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c764 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c764 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c765 $$(./nexus c765 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c765 $$(./oracle c765 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c765 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c765 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c766 $$(./nexus c766 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c766 $$(./oracle c766 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c766 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c766 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c767 $$(./nexus c767 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c767 $$(./oracle c767 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c767 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c767 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c768 $$(./nexus c768 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c768 $$(./oracle c768 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c768 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c768 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c769 $$(./nexus c769 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c769 $$(./oracle c769 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c769 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c769 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c770 $$(./nexus c770 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c770 $$(./oracle c770 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c770 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c770 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c771 $$(./nexus c771 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c771 $$(./oracle c771 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c771 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c771 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c772 $$(./nexus c772 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c772 $$(./oracle c772 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c772 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c772 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c773 $$(./nexus c773 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c773 $$(./oracle c773 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c773 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c773 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c774 $$(./nexus c774 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c774 $$(./oracle c774 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c774 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c774 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c775 $$(./nexus c775 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c775 $$(./oracle c775 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c775 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c775 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c776 $$(./nexus c776 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c776 $$(./oracle c776 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c776 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c776 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c777 $$(./nexus c777 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c777 $$(./oracle c777 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c777 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c777 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c778 $$(./nexus c778 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c778 $$(./oracle c778 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c778 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c778 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c779 $$(./nexus c779 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c779 $$(./oracle c779 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c779 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c779 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c780 $$(./nexus c780 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c780 $$(./oracle c780 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "c780 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c780 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c781 $$(./nexus c781 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c781 $$(./oracle c781 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "c781 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c781 7 0 2>> obs.err || echo failed)" >> asan.txt
	python3 check.py run.txt oracle.txt asan.txt > obs.txt

clean:
	rm -rf nexus oracle asan pools __pycache__ *-build *build.tmp*
	rm -f *.err run.txt oracle.txt asan.txt
