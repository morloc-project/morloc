all:
	rm -f *.err obs.txt run.txt asan.txt
	morloc make -o nexus main.loc 2> build.err
	CXX="$${CXX:-g++} -fsanitize=address,undefined -fno-sanitize-recover=all" morloc make -o asan main.loc 2> asan-build.err
	echo "c000-j1 $$(./nexus c000 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c000-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c000 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c000-j0 $$(./nexus c000 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c000-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c000 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c001-j1 $$(./nexus c001 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c001-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c001 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c001-j0 $$(./nexus c001 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c001-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c001 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c002-j1 $$(./nexus c002 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c002-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c002 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c002-j0 $$(./nexus c002 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c002-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c002 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c003-j1 $$(./nexus c003 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c003-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c003 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c003-j0 $$(./nexus c003 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c003-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c003 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c004-j1 $$(./nexus c004 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c004-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c004 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c004-j0 $$(./nexus c004 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c004-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c004 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c005-j1 $$(./nexus c005 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c005-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c005 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c005-j0 $$(./nexus c005 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c005-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c005 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c006-j1 $$(./nexus c006 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c006-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c006 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c006-j0 $$(./nexus c006 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c006-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c006 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c007-j1 $$(./nexus c007 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c007-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c007 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c007-j0 $$(./nexus c007 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c007-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c007 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c008-j1 $$(./nexus c008 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c008-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c008 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c008-j0 $$(./nexus c008 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c008-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c008 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c009-j1 $$(./nexus c009 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c009-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c009 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c009-j0 $$(./nexus c009 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c009-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c009 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c010-j1 $$(./nexus c010 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c010-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c010 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c010-j0 $$(./nexus c010 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c010-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c010 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c011-j1 $$(./nexus c011 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c011-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c011 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c011-j0 $$(./nexus c011 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c011-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c011 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c012-j1 $$(./nexus c012 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c012-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c012 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c012-j0 $$(./nexus c012 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c012-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c012 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c013-j1 $$(./nexus c013 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c013-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c013 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c013-j0 $$(./nexus c013 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c013-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c013 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c014-j1 $$(./nexus c014 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c014-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c014 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c014-j0 $$(./nexus c014 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c014-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c014 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c015-j1 $$(./nexus c015 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c015-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c015 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c016-j1 $$(./nexus c016 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c016-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c016 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c016-j0 $$(./nexus c016 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c016-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c016 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c017-j1 $$(./nexus c017 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c017-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c017 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c018-j1 $$(./nexus c018 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c018-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c018 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c019-j1 $$(./nexus c019 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c019-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c019 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c020-j1 $$(./nexus c020 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c020-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c020 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c021-j1 $$(./nexus c021 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c021-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c021 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c022-j1 $$(./nexus c022 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c022-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c022 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c023-j1 $$(./nexus c023 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c023-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c023 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c024-j1 $$(./nexus c024 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c024-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c024 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c025-j1 $$(./nexus c025 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c025-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c025 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c026-j1 $$(./nexus c026 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c026-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c026 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c027-j1 $$(./nexus c027 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c027-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c027 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c028-j1 $$(./nexus c028 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c028-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c028 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c029-j1 $$(./nexus c029 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c029-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c029 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c030-j1 $$(./nexus c030 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c030-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c030 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c030-j0 $$(./nexus c030 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c030-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c030 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c031-j1 $$(./nexus c031 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c031-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c031 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c032-j1 $$(./nexus c032 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c032-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c032 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c032-j0 $$(./nexus c032 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c032-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c032 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c033-j1 $$(./nexus c033 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c033-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c033 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c034-j1 $$(./nexus c034 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c034-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c034 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c035-j1 $$(./nexus c035 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c035-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c035 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c036-j1 $$(./nexus c036 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c036-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c036 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c037-j1 $$(./nexus c037 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c037-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c037 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c038-j1 $$(./nexus c038 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c038-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c038 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c039-j1 $$(./nexus c039 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c039-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c039 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c040-j1 $$(./nexus c040 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c040-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c040 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c041-j1 $$(./nexus c041 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c041-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c041 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c042-j1 $$(./nexus c042 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c042-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c042 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c043-j1 $$(./nexus c043 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c043-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c043 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c044-j1 $$(./nexus c044 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c044-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c044 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c045-j1 $$(./nexus c045 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c045-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c045 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c046-j1 $$(./nexus c046 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c046-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c046 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c046-j0 $$(./nexus c046 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c046-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c046 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c047-j1 $$(./nexus c047 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c047-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c047 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c048-j1 $$(./nexus c048 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c048-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c048 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c048-j0 $$(./nexus c048 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c048-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c048 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c049-j1 $$(./nexus c049 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c049-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c049 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c050-j1 $$(./nexus c050 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c050-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c050 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c051-j1 $$(./nexus c051 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c051-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c051 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c052-j1 $$(./nexus c052 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c052-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c052 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c053-j1 $$(./nexus c053 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c053-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c053 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c054-j1 $$(./nexus c054 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c054-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c054 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c055-j1 $$(./nexus c055 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c055-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c055 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c056-j1 $$(./nexus c056 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c056-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c056 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c057-j1 $$(./nexus c057 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c057-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c057 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c058-j1 $$(./nexus c058 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c058-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c058 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c059-j1 $$(./nexus c059 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c059-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c059 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c060-j1 $$(./nexus c060 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c060-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c060 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c061-j1 $$(./nexus c061 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c061-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c061 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c062-j1 $$(./nexus c062 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c062-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c062 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c062-j0 $$(./nexus c062 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c062-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c062 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c063-j1 $$(./nexus c063 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c063-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c063 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c064-j1 $$(./nexus c064 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c064-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c064 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c064-j0 $$(./nexus c064 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c064-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c064 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c065-j1 $$(./nexus c065 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c065-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c065 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c066-j1 $$(./nexus c066 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c066-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c066 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c067-j1 $$(./nexus c067 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c067-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c067 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c068-j1 $$(./nexus c068 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c068-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c068 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c069-j1 $$(./nexus c069 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c069-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c069 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c070-j1 $$(./nexus c070 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c070-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c070 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c070-j0 $$(./nexus c070 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c070-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c070 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c071-j1 $$(./nexus c071 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c071-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c071 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c071-j0 $$(./nexus c071 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c071-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c071 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c072-j1 $$(./nexus c072 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c072-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c072 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c072-j0 $$(./nexus c072 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c072-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c072 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c073-j1 $$(./nexus c073 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c073-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c073 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c073-j0 $$(./nexus c073 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c073-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c073 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c074-j1 $$(./nexus c074 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c074-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c074 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c074-j0 $$(./nexus c074 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c074-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c074 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c075-j1 $$(./nexus c075 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c075-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c075 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c075-j0 $$(./nexus c075 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c075-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c075 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c076-j1 $$(./nexus c076 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c076-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c076 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c076-j0 $$(./nexus c076 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c076-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c076 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c077-j1 $$(./nexus c077 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c077-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c077 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c077-j0 $$(./nexus c077 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c077-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c077 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c078-j1 $$(./nexus c078 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c078-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c078 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c078-j0 $$(./nexus c078 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c078-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c078 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c079-j1 $$(./nexus c079 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c079-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c079 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c079-j0 $$(./nexus c079 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c079-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c079 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c080-j1 $$(./nexus c080 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c080-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c080 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c080-j0 $$(./nexus c080 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c080-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c080 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c081-j1 $$(./nexus c081 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c081-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c081 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c081-j0 $$(./nexus c081 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c081-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c081 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c082-j1 $$(./nexus c082 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c082-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c082 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c082-j0 $$(./nexus c082 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c082-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c082 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c083-j1 $$(./nexus c083 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c083-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c083 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c083-j0 $$(./nexus c083 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c083-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c083 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c084-j1 $$(./nexus c084 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c084-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c084 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c084-j0 $$(./nexus c084 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c084-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c084 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c085-j1 $$(./nexus c085 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c085-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c085 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c085-j0 $$(./nexus c085 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c085-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c085 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c086-j1 $$(./nexus c086 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c086-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c086 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c086-j0 $$(./nexus c086 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c086-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c086 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c087-j1 $$(./nexus c087 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c087-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c087 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c087-j0 $$(./nexus c087 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c087-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c087 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c088-j1 $$(./nexus c088 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c088-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c088 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c088-j0 $$(./nexus c088 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c088-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c088 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c089-j1 $$(./nexus c089 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c089-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c089 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c089-j0 $$(./nexus c089 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c089-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c089 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c090-j1 $$(./nexus c090 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c090-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c090 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c090-j0 $$(./nexus c090 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c090-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c090 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c091-j1 $$(./nexus c091 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c091-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c091 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c091-j0 $$(./nexus c091 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c091-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c091 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c092-j1 $$(./nexus c092 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c092-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c092 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c092-j0 $$(./nexus c092 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c092-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c092 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c093-j1 $$(./nexus c093 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c093-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c093 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c093-j0 $$(./nexus c093 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c093-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c093 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c094-j1 $$(./nexus c094 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c094-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c094 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c094-j0 $$(./nexus c094 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c094-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c094 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c095-j1 $$(./nexus c095 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c095-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c095 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c095-j0 $$(./nexus c095 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c095-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c095 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c096-j1 $$(./nexus c096 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c096-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c096 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c096-j0 $$(./nexus c096 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c096-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c096 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c097-j1 $$(./nexus c097 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c097-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c097 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c097-j0 $$(./nexus c097 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c097-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c097 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c098-j1 $$(./nexus c098 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c098-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c098 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c098-j0 $$(./nexus c098 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c098-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c098 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c099-j1 $$(./nexus c099 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c099-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c099 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c099-j0 $$(./nexus c099 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c099-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c099 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c100-j1 $$(./nexus c100 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c100-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c100 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c100-j0 $$(./nexus c100 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c100-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c100 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c101-j1 $$(./nexus c101 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c101-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c101 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c101-j0 $$(./nexus c101 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c101-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c101 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c102-j1 $$(./nexus c102 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c102-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c102 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c102-j0 $$(./nexus c102 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c102-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c102 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c103-j1 $$(./nexus c103 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c103-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c103 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c103-j0 $$(./nexus c103 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c103-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c103 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c104-j1 $$(./nexus c104 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c104-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c104 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c104-j0 $$(./nexus c104 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c104-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c104 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c105-j1 $$(./nexus c105 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c105-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c105 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c105-j0 $$(./nexus c105 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c105-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c105 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c106-j1 $$(./nexus c106 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c106-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c106 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c106-j0 $$(./nexus c106 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c106-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c106 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c107-j1 $$(./nexus c107 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c107-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c107 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c107-j0 $$(./nexus c107 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c107-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c107 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c108-j1 $$(./nexus c108 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c108-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c108 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c108-j0 $$(./nexus c108 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c108-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c108 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c109-j1 $$(./nexus c109 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c109-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c109 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c109-j0 $$(./nexus c109 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c109-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c109 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c110-j1 $$(./nexus c110 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c110-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c110 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c110-j0 $$(./nexus c110 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c110-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c110 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c111-j1 $$(./nexus c111 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c111-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c111 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c111-j0 $$(./nexus c111 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c111-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c111 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c112-j1 $$(./nexus c112 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c112-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c112 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c112-j0 $$(./nexus c112 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c112-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c112 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c113-j1 $$(./nexus c113 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c113-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c113 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c113-j0 $$(./nexus c113 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c113-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c113 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c114-j1 $$(./nexus c114 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c114-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c114 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c114-j0 $$(./nexus c114 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c114-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c114 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c115-j1 $$(./nexus c115 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c115-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c115 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c115-j0 $$(./nexus c115 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c115-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c115 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c116-j1 $$(./nexus c116 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c116-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c116 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c117-j1 $$(./nexus c117 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c117-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c117 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c117-j0 $$(./nexus c117 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c117-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c117 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c118-j1 $$(./nexus c118 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c118-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c118 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c119-j1 $$(./nexus c119 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c119-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c119 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c120-j1 $$(./nexus c120 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c120-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c120 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c121-j1 $$(./nexus c121 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c121-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c121 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c122-j1 $$(./nexus c122 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c122-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c122 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c123-j1 $$(./nexus c123 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c123-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c123 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c123-j0 $$(./nexus c123 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c123-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c123 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c124-j1 $$(./nexus c124 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c124-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c124 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c125-j1 $$(./nexus c125 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c125-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c125 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c126-j1 $$(./nexus c126 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c126-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c126 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c127-j1 $$(./nexus c127 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c127-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c127 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c128-j1 $$(./nexus c128 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c128-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c128 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c129-j1 $$(./nexus c129 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c129-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c129 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c130-j1 $$(./nexus c130 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c130-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c130 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c131-j1 $$(./nexus c131 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c131-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c131 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c132-j1 $$(./nexus c132 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c132-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c132 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c133-j1 $$(./nexus c133 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c133-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c133 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c133-j0 $$(./nexus c133 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c133-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c133 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c134-j1 $$(./nexus c134 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c134-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c134 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c135-j1 $$(./nexus c135 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c135-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c135 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c135-j0 $$(./nexus c135 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c135-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c135 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c136-j1 $$(./nexus c136 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c136-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c136 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c137-j1 $$(./nexus c137 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c137-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c137 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c138-j1 $$(./nexus c138 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c138-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c138 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c139-j1 $$(./nexus c139 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c139-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c139 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c140-j1 $$(./nexus c140 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c140-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c140 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c141-j1 $$(./nexus c141 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c141-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c141 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c141-j0 $$(./nexus c141 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c141-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c141 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c142-j1 $$(./nexus c142 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c142-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c142 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c143-j1 $$(./nexus c143 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c143-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c143 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c144-j1 $$(./nexus c144 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c144-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c144 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c145-j1 $$(./nexus c145 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c145-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c145 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c146-j1 $$(./nexus c146 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c146-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c146 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c147-j1 $$(./nexus c147 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c147-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c147 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c148-j1 $$(./nexus c148 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c148-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c148 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c149-j1 $$(./nexus c149 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c149-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c149 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c150-j1 $$(./nexus c150 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c150-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c150 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c151-j1 $$(./nexus c151 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c151-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c151 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c151-j0 $$(./nexus c151 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c151-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c151 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c152-j1 $$(./nexus c152 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c152-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c152 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c153-j1 $$(./nexus c153 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c153-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c153 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c153-j0 $$(./nexus c153 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c153-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c153 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c154-j1 $$(./nexus c154 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c154-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c154 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c155-j1 $$(./nexus c155 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c155-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c155 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c156-j1 $$(./nexus c156 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c156-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c156 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c157-j1 $$(./nexus c157 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c157-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c157 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c158-j1 $$(./nexus c158 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c158-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c158 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c159-j1 $$(./nexus c159 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c159-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c159 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c159-j0 $$(./nexus c159 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c159-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c159 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c160-j1 $$(./nexus c160 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c160-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c160 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c161-j1 $$(./nexus c161 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c161-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c161 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c162-j1 $$(./nexus c162 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c162-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c162 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c163-j1 $$(./nexus c163 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c163-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c163 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c164-j1 $$(./nexus c164 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c164-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c164 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c165-j1 $$(./nexus c165 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c165-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c165 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c166-j1 $$(./nexus c166 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c166-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c166 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c167-j1 $$(./nexus c167 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c167-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c167 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c168-j1 $$(./nexus c168 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c168-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c168 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c169-j1 $$(./nexus c169 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c169-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c169 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c169-j0 $$(./nexus c169 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c169-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c169 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c170-j1 $$(./nexus c170 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c170-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c170 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c171-j1 $$(./nexus c171 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c171-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c171 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c171-j0 $$(./nexus c171 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c171-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c171 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c172-j1 $$(./nexus c172 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c172-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c172 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c173-j1 $$(./nexus c173 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c173-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c173 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c174-j1 $$(./nexus c174 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c174-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c174 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c175-j1 $$(./nexus c175 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c175-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c175 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c176-j1 $$(./nexus c176 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c176-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c176 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c177-j1 $$(./nexus c177 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c177-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c177 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c177-j0 $$(./nexus c177 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c177-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c177 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c178-j1 $$(./nexus c178 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c178-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c178 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c179-j1 $$(./nexus c179 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c179-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c179 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c180-j1 $$(./nexus c180 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c180-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c180 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c181-j1 $$(./nexus c181 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c181-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c181 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c182-j1 $$(./nexus c182 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c182-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c182 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c183-j1 $$(./nexus c183 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c183-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c183 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c184-j1 $$(./nexus c184 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c184-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c184 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c185-j1 $$(./nexus c185 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c185-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c185 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c186-j1 $$(./nexus c186 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c186-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c186 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c187-j1 $$(./nexus c187 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c187-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c187 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c187-j0 $$(./nexus c187 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c187-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c187 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c188-j1 $$(./nexus c188 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c188-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c188 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c189-j1 $$(./nexus c189 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c189-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c189 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c189-j0 $$(./nexus c189 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c189-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c189 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c190-j1 $$(./nexus c190 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c190-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c190 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c191-j1 $$(./nexus c191 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c191-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c191 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c192-j1 $$(./nexus c192 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c192-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c192 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c193-j1 $$(./nexus c193 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c193-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c193 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c194-j1 $$(./nexus c194 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c194-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c194 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c195-j1 $$(./nexus c195 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c195-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c195 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c195-j0 $$(./nexus c195 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c195-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c195 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c196-j1 $$(./nexus c196 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c196-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c196 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c196-j0 $$(./nexus c196 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c196-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c196 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c197-j1 $$(./nexus c197 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c197-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c197 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c197-j0 $$(./nexus c197 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c197-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c197 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c198-j1 $$(./nexus c198 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c198-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c198 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c198-j0 $$(./nexus c198 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c198-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c198 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c199-j1 $$(./nexus c199 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c199-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c199 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c199-j0 $$(./nexus c199 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c199-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c199 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c200-j1 $$(./nexus c200 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c200-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c200 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c200-j0 $$(./nexus c200 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c200-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c200 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c201-j1 $$(./nexus c201 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c201-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c201 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c201-j0 $$(./nexus c201 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c201-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c201 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c202-j1 $$(./nexus c202 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c202-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c202 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c202-j0 $$(./nexus c202 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c202-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c202 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c203-j1 $$(./nexus c203 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c203-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c203 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c203-j0 $$(./nexus c203 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c203-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c203 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c204-j1 $$(./nexus c204 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c204-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c204 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c204-j0 $$(./nexus c204 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c204-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c204 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c205-j1 $$(./nexus c205 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c205-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c205 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c205-j0 $$(./nexus c205 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c205-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c205 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c206-j1 $$(./nexus c206 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c206-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c206 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c206-j0 $$(./nexus c206 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c206-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c206 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c207-j1 $$(./nexus c207 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c207-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c207 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c207-j0 $$(./nexus c207 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c207-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c207 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c208-j1 $$(./nexus c208 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c208-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c208 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c208-j0 $$(./nexus c208 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c208-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c208 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c209-j1 $$(./nexus c209 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c209-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c209 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c209-j0 $$(./nexus c209 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c209-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c209 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c210-j1 $$(./nexus c210 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c210-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c210 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c210-j0 $$(./nexus c210 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c210-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c210 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c211-j1 $$(./nexus c211 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c211-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c211 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c211-j0 $$(./nexus c211 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c211-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c211 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c212-j1 $$(./nexus c212 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c212-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c212 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c213-j1 $$(./nexus c213 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c213-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c213 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c213-j0 $$(./nexus c213 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c213-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c213 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c214-j1 $$(./nexus c214 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c214-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c214 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c215-j1 $$(./nexus c215 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c215-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c215 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c216-j1 $$(./nexus c216 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c216-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c216 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c217-j1 $$(./nexus c217 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c217-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c217 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c218-j1 $$(./nexus c218 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c218-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c218 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c219-j1 $$(./nexus c219 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c219-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c219 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c219-j0 $$(./nexus c219 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c219-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c219 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c220-j1 $$(./nexus c220 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c220-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c220 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c221-j1 $$(./nexus c221 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c221-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c221 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c222-j1 $$(./nexus c222 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c222-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c222 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c223-j1 $$(./nexus c223 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c223-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c223 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c224-j1 $$(./nexus c224 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c224-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c224 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c225-j1 $$(./nexus c225 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c225-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c225 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c226-j1 $$(./nexus c226 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c226-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c226 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c227-j1 $$(./nexus c227 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c227-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c227 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c228-j1 $$(./nexus c228 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c228-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c228 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c229-j1 $$(./nexus c229 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c229-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c229 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c229-j0 $$(./nexus c229 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c229-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c229 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c230-j1 $$(./nexus c230 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c230-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c230 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c231-j1 $$(./nexus c231 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c231-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c231 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c231-j0 $$(./nexus c231 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c231-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c231 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c232-j1 $$(./nexus c232 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c232-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c232 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c233-j1 $$(./nexus c233 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c233-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c233 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c234-j1 $$(./nexus c234 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c234-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c234 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c235-j1 $$(./nexus c235 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c235-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c235 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c236-j1 $$(./nexus c236 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c236-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c236 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c237-j1 $$(./nexus c237 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c237-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c237 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c237-j0 $$(./nexus c237 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c237-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c237 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c238-j1 $$(./nexus c238 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c238-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c238 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c239-j1 $$(./nexus c239 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c239-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c239 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c240-j1 $$(./nexus c240 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c240-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c240 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c241-j1 $$(./nexus c241 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c241-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c241 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c242-j1 $$(./nexus c242 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c242-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c242 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c243-j1 $$(./nexus c243 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c243-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c243 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c244-j1 $$(./nexus c244 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c244-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c244 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c245-j1 $$(./nexus c245 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c245-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c245 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c246-j1 $$(./nexus c246 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c246-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c246 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c247-j1 $$(./nexus c247 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c247-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c247 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c247-j0 $$(./nexus c247 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c247-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c247 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c248-j1 $$(./nexus c248 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c248-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c248 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c249-j1 $$(./nexus c249 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c249-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c249 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c249-j0 $$(./nexus c249 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c249-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c249 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c250-j1 $$(./nexus c250 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c250-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c250 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c251-j1 $$(./nexus c251 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c251-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c251 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c252-j1 $$(./nexus c252 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c252-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c252 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c253-j1 $$(./nexus c253 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c253-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c253 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c254-j1 $$(./nexus c254 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c254-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c254 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c255-j1 $$(./nexus c255 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c255-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c255 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c255-j0 $$(./nexus c255 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c255-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c255 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c256-j1 $$(./nexus c256 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c256-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c256 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c257-j1 $$(./nexus c257 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c257-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c257 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c258-j1 $$(./nexus c258 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c258-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c258 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c259-j1 $$(./nexus c259 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c259-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c259 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c260-j1 $$(./nexus c260 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c260-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c260 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c261-j1 $$(./nexus c261 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c261-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c261 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c262-j1 $$(./nexus c262 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c262-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c262 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c263-j1 $$(./nexus c263 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c263-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c263 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c264-j1 $$(./nexus c264 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c264-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c264 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c265-j1 $$(./nexus c265 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c265-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c265 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c265-j0 $$(./nexus c265 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c265-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c265 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c266-j1 $$(./nexus c266 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c266-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c266 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c267-j1 $$(./nexus c267 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c267-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c267 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c267-j0 $$(./nexus c267 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c267-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c267 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c268-j1 $$(./nexus c268 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c268-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c268 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c269-j1 $$(./nexus c269 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c269-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c269 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c270-j1 $$(./nexus c270 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c270-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c270 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c271-j1 $$(./nexus c271 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c271-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c271 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c272-j1 $$(./nexus c272 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c272-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c272 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c273-j1 $$(./nexus c273 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c273-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c273 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c273-j0 $$(./nexus c273 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c273-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c273 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c274-j1 $$(./nexus c274 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c274-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c274 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c275-j1 $$(./nexus c275 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c275-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c275 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c276-j1 $$(./nexus c276 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c276-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c276 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c277-j1 $$(./nexus c277 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c277-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c277 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c278-j1 $$(./nexus c278 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c278-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c278 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c279-j1 $$(./nexus c279 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c279-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c279 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c280-j1 $$(./nexus c280 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c280-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c280 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c281-j1 $$(./nexus c281 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c281-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c281 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c282-j1 $$(./nexus c282 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c282-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c282 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c283-j1 $$(./nexus c283 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c283-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c283 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c283-j0 $$(./nexus c283 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c283-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c283 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c284-j1 $$(./nexus c284 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c284-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c284 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c285-j1 $$(./nexus c285 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c285-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c285 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c285-j0 $$(./nexus c285 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c285-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c285 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c286-j1 $$(./nexus c286 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c286-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c286 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c287-j1 $$(./nexus c287 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c287-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c287 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c288-j1 $$(./nexus c288 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c288-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c288 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c289-j1 $$(./nexus c289 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c289-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c289 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c290-j1 $$(./nexus c290 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c290-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c290 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c291-j1 $$(./nexus c291 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c291-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c291 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c291-j0 $$(./nexus c291 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c291-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c291 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c292-j1 $$(./nexus c292 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c292-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c292 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c292-j0 $$(./nexus c292 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c292-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c292 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c293-j1 $$(./nexus c293 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c293-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c293 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c293-j0 $$(./nexus c293 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c293-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c293 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c294-j1 $$(./nexus c294 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c294-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c294 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c294-j0 $$(./nexus c294 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c294-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c294 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c295-j1 $$(./nexus c295 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c295-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c295 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c295-j0 $$(./nexus c295 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c295-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c295 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c296-j1 $$(./nexus c296 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c296-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c296 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c296-j0 $$(./nexus c296 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c296-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c296 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c297-j1 $$(./nexus c297 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c297-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c297 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c297-j0 $$(./nexus c297 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c297-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c297 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c298-j1 $$(./nexus c298 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c298-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c298 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c298-j0 $$(./nexus c298 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c298-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c298 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c299-j1 $$(./nexus c299 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c299-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c299 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c299-j0 $$(./nexus c299 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c299-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c299 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c300-j1 $$(./nexus c300 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c300-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c300 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c300-j0 $$(./nexus c300 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c300-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c300 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c301-j1 $$(./nexus c301 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c301-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c301 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c301-j0 $$(./nexus c301 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c301-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c301 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c302-j1 $$(./nexus c302 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c302-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c302 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c302-j0 $$(./nexus c302 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c302-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c302 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c303-j1 $$(./nexus c303 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c303-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c303 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c303-j0 $$(./nexus c303 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c303-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c303 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c304-j1 $$(./nexus c304 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c304-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c304 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c304-j0 $$(./nexus c304 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c304-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c304 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c305-j1 $$(./nexus c305 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c305-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c305 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c305-j0 $$(./nexus c305 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c305-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c305 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c306-j1 $$(./nexus c306 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c306-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c306 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c306-j0 $$(./nexus c306 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c306-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c306 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c307-j1 $$(./nexus c307 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c307-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c307 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c307-j0 $$(./nexus c307 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c307-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c307 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c308-j1 $$(./nexus c308 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c308-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c308 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c309-j1 $$(./nexus c309 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c309-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c309 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c309-j0 $$(./nexus c309 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c309-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c309 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c310-j1 $$(./nexus c310 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c310-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c310 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c311-j1 $$(./nexus c311 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c311-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c311 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c312-j1 $$(./nexus c312 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c312-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c312 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c313-j1 $$(./nexus c313 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c313-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c313 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c314-j1 $$(./nexus c314 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c314-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c314 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c315-j1 $$(./nexus c315 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c315-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c315 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c315-j0 $$(./nexus c315 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c315-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c315 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c316-j1 $$(./nexus c316 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c316-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c316 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c317-j1 $$(./nexus c317 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c317-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c317 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c318-j1 $$(./nexus c318 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c318-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c318 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c319-j1 $$(./nexus c319 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c319-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c319 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c320-j1 $$(./nexus c320 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c320-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c320 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c321-j1 $$(./nexus c321 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c321-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c321 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c322-j1 $$(./nexus c322 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c322-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c322 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c323-j1 $$(./nexus c323 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c323-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c323 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c324-j1 $$(./nexus c324 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c324-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c324 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c325-j1 $$(./nexus c325 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c325-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c325 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c325-j0 $$(./nexus c325 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c325-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c325 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c326-j1 $$(./nexus c326 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c326-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c326 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c327-j1 $$(./nexus c327 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c327-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c327 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c327-j0 $$(./nexus c327 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c327-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c327 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c328-j1 $$(./nexus c328 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c328-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c328 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c329-j1 $$(./nexus c329 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c329-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c329 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c330-j1 $$(./nexus c330 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c330-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c330 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c331-j1 $$(./nexus c331 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c331-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c331 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c332-j1 $$(./nexus c332 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c332-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c332 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c333-j1 $$(./nexus c333 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c333-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c333 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c333-j0 $$(./nexus c333 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c333-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c333 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c334-j1 $$(./nexus c334 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c334-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c334 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c335-j1 $$(./nexus c335 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c335-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c335 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c336-j1 $$(./nexus c336 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c336-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c336 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c337-j1 $$(./nexus c337 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c337-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c337 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c338-j1 $$(./nexus c338 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c338-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c338 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c339-j1 $$(./nexus c339 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c339-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c339 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c340-j1 $$(./nexus c340 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c340-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c340 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c341-j1 $$(./nexus c341 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c341-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c341 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c342-j1 $$(./nexus c342 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c342-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c342 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c343-j1 $$(./nexus c343 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c343-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c343 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c343-j0 $$(./nexus c343 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c343-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c343 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c344-j1 $$(./nexus c344 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c344-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c344 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c345-j1 $$(./nexus c345 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c345-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c345 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c345-j0 $$(./nexus c345 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c345-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c345 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c346-j1 $$(./nexus c346 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c346-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c346 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c347-j1 $$(./nexus c347 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c347-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c347 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c348-j1 $$(./nexus c348 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c348-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c348 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c349-j1 $$(./nexus c349 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c349-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c349 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c350-j1 $$(./nexus c350 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c350-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c350 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c351-j1 $$(./nexus c351 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c351-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c351 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c351-j0 $$(./nexus c351 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c351-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c351 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c352-j1 $$(./nexus c352 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c352-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c352 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c353-j1 $$(./nexus c353 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c353-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c353 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c354-j1 $$(./nexus c354 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c354-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c354 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c355-j1 $$(./nexus c355 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c355-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c355 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c356-j1 $$(./nexus c356 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c356-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c356 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c357-j1 $$(./nexus c357 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c357-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c357 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c358-j1 $$(./nexus c358 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c358-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c358 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c359-j1 $$(./nexus c359 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c359-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c359 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c360-j1 $$(./nexus c360 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c360-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c360 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c361-j1 $$(./nexus c361 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c361-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c361 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c361-j0 $$(./nexus c361 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c361-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c361 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c362-j1 $$(./nexus c362 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c362-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c362 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c363-j1 $$(./nexus c363 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c363-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c363 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c363-j0 $$(./nexus c363 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c363-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c363 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c364-j1 $$(./nexus c364 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c364-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c364 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c365-j1 $$(./nexus c365 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c365-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c365 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c366-j1 $$(./nexus c366 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c366-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c366 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c367-j1 $$(./nexus c367 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c367-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c367 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c368-j1 $$(./nexus c368 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c368-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c368 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c369-j1 $$(./nexus c369 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c369-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c369 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c369-j0 $$(./nexus c369 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c369-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c369 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c370-j1 $$(./nexus c370 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c370-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c370 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c371-j1 $$(./nexus c371 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c371-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c371 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c372-j1 $$(./nexus c372 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c372-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c372 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c373-j1 $$(./nexus c373 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c373-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c373 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c374-j1 $$(./nexus c374 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c374-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c374 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c375-j1 $$(./nexus c375 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c375-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c375 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c376-j1 $$(./nexus c376 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c376-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c376 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c377-j1 $$(./nexus c377 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c377-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c377 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c378-j1 $$(./nexus c378 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c378-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c378 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c379-j1 $$(./nexus c379 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c379-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c379 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c379-j0 $$(./nexus c379 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c379-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c379 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c380-j1 $$(./nexus c380 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c380-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c380 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c381-j1 $$(./nexus c381 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c381-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c381 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c381-j0 $$(./nexus c381 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c381-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c381 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c382-j1 $$(./nexus c382 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c382-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c382 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c383-j1 $$(./nexus c383 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c383-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c383 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c384-j1 $$(./nexus c384 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c384-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c384 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c385-j1 $$(./nexus c385 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c385-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c385 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c386-j1 $$(./nexus c386 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c386-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c386 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c387-j1 $$(./nexus c387 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c387-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c387 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c387-j0 $$(./nexus c387 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c387-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c387 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c388-j1 $$(./nexus c388 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c388-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c388 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c388-j0 $$(./nexus c388 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c388-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c388 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c389-j1 $$(./nexus c389 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c389-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c389 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c389-j0 $$(./nexus c389 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c389-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c389 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c390-j1 $$(./nexus c390 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c390-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c390 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c390-j0 $$(./nexus c390 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c390-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c390 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c391-j1 $$(./nexus c391 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c391-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c391 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c391-j0 $$(./nexus c391 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c391-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c391 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c392-j1 $$(./nexus c392 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c392-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c392 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c392-j0 $$(./nexus c392 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c392-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c392 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c393-j1 $$(./nexus c393 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c393-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c393 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c393-j0 $$(./nexus c393 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c393-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c393 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c394-j1 $$(./nexus c394 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c394-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c394 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c394-j0 $$(./nexus c394 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c394-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c394 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c395-j1 $$(./nexus c395 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c395-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c395 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c395-j0 $$(./nexus c395 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c395-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c395 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c396-j1 $$(./nexus c396 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c396-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c396 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c396-j0 $$(./nexus c396 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c396-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c396 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c397-j1 $$(./nexus c397 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c397-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c397 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c397-j0 $$(./nexus c397 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c397-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c397 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c398-j1 $$(./nexus c398 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c398-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c398 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c398-j0 $$(./nexus c398 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c398-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c398 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c399-j1 $$(./nexus c399 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c399-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c399 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c399-j0 $$(./nexus c399 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c399-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c399 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c400-j1 $$(./nexus c400 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c400-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c400 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c400-j0 $$(./nexus c400 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c400-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c400 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c401-j1 $$(./nexus c401 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c401-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c401 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c401-j0 $$(./nexus c401 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c401-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c401 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c402-j1 $$(./nexus c402 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c402-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c402 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c402-j0 $$(./nexus c402 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c402-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c402 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c403-j1 $$(./nexus c403 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c403-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c403 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c403-j0 $$(./nexus c403 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c403-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c403 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c404-j1 $$(./nexus c404 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c404-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c404 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c405-j1 $$(./nexus c405 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c405-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c405 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c405-j0 $$(./nexus c405 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c405-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c405 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c406-j1 $$(./nexus c406 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c406-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c406 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c407-j1 $$(./nexus c407 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c407-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c407 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c408-j1 $$(./nexus c408 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c408-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c408 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c409-j1 $$(./nexus c409 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c409-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c409 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c410-j1 $$(./nexus c410 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c410-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c410 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c411-j1 $$(./nexus c411 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c411-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c411 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c411-j0 $$(./nexus c411 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c411-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c411 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c412-j1 $$(./nexus c412 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c412-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c412 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c413-j1 $$(./nexus c413 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c413-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c413 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c414-j1 $$(./nexus c414 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c414-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c414 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c415-j1 $$(./nexus c415 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c415-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c415 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c416-j1 $$(./nexus c416 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c416-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c416 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c417-j1 $$(./nexus c417 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c417-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c417 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c418-j1 $$(./nexus c418 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c418-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c418 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c419-j1 $$(./nexus c419 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c419-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c419 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c420-j1 $$(./nexus c420 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c420-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c420 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c421-j1 $$(./nexus c421 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c421-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c421 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c421-j0 $$(./nexus c421 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c421-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c421 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c422-j1 $$(./nexus c422 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c422-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c422 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c423-j1 $$(./nexus c423 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c423-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c423 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c423-j0 $$(./nexus c423 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c423-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c423 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c424-j1 $$(./nexus c424 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c424-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c424 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c425-j1 $$(./nexus c425 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c425-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c425 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c426-j1 $$(./nexus c426 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c426-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c426 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c427-j1 $$(./nexus c427 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c427-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c427 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c428-j1 $$(./nexus c428 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c428-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c428 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c429-j1 $$(./nexus c429 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c429-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c429 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c429-j0 $$(./nexus c429 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c429-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c429 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c430-j1 $$(./nexus c430 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c430-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c430 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c431-j1 $$(./nexus c431 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c431-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c431 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c432-j1 $$(./nexus c432 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c432-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c432 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c433-j1 $$(./nexus c433 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c433-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c433 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c434-j1 $$(./nexus c434 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c434-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c434 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c435-j1 $$(./nexus c435 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c435-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c435 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c436-j1 $$(./nexus c436 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c436-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c436 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c437-j1 $$(./nexus c437 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c437-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c437 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c438-j1 $$(./nexus c438 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c438-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c438 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c439-j1 $$(./nexus c439 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c439-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c439 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c439-j0 $$(./nexus c439 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c439-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c439 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c440-j1 $$(./nexus c440 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c440-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c440 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c441-j1 $$(./nexus c441 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c441-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c441 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c441-j0 $$(./nexus c441 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c441-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c441 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c442-j1 $$(./nexus c442 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c442-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c442 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c443-j1 $$(./nexus c443 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c443-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c443 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c444-j1 $$(./nexus c444 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c444-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c444 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c445-j1 $$(./nexus c445 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c445-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c445 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c446-j1 $$(./nexus c446 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c446-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c446 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c447-j1 $$(./nexus c447 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c447-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c447 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c447-j0 $$(./nexus c447 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c447-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c447 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c448-j1 $$(./nexus c448 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c448-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c448 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c449-j1 $$(./nexus c449 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c449-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c449 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c450-j1 $$(./nexus c450 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c450-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c450 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c451-j1 $$(./nexus c451 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c451-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c451 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c452-j1 $$(./nexus c452 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c452-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c452 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c453-j1 $$(./nexus c453 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c453-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c453 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c454-j1 $$(./nexus c454 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c454-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c454 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c455-j1 $$(./nexus c455 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c455-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c455 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c456-j1 $$(./nexus c456 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c456-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c456 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c457-j1 $$(./nexus c457 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c457-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c457 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c457-j0 $$(./nexus c457 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c457-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c457 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c458-j1 $$(./nexus c458 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c458-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c458 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c459-j1 $$(./nexus c459 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c459-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c459 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c459-j0 $$(./nexus c459 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c459-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c459 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c460-j1 $$(./nexus c460 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c460-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c460 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c461-j1 $$(./nexus c461 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c461-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c461 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c462-j1 $$(./nexus c462 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c462-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c462 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c463-j1 $$(./nexus c463 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c463-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c463 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c464-j1 $$(./nexus c464 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c464-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c464 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c465-j1 $$(./nexus c465 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c465-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c465 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c465-j0 $$(./nexus c465 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c465-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c465 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c466-j1 $$(./nexus c466 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c466-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c466 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c467-j1 $$(./nexus c467 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c467-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c467 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c468-j1 $$(./nexus c468 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c468-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c468 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c469-j1 $$(./nexus c469 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c469-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c469 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c470-j1 $$(./nexus c470 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c470-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c470 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c471-j1 $$(./nexus c471 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c471-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c471 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c472-j1 $$(./nexus c472 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c472-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c472 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c473-j1 $$(./nexus c473 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c473-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c473 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c474-j1 $$(./nexus c474 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c474-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c474 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c475-j1 $$(./nexus c475 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c475-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c475 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c475-j0 $$(./nexus c475 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c475-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c475 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c476-j1 $$(./nexus c476 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c476-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c476 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c477-j1 $$(./nexus c477 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c477-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c477 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c477-j0 $$(./nexus c477 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c477-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c477 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c478-j1 $$(./nexus c478 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c478-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c478 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c479-j1 $$(./nexus c479 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c479-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c479 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c480-j1 $$(./nexus c480 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c480-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c480 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c481-j1 $$(./nexus c481 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c481-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c481 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c482-j1 $$(./nexus c482 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c482-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c482 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c483-j1 $$(./nexus c483 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c483-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c483 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c483-j0 $$(./nexus c483 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c483-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c483 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c484-j1 $$(./nexus c484 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c484-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c484 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c484-j0 $$(./nexus c484 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c484-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c484 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c485-j1 $$(./nexus c485 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c485-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c485 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c485-j0 $$(./nexus c485 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c485-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c485 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c486-j1 $$(./nexus c486 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c486-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c486 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c486-j0 $$(./nexus c486 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c486-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c486 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c487-j1 $$(./nexus c487 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c487-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c487 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c487-j0 $$(./nexus c487 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c487-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c487 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c488-j1 $$(./nexus c488 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c488-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c488 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c488-j0 $$(./nexus c488 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c488-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c488 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c489-j1 $$(./nexus c489 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c489-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c489 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c489-j0 $$(./nexus c489 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c489-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c489 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c490-j1 $$(./nexus c490 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c490-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c490 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c490-j0 $$(./nexus c490 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c490-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c490 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c491-j1 $$(./nexus c491 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c491-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c491 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c491-j0 $$(./nexus c491 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c491-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c491 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c492-j1 $$(./nexus c492 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c492-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c492 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c492-j0 $$(./nexus c492 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c492-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c492 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c493-j1 $$(./nexus c493 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c493-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c493 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c493-j0 $$(./nexus c493 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c493-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c493 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c494-j1 $$(./nexus c494 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c494-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c494 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c494-j0 $$(./nexus c494 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c494-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c494 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c495-j1 $$(./nexus c495 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c495-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c495 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c495-j0 $$(./nexus c495 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c495-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c495 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c496-j1 $$(./nexus c496 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c496-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c496 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c496-j0 $$(./nexus c496 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c496-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c496 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c497-j1 $$(./nexus c497 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c497-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c497 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c497-j0 $$(./nexus c497 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c497-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c497 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c498-j1 $$(./nexus c498 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c498-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c498 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c498-j0 $$(./nexus c498 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c498-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c498 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c499-j1 $$(./nexus c499 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c499-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c499 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c499-j0 $$(./nexus c499 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c499-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c499 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c500-j1 $$(./nexus c500 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c500-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c500 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c501-j1 $$(./nexus c501 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c501-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c501 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c501-j0 $$(./nexus c501 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c501-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c501 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c502-j1 $$(./nexus c502 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c502-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c502 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c503-j1 $$(./nexus c503 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c503-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c503 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c504-j1 $$(./nexus c504 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c504-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c504 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c505-j1 $$(./nexus c505 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c505-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c505 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c506-j1 $$(./nexus c506 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c506-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c506 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c507-j1 $$(./nexus c507 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c507-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c507 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c507-j0 $$(./nexus c507 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c507-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c507 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c508-j1 $$(./nexus c508 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c508-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c508 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c509-j1 $$(./nexus c509 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c509-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c509 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c510-j1 $$(./nexus c510 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c510-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c510 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c511-j1 $$(./nexus c511 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c511-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c511 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c512-j1 $$(./nexus c512 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c512-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c512 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c513-j1 $$(./nexus c513 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c513-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c513 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c514-j1 $$(./nexus c514 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c514-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c514 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c515-j1 $$(./nexus c515 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c515-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c515 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c516-j1 $$(./nexus c516 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c516-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c516 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c517-j1 $$(./nexus c517 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c517-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c517 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c517-j0 $$(./nexus c517 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c517-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c517 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c518-j1 $$(./nexus c518 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c518-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c518 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c519-j1 $$(./nexus c519 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c519-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c519 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c519-j0 $$(./nexus c519 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c519-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c519 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c520-j1 $$(./nexus c520 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c520-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c520 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c521-j1 $$(./nexus c521 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c521-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c521 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c522-j1 $$(./nexus c522 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c522-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c522 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c523-j1 $$(./nexus c523 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c523-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c523 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c524-j1 $$(./nexus c524 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c524-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c524 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c525-j1 $$(./nexus c525 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c525-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c525 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c525-j0 $$(./nexus c525 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c525-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c525 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c526-j1 $$(./nexus c526 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c526-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c526 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c527-j1 $$(./nexus c527 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c527-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c527 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c528-j1 $$(./nexus c528 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c528-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c528 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c529-j1 $$(./nexus c529 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c529-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c529 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c530-j1 $$(./nexus c530 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c530-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c530 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c531-j1 $$(./nexus c531 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c531-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c531 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c532-j1 $$(./nexus c532 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c532-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c532 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c533-j1 $$(./nexus c533 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c533-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c533 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c534-j1 $$(./nexus c534 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c534-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c534 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c535-j1 $$(./nexus c535 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c535-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c535 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c535-j0 $$(./nexus c535 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c535-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c535 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c536-j1 $$(./nexus c536 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c536-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c536 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c537-j1 $$(./nexus c537 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c537-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c537 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c537-j0 $$(./nexus c537 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c537-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c537 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c538-j1 $$(./nexus c538 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c538-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c538 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c539-j1 $$(./nexus c539 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c539-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c539 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c540-j1 $$(./nexus c540 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c540-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c540 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c541-j1 $$(./nexus c541 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c541-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c541 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c542-j1 $$(./nexus c542 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c542-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c542 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c543-j1 $$(./nexus c543 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c543-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c543 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c543-j0 $$(./nexus c543 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c543-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c543 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c544-j1 $$(./nexus c544 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c544-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c544 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c545-j1 $$(./nexus c545 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c545-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c545 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c546-j1 $$(./nexus c546 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c546-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c546 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c547-j1 $$(./nexus c547 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c547-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c547 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c548-j1 $$(./nexus c548 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c548-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c548 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c549-j1 $$(./nexus c549 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c549-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c549 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c550-j1 $$(./nexus c550 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c550-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c550 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c551-j1 $$(./nexus c551 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c551-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c551 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c552-j1 $$(./nexus c552 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c552-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c552 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c553-j1 $$(./nexus c553 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c553-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c553 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c553-j0 $$(./nexus c553 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c553-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c553 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c554-j1 $$(./nexus c554 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c554-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c554 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c555-j1 $$(./nexus c555 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c555-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c555 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c555-j0 $$(./nexus c555 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c555-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c555 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c556-j1 $$(./nexus c556 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c556-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c556 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c557-j1 $$(./nexus c557 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c557-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c557 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c558-j1 $$(./nexus c558 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c558-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c558 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c559-j1 $$(./nexus c559 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c559-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c559 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c560-j1 $$(./nexus c560 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c560-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c560 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c561-j1 $$(./nexus c561 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c561-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c561 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c561-j0 $$(./nexus c561 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c561-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c561 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c562-j1 $$(./nexus c562 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c562-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c562 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c563-j1 $$(./nexus c563 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c563-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c563 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c564-j1 $$(./nexus c564 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c564-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c564 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c565-j1 $$(./nexus c565 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c565-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c565 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c566-j1 $$(./nexus c566 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c566-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c566 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c567-j1 $$(./nexus c567 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c567-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c567 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c568-j1 $$(./nexus c568 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c568-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c568 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c569-j1 $$(./nexus c569 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c569-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c569 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c570-j1 $$(./nexus c570 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c570-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c570 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c571-j1 $$(./nexus c571 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c571-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c571 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c571-j0 $$(./nexus c571 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c571-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c571 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c572-j1 $$(./nexus c572 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c572-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c572 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c573-j1 $$(./nexus c573 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c573-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c573 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c573-j0 $$(./nexus c573 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c573-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c573 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "c574-j1 $$(./nexus c574 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c574-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c574 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c575-j1 $$(./nexus c575 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c575-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c575 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c576-j1 $$(./nexus c576 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c576-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c576 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c577-j1 $$(./nexus c577 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c577-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c577 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c578-j1 $$(./nexus c578 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c578-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c578 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c579-j1 $$(./nexus c579 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "c579-j1 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c579 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "c579-j0 $$(./nexus c579 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "c579-j0 $$(ASAN_OPTIONS=detect_leaks=0 ./asan c579 7 0 2>> obs.err || echo failed)" >> asan.txt
	python3 check.py run.txt oracle.txt asan.txt > obs.txt

oracle:
	rm -f oracle.txt oracle-build.err oracle.err
	MORLOC_NO_MOVES=1 morloc make -o oracle main.loc 2> oracle-build.err
	echo "c000-j1 $$(./oracle c000 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c000-j0 $$(./oracle c000 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c001-j1 $$(./oracle c001 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c001-j0 $$(./oracle c001 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c002-j1 $$(./oracle c002 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c002-j0 $$(./oracle c002 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c003-j1 $$(./oracle c003 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c003-j0 $$(./oracle c003 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c004-j1 $$(./oracle c004 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c004-j0 $$(./oracle c004 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c005-j1 $$(./oracle c005 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c005-j0 $$(./oracle c005 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c006-j1 $$(./oracle c006 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c006-j0 $$(./oracle c006 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c007-j1 $$(./oracle c007 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c007-j0 $$(./oracle c007 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c008-j1 $$(./oracle c008 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c008-j0 $$(./oracle c008 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c009-j1 $$(./oracle c009 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c009-j0 $$(./oracle c009 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c010-j1 $$(./oracle c010 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c010-j0 $$(./oracle c010 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c011-j1 $$(./oracle c011 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c011-j0 $$(./oracle c011 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c012-j1 $$(./oracle c012 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c012-j0 $$(./oracle c012 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c013-j1 $$(./oracle c013 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c013-j0 $$(./oracle c013 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c014-j1 $$(./oracle c014 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c014-j0 $$(./oracle c014 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c015-j1 $$(./oracle c015 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c016-j1 $$(./oracle c016 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c016-j0 $$(./oracle c016 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c017-j1 $$(./oracle c017 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c018-j1 $$(./oracle c018 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c019-j1 $$(./oracle c019 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c020-j1 $$(./oracle c020 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c021-j1 $$(./oracle c021 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c022-j1 $$(./oracle c022 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c023-j1 $$(./oracle c023 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c024-j1 $$(./oracle c024 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c025-j1 $$(./oracle c025 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c026-j1 $$(./oracle c026 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c027-j1 $$(./oracle c027 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c028-j1 $$(./oracle c028 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c029-j1 $$(./oracle c029 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c030-j1 $$(./oracle c030 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c030-j0 $$(./oracle c030 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c031-j1 $$(./oracle c031 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c032-j1 $$(./oracle c032 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c032-j0 $$(./oracle c032 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c033-j1 $$(./oracle c033 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c034-j1 $$(./oracle c034 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c035-j1 $$(./oracle c035 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c036-j1 $$(./oracle c036 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c037-j1 $$(./oracle c037 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c038-j1 $$(./oracle c038 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c039-j1 $$(./oracle c039 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c040-j1 $$(./oracle c040 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c041-j1 $$(./oracle c041 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c042-j1 $$(./oracle c042 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c043-j1 $$(./oracle c043 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c044-j1 $$(./oracle c044 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c045-j1 $$(./oracle c045 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c046-j1 $$(./oracle c046 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c046-j0 $$(./oracle c046 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c047-j1 $$(./oracle c047 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c048-j1 $$(./oracle c048 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c048-j0 $$(./oracle c048 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c049-j1 $$(./oracle c049 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c050-j1 $$(./oracle c050 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c051-j1 $$(./oracle c051 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c052-j1 $$(./oracle c052 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c053-j1 $$(./oracle c053 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c054-j1 $$(./oracle c054 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c055-j1 $$(./oracle c055 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c056-j1 $$(./oracle c056 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c057-j1 $$(./oracle c057 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c058-j1 $$(./oracle c058 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c059-j1 $$(./oracle c059 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c060-j1 $$(./oracle c060 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c061-j1 $$(./oracle c061 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c062-j1 $$(./oracle c062 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c062-j0 $$(./oracle c062 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c063-j1 $$(./oracle c063 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c064-j1 $$(./oracle c064 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c064-j0 $$(./oracle c064 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c065-j1 $$(./oracle c065 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c066-j1 $$(./oracle c066 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c067-j1 $$(./oracle c067 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c068-j1 $$(./oracle c068 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c069-j1 $$(./oracle c069 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c070-j1 $$(./oracle c070 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c070-j0 $$(./oracle c070 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c071-j1 $$(./oracle c071 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c071-j0 $$(./oracle c071 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c072-j1 $$(./oracle c072 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c072-j0 $$(./oracle c072 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c073-j1 $$(./oracle c073 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c073-j0 $$(./oracle c073 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c074-j1 $$(./oracle c074 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c074-j0 $$(./oracle c074 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c075-j1 $$(./oracle c075 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c075-j0 $$(./oracle c075 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c076-j1 $$(./oracle c076 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c076-j0 $$(./oracle c076 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c077-j1 $$(./oracle c077 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c077-j0 $$(./oracle c077 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c078-j1 $$(./oracle c078 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c078-j0 $$(./oracle c078 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c079-j1 $$(./oracle c079 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c079-j0 $$(./oracle c079 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c080-j1 $$(./oracle c080 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c080-j0 $$(./oracle c080 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c081-j1 $$(./oracle c081 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c081-j0 $$(./oracle c081 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c082-j1 $$(./oracle c082 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c082-j0 $$(./oracle c082 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c083-j1 $$(./oracle c083 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c083-j0 $$(./oracle c083 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c084-j1 $$(./oracle c084 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c084-j0 $$(./oracle c084 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c085-j1 $$(./oracle c085 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c085-j0 $$(./oracle c085 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c086-j1 $$(./oracle c086 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c086-j0 $$(./oracle c086 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c087-j1 $$(./oracle c087 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c087-j0 $$(./oracle c087 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c088-j1 $$(./oracle c088 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c088-j0 $$(./oracle c088 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c089-j1 $$(./oracle c089 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c089-j0 $$(./oracle c089 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c090-j1 $$(./oracle c090 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c090-j0 $$(./oracle c090 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c091-j1 $$(./oracle c091 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c091-j0 $$(./oracle c091 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c092-j1 $$(./oracle c092 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c092-j0 $$(./oracle c092 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c093-j1 $$(./oracle c093 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c093-j0 $$(./oracle c093 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c094-j1 $$(./oracle c094 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c094-j0 $$(./oracle c094 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c095-j1 $$(./oracle c095 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c095-j0 $$(./oracle c095 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c096-j1 $$(./oracle c096 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c096-j0 $$(./oracle c096 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c097-j1 $$(./oracle c097 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c097-j0 $$(./oracle c097 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c098-j1 $$(./oracle c098 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c098-j0 $$(./oracle c098 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c099-j1 $$(./oracle c099 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c099-j0 $$(./oracle c099 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c100-j1 $$(./oracle c100 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c100-j0 $$(./oracle c100 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c101-j1 $$(./oracle c101 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c101-j0 $$(./oracle c101 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c102-j1 $$(./oracle c102 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c102-j0 $$(./oracle c102 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c103-j1 $$(./oracle c103 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c103-j0 $$(./oracle c103 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c104-j1 $$(./oracle c104 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c104-j0 $$(./oracle c104 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c105-j1 $$(./oracle c105 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c105-j0 $$(./oracle c105 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c106-j1 $$(./oracle c106 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c106-j0 $$(./oracle c106 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c107-j1 $$(./oracle c107 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c107-j0 $$(./oracle c107 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c108-j1 $$(./oracle c108 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c108-j0 $$(./oracle c108 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c109-j1 $$(./oracle c109 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c109-j0 $$(./oracle c109 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c110-j1 $$(./oracle c110 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c110-j0 $$(./oracle c110 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c111-j1 $$(./oracle c111 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c111-j0 $$(./oracle c111 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c112-j1 $$(./oracle c112 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c112-j0 $$(./oracle c112 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c113-j1 $$(./oracle c113 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c113-j0 $$(./oracle c113 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c114-j1 $$(./oracle c114 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c114-j0 $$(./oracle c114 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c115-j1 $$(./oracle c115 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c115-j0 $$(./oracle c115 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c116-j1 $$(./oracle c116 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c117-j1 $$(./oracle c117 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c117-j0 $$(./oracle c117 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c118-j1 $$(./oracle c118 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c119-j1 $$(./oracle c119 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c120-j1 $$(./oracle c120 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c121-j1 $$(./oracle c121 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c122-j1 $$(./oracle c122 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c123-j1 $$(./oracle c123 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c123-j0 $$(./oracle c123 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c124-j1 $$(./oracle c124 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c125-j1 $$(./oracle c125 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c126-j1 $$(./oracle c126 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c127-j1 $$(./oracle c127 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c128-j1 $$(./oracle c128 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c129-j1 $$(./oracle c129 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c130-j1 $$(./oracle c130 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c131-j1 $$(./oracle c131 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c132-j1 $$(./oracle c132 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c133-j1 $$(./oracle c133 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c133-j0 $$(./oracle c133 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c134-j1 $$(./oracle c134 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c135-j1 $$(./oracle c135 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c135-j0 $$(./oracle c135 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c136-j1 $$(./oracle c136 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c137-j1 $$(./oracle c137 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c138-j1 $$(./oracle c138 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c139-j1 $$(./oracle c139 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c140-j1 $$(./oracle c140 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c141-j1 $$(./oracle c141 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c141-j0 $$(./oracle c141 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c142-j1 $$(./oracle c142 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c143-j1 $$(./oracle c143 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c144-j1 $$(./oracle c144 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c145-j1 $$(./oracle c145 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c146-j1 $$(./oracle c146 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c147-j1 $$(./oracle c147 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c148-j1 $$(./oracle c148 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c149-j1 $$(./oracle c149 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c150-j1 $$(./oracle c150 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c151-j1 $$(./oracle c151 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c151-j0 $$(./oracle c151 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c152-j1 $$(./oracle c152 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c153-j1 $$(./oracle c153 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c153-j0 $$(./oracle c153 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c154-j1 $$(./oracle c154 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c155-j1 $$(./oracle c155 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c156-j1 $$(./oracle c156 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c157-j1 $$(./oracle c157 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c158-j1 $$(./oracle c158 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c159-j1 $$(./oracle c159 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c159-j0 $$(./oracle c159 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c160-j1 $$(./oracle c160 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c161-j1 $$(./oracle c161 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c162-j1 $$(./oracle c162 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c163-j1 $$(./oracle c163 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c164-j1 $$(./oracle c164 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c165-j1 $$(./oracle c165 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c166-j1 $$(./oracle c166 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c167-j1 $$(./oracle c167 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c168-j1 $$(./oracle c168 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c169-j1 $$(./oracle c169 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c169-j0 $$(./oracle c169 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c170-j1 $$(./oracle c170 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c171-j1 $$(./oracle c171 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c171-j0 $$(./oracle c171 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c172-j1 $$(./oracle c172 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c173-j1 $$(./oracle c173 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c174-j1 $$(./oracle c174 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c175-j1 $$(./oracle c175 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c176-j1 $$(./oracle c176 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c177-j1 $$(./oracle c177 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c177-j0 $$(./oracle c177 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c178-j1 $$(./oracle c178 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c179-j1 $$(./oracle c179 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c180-j1 $$(./oracle c180 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c181-j1 $$(./oracle c181 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c182-j1 $$(./oracle c182 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c183-j1 $$(./oracle c183 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c184-j1 $$(./oracle c184 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c185-j1 $$(./oracle c185 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c186-j1 $$(./oracle c186 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c187-j1 $$(./oracle c187 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c187-j0 $$(./oracle c187 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c188-j1 $$(./oracle c188 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c189-j1 $$(./oracle c189 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c189-j0 $$(./oracle c189 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c190-j1 $$(./oracle c190 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c191-j1 $$(./oracle c191 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c192-j1 $$(./oracle c192 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c193-j1 $$(./oracle c193 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c194-j1 $$(./oracle c194 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c195-j1 $$(./oracle c195 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c195-j0 $$(./oracle c195 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c196-j1 $$(./oracle c196 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c196-j0 $$(./oracle c196 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c197-j1 $$(./oracle c197 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c197-j0 $$(./oracle c197 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c198-j1 $$(./oracle c198 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c198-j0 $$(./oracle c198 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c199-j1 $$(./oracle c199 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c199-j0 $$(./oracle c199 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c200-j1 $$(./oracle c200 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c200-j0 $$(./oracle c200 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c201-j1 $$(./oracle c201 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c201-j0 $$(./oracle c201 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c202-j1 $$(./oracle c202 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c202-j0 $$(./oracle c202 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c203-j1 $$(./oracle c203 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c203-j0 $$(./oracle c203 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c204-j1 $$(./oracle c204 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c204-j0 $$(./oracle c204 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c205-j1 $$(./oracle c205 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c205-j0 $$(./oracle c205 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c206-j1 $$(./oracle c206 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c206-j0 $$(./oracle c206 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c207-j1 $$(./oracle c207 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c207-j0 $$(./oracle c207 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c208-j1 $$(./oracle c208 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c208-j0 $$(./oracle c208 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c209-j1 $$(./oracle c209 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c209-j0 $$(./oracle c209 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c210-j1 $$(./oracle c210 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c210-j0 $$(./oracle c210 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c211-j1 $$(./oracle c211 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c211-j0 $$(./oracle c211 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c212-j1 $$(./oracle c212 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c213-j1 $$(./oracle c213 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c213-j0 $$(./oracle c213 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c214-j1 $$(./oracle c214 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c215-j1 $$(./oracle c215 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c216-j1 $$(./oracle c216 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c217-j1 $$(./oracle c217 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c218-j1 $$(./oracle c218 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c219-j1 $$(./oracle c219 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c219-j0 $$(./oracle c219 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c220-j1 $$(./oracle c220 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c221-j1 $$(./oracle c221 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c222-j1 $$(./oracle c222 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c223-j1 $$(./oracle c223 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c224-j1 $$(./oracle c224 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c225-j1 $$(./oracle c225 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c226-j1 $$(./oracle c226 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c227-j1 $$(./oracle c227 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c228-j1 $$(./oracle c228 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c229-j1 $$(./oracle c229 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c229-j0 $$(./oracle c229 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c230-j1 $$(./oracle c230 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c231-j1 $$(./oracle c231 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c231-j0 $$(./oracle c231 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c232-j1 $$(./oracle c232 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c233-j1 $$(./oracle c233 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c234-j1 $$(./oracle c234 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c235-j1 $$(./oracle c235 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c236-j1 $$(./oracle c236 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c237-j1 $$(./oracle c237 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c237-j0 $$(./oracle c237 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c238-j1 $$(./oracle c238 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c239-j1 $$(./oracle c239 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c240-j1 $$(./oracle c240 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c241-j1 $$(./oracle c241 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c242-j1 $$(./oracle c242 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c243-j1 $$(./oracle c243 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c244-j1 $$(./oracle c244 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c245-j1 $$(./oracle c245 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c246-j1 $$(./oracle c246 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c247-j1 $$(./oracle c247 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c247-j0 $$(./oracle c247 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c248-j1 $$(./oracle c248 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c249-j1 $$(./oracle c249 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c249-j0 $$(./oracle c249 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c250-j1 $$(./oracle c250 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c251-j1 $$(./oracle c251 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c252-j1 $$(./oracle c252 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c253-j1 $$(./oracle c253 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c254-j1 $$(./oracle c254 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c255-j1 $$(./oracle c255 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c255-j0 $$(./oracle c255 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c256-j1 $$(./oracle c256 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c257-j1 $$(./oracle c257 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c258-j1 $$(./oracle c258 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c259-j1 $$(./oracle c259 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c260-j1 $$(./oracle c260 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c261-j1 $$(./oracle c261 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c262-j1 $$(./oracle c262 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c263-j1 $$(./oracle c263 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c264-j1 $$(./oracle c264 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c265-j1 $$(./oracle c265 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c265-j0 $$(./oracle c265 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c266-j1 $$(./oracle c266 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c267-j1 $$(./oracle c267 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c267-j0 $$(./oracle c267 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c268-j1 $$(./oracle c268 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c269-j1 $$(./oracle c269 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c270-j1 $$(./oracle c270 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c271-j1 $$(./oracle c271 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c272-j1 $$(./oracle c272 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c273-j1 $$(./oracle c273 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c273-j0 $$(./oracle c273 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c274-j1 $$(./oracle c274 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c275-j1 $$(./oracle c275 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c276-j1 $$(./oracle c276 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c277-j1 $$(./oracle c277 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c278-j1 $$(./oracle c278 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c279-j1 $$(./oracle c279 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c280-j1 $$(./oracle c280 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c281-j1 $$(./oracle c281 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c282-j1 $$(./oracle c282 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c283-j1 $$(./oracle c283 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c283-j0 $$(./oracle c283 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c284-j1 $$(./oracle c284 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c285-j1 $$(./oracle c285 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c285-j0 $$(./oracle c285 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c286-j1 $$(./oracle c286 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c287-j1 $$(./oracle c287 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c288-j1 $$(./oracle c288 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c289-j1 $$(./oracle c289 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c290-j1 $$(./oracle c290 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c291-j1 $$(./oracle c291 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c291-j0 $$(./oracle c291 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c292-j1 $$(./oracle c292 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c292-j0 $$(./oracle c292 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c293-j1 $$(./oracle c293 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c293-j0 $$(./oracle c293 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c294-j1 $$(./oracle c294 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c294-j0 $$(./oracle c294 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c295-j1 $$(./oracle c295 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c295-j0 $$(./oracle c295 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c296-j1 $$(./oracle c296 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c296-j0 $$(./oracle c296 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c297-j1 $$(./oracle c297 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c297-j0 $$(./oracle c297 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c298-j1 $$(./oracle c298 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c298-j0 $$(./oracle c298 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c299-j1 $$(./oracle c299 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c299-j0 $$(./oracle c299 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c300-j1 $$(./oracle c300 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c300-j0 $$(./oracle c300 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c301-j1 $$(./oracle c301 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c301-j0 $$(./oracle c301 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c302-j1 $$(./oracle c302 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c302-j0 $$(./oracle c302 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c303-j1 $$(./oracle c303 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c303-j0 $$(./oracle c303 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c304-j1 $$(./oracle c304 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c304-j0 $$(./oracle c304 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c305-j1 $$(./oracle c305 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c305-j0 $$(./oracle c305 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c306-j1 $$(./oracle c306 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c306-j0 $$(./oracle c306 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c307-j1 $$(./oracle c307 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c307-j0 $$(./oracle c307 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c308-j1 $$(./oracle c308 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c309-j1 $$(./oracle c309 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c309-j0 $$(./oracle c309 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c310-j1 $$(./oracle c310 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c311-j1 $$(./oracle c311 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c312-j1 $$(./oracle c312 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c313-j1 $$(./oracle c313 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c314-j1 $$(./oracle c314 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c315-j1 $$(./oracle c315 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c315-j0 $$(./oracle c315 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c316-j1 $$(./oracle c316 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c317-j1 $$(./oracle c317 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c318-j1 $$(./oracle c318 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c319-j1 $$(./oracle c319 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c320-j1 $$(./oracle c320 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c321-j1 $$(./oracle c321 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c322-j1 $$(./oracle c322 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c323-j1 $$(./oracle c323 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c324-j1 $$(./oracle c324 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c325-j1 $$(./oracle c325 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c325-j0 $$(./oracle c325 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c326-j1 $$(./oracle c326 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c327-j1 $$(./oracle c327 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c327-j0 $$(./oracle c327 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c328-j1 $$(./oracle c328 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c329-j1 $$(./oracle c329 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c330-j1 $$(./oracle c330 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c331-j1 $$(./oracle c331 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c332-j1 $$(./oracle c332 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c333-j1 $$(./oracle c333 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c333-j0 $$(./oracle c333 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c334-j1 $$(./oracle c334 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c335-j1 $$(./oracle c335 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c336-j1 $$(./oracle c336 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c337-j1 $$(./oracle c337 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c338-j1 $$(./oracle c338 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c339-j1 $$(./oracle c339 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c340-j1 $$(./oracle c340 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c341-j1 $$(./oracle c341 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c342-j1 $$(./oracle c342 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c343-j1 $$(./oracle c343 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c343-j0 $$(./oracle c343 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c344-j1 $$(./oracle c344 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c345-j1 $$(./oracle c345 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c345-j0 $$(./oracle c345 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c346-j1 $$(./oracle c346 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c347-j1 $$(./oracle c347 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c348-j1 $$(./oracle c348 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c349-j1 $$(./oracle c349 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c350-j1 $$(./oracle c350 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c351-j1 $$(./oracle c351 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c351-j0 $$(./oracle c351 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c352-j1 $$(./oracle c352 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c353-j1 $$(./oracle c353 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c354-j1 $$(./oracle c354 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c355-j1 $$(./oracle c355 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c356-j1 $$(./oracle c356 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c357-j1 $$(./oracle c357 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c358-j1 $$(./oracle c358 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c359-j1 $$(./oracle c359 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c360-j1 $$(./oracle c360 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c361-j1 $$(./oracle c361 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c361-j0 $$(./oracle c361 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c362-j1 $$(./oracle c362 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c363-j1 $$(./oracle c363 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c363-j0 $$(./oracle c363 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c364-j1 $$(./oracle c364 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c365-j1 $$(./oracle c365 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c366-j1 $$(./oracle c366 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c367-j1 $$(./oracle c367 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c368-j1 $$(./oracle c368 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c369-j1 $$(./oracle c369 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c369-j0 $$(./oracle c369 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c370-j1 $$(./oracle c370 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c371-j1 $$(./oracle c371 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c372-j1 $$(./oracle c372 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c373-j1 $$(./oracle c373 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c374-j1 $$(./oracle c374 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c375-j1 $$(./oracle c375 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c376-j1 $$(./oracle c376 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c377-j1 $$(./oracle c377 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c378-j1 $$(./oracle c378 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c379-j1 $$(./oracle c379 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c379-j0 $$(./oracle c379 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c380-j1 $$(./oracle c380 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c381-j1 $$(./oracle c381 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c381-j0 $$(./oracle c381 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c382-j1 $$(./oracle c382 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c383-j1 $$(./oracle c383 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c384-j1 $$(./oracle c384 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c385-j1 $$(./oracle c385 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c386-j1 $$(./oracle c386 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c387-j1 $$(./oracle c387 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c387-j0 $$(./oracle c387 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c388-j1 $$(./oracle c388 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c388-j0 $$(./oracle c388 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c389-j1 $$(./oracle c389 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c389-j0 $$(./oracle c389 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c390-j1 $$(./oracle c390 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c390-j0 $$(./oracle c390 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c391-j1 $$(./oracle c391 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c391-j0 $$(./oracle c391 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c392-j1 $$(./oracle c392 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c392-j0 $$(./oracle c392 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c393-j1 $$(./oracle c393 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c393-j0 $$(./oracle c393 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c394-j1 $$(./oracle c394 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c394-j0 $$(./oracle c394 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c395-j1 $$(./oracle c395 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c395-j0 $$(./oracle c395 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c396-j1 $$(./oracle c396 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c396-j0 $$(./oracle c396 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c397-j1 $$(./oracle c397 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c397-j0 $$(./oracle c397 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c398-j1 $$(./oracle c398 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c398-j0 $$(./oracle c398 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c399-j1 $$(./oracle c399 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c399-j0 $$(./oracle c399 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c400-j1 $$(./oracle c400 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c400-j0 $$(./oracle c400 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c401-j1 $$(./oracle c401 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c401-j0 $$(./oracle c401 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c402-j1 $$(./oracle c402 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c402-j0 $$(./oracle c402 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c403-j1 $$(./oracle c403 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c403-j0 $$(./oracle c403 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c404-j1 $$(./oracle c404 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c405-j1 $$(./oracle c405 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c405-j0 $$(./oracle c405 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c406-j1 $$(./oracle c406 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c407-j1 $$(./oracle c407 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c408-j1 $$(./oracle c408 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c409-j1 $$(./oracle c409 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c410-j1 $$(./oracle c410 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c411-j1 $$(./oracle c411 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c411-j0 $$(./oracle c411 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c412-j1 $$(./oracle c412 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c413-j1 $$(./oracle c413 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c414-j1 $$(./oracle c414 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c415-j1 $$(./oracle c415 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c416-j1 $$(./oracle c416 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c417-j1 $$(./oracle c417 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c418-j1 $$(./oracle c418 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c419-j1 $$(./oracle c419 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c420-j1 $$(./oracle c420 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c421-j1 $$(./oracle c421 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c421-j0 $$(./oracle c421 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c422-j1 $$(./oracle c422 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c423-j1 $$(./oracle c423 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c423-j0 $$(./oracle c423 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c424-j1 $$(./oracle c424 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c425-j1 $$(./oracle c425 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c426-j1 $$(./oracle c426 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c427-j1 $$(./oracle c427 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c428-j1 $$(./oracle c428 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c429-j1 $$(./oracle c429 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c429-j0 $$(./oracle c429 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c430-j1 $$(./oracle c430 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c431-j1 $$(./oracle c431 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c432-j1 $$(./oracle c432 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c433-j1 $$(./oracle c433 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c434-j1 $$(./oracle c434 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c435-j1 $$(./oracle c435 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c436-j1 $$(./oracle c436 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c437-j1 $$(./oracle c437 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c438-j1 $$(./oracle c438 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c439-j1 $$(./oracle c439 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c439-j0 $$(./oracle c439 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c440-j1 $$(./oracle c440 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c441-j1 $$(./oracle c441 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c441-j0 $$(./oracle c441 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c442-j1 $$(./oracle c442 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c443-j1 $$(./oracle c443 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c444-j1 $$(./oracle c444 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c445-j1 $$(./oracle c445 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c446-j1 $$(./oracle c446 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c447-j1 $$(./oracle c447 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c447-j0 $$(./oracle c447 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c448-j1 $$(./oracle c448 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c449-j1 $$(./oracle c449 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c450-j1 $$(./oracle c450 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c451-j1 $$(./oracle c451 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c452-j1 $$(./oracle c452 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c453-j1 $$(./oracle c453 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c454-j1 $$(./oracle c454 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c455-j1 $$(./oracle c455 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c456-j1 $$(./oracle c456 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c457-j1 $$(./oracle c457 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c457-j0 $$(./oracle c457 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c458-j1 $$(./oracle c458 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c459-j1 $$(./oracle c459 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c459-j0 $$(./oracle c459 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c460-j1 $$(./oracle c460 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c461-j1 $$(./oracle c461 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c462-j1 $$(./oracle c462 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c463-j1 $$(./oracle c463 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c464-j1 $$(./oracle c464 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c465-j1 $$(./oracle c465 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c465-j0 $$(./oracle c465 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c466-j1 $$(./oracle c466 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c467-j1 $$(./oracle c467 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c468-j1 $$(./oracle c468 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c469-j1 $$(./oracle c469 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c470-j1 $$(./oracle c470 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c471-j1 $$(./oracle c471 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c472-j1 $$(./oracle c472 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c473-j1 $$(./oracle c473 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c474-j1 $$(./oracle c474 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c475-j1 $$(./oracle c475 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c475-j0 $$(./oracle c475 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c476-j1 $$(./oracle c476 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c477-j1 $$(./oracle c477 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c477-j0 $$(./oracle c477 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c478-j1 $$(./oracle c478 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c479-j1 $$(./oracle c479 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c480-j1 $$(./oracle c480 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c481-j1 $$(./oracle c481 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c482-j1 $$(./oracle c482 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c483-j1 $$(./oracle c483 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c483-j0 $$(./oracle c483 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c484-j1 $$(./oracle c484 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c484-j0 $$(./oracle c484 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c485-j1 $$(./oracle c485 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c485-j0 $$(./oracle c485 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c486-j1 $$(./oracle c486 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c486-j0 $$(./oracle c486 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c487-j1 $$(./oracle c487 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c487-j0 $$(./oracle c487 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c488-j1 $$(./oracle c488 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c488-j0 $$(./oracle c488 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c489-j1 $$(./oracle c489 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c489-j0 $$(./oracle c489 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c490-j1 $$(./oracle c490 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c490-j0 $$(./oracle c490 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c491-j1 $$(./oracle c491 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c491-j0 $$(./oracle c491 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c492-j1 $$(./oracle c492 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c492-j0 $$(./oracle c492 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c493-j1 $$(./oracle c493 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c493-j0 $$(./oracle c493 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c494-j1 $$(./oracle c494 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c494-j0 $$(./oracle c494 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c495-j1 $$(./oracle c495 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c495-j0 $$(./oracle c495 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c496-j1 $$(./oracle c496 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c496-j0 $$(./oracle c496 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c497-j1 $$(./oracle c497 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c497-j0 $$(./oracle c497 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c498-j1 $$(./oracle c498 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c498-j0 $$(./oracle c498 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c499-j1 $$(./oracle c499 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c499-j0 $$(./oracle c499 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c500-j1 $$(./oracle c500 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c501-j1 $$(./oracle c501 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c501-j0 $$(./oracle c501 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c502-j1 $$(./oracle c502 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c503-j1 $$(./oracle c503 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c504-j1 $$(./oracle c504 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c505-j1 $$(./oracle c505 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c506-j1 $$(./oracle c506 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c507-j1 $$(./oracle c507 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c507-j0 $$(./oracle c507 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c508-j1 $$(./oracle c508 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c509-j1 $$(./oracle c509 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c510-j1 $$(./oracle c510 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c511-j1 $$(./oracle c511 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c512-j1 $$(./oracle c512 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c513-j1 $$(./oracle c513 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c514-j1 $$(./oracle c514 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c515-j1 $$(./oracle c515 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c516-j1 $$(./oracle c516 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c517-j1 $$(./oracle c517 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c517-j0 $$(./oracle c517 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c518-j1 $$(./oracle c518 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c519-j1 $$(./oracle c519 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c519-j0 $$(./oracle c519 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c520-j1 $$(./oracle c520 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c521-j1 $$(./oracle c521 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c522-j1 $$(./oracle c522 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c523-j1 $$(./oracle c523 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c524-j1 $$(./oracle c524 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c525-j1 $$(./oracle c525 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c525-j0 $$(./oracle c525 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c526-j1 $$(./oracle c526 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c527-j1 $$(./oracle c527 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c528-j1 $$(./oracle c528 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c529-j1 $$(./oracle c529 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c530-j1 $$(./oracle c530 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c531-j1 $$(./oracle c531 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c532-j1 $$(./oracle c532 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c533-j1 $$(./oracle c533 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c534-j1 $$(./oracle c534 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c535-j1 $$(./oracle c535 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c535-j0 $$(./oracle c535 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c536-j1 $$(./oracle c536 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c537-j1 $$(./oracle c537 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c537-j0 $$(./oracle c537 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c538-j1 $$(./oracle c538 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c539-j1 $$(./oracle c539 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c540-j1 $$(./oracle c540 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c541-j1 $$(./oracle c541 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c542-j1 $$(./oracle c542 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c543-j1 $$(./oracle c543 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c543-j0 $$(./oracle c543 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c544-j1 $$(./oracle c544 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c545-j1 $$(./oracle c545 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c546-j1 $$(./oracle c546 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c547-j1 $$(./oracle c547 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c548-j1 $$(./oracle c548 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c549-j1 $$(./oracle c549 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c550-j1 $$(./oracle c550 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c551-j1 $$(./oracle c551 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c552-j1 $$(./oracle c552 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c553-j1 $$(./oracle c553 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c553-j0 $$(./oracle c553 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c554-j1 $$(./oracle c554 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c555-j1 $$(./oracle c555 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c555-j0 $$(./oracle c555 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c556-j1 $$(./oracle c556 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c557-j1 $$(./oracle c557 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c558-j1 $$(./oracle c558 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c559-j1 $$(./oracle c559 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c560-j1 $$(./oracle c560 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c561-j1 $$(./oracle c561 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c561-j0 $$(./oracle c561 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c562-j1 $$(./oracle c562 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c563-j1 $$(./oracle c563 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c564-j1 $$(./oracle c564 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c565-j1 $$(./oracle c565 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c566-j1 $$(./oracle c566 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c567-j1 $$(./oracle c567 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c568-j1 $$(./oracle c568 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c569-j1 $$(./oracle c569 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c570-j1 $$(./oracle c570 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c571-j1 $$(./oracle c571 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c571-j0 $$(./oracle c571 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c572-j1 $$(./oracle c572 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c573-j1 $$(./oracle c573 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c573-j0 $$(./oracle c573 7 0 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c574-j1 $$(./oracle c574 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c575-j1 $$(./oracle c575 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c576-j1 $$(./oracle c576 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c577-j1 $$(./oracle c577 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c578-j1 $$(./oracle c578 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c579-j1 $$(./oracle c579 7 1 2>> oracle.err || echo failed)" >> oracle.txt
	echo "c579-j0 $$(./oracle c579 7 0 2>> oracle.err || echo failed)" >> oracle.txt

clean:
	rm -rf nexus oracle asan pools __pycache__ *-build *build.tmp*
	rm -f *.err run.txt asan.txt
