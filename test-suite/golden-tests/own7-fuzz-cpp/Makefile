all:
	rm -f *.err obs.txt run.txt oracle.txt asan.txt
	morloc make -o nexus main.loc 2> build.err
	MORLOC_NO_MOVES=1 morloc make -o oracle main.loc 2> oracle-build.err
	CXX="$${CXX:-g++} -fsanitize=address,undefined -fno-sanitize-recover=all" morloc make -o asan main.loc 2> asan-build.err
	echo "p000 $$(./nexus p000 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p000 $$(./oracle p000 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p000 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p000 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p001 $$(./nexus p001 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p001 $$(./oracle p001 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p001 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p001 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p002 $$(./nexus p002 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p002 $$(./oracle p002 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p002 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p002 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p003 $$(./nexus p003 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p003 $$(./oracle p003 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p003 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p003 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p004 $$(./nexus p004 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p004 $$(./oracle p004 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p004 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p004 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p005 $$(./nexus p005 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p005 $$(./oracle p005 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p005 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p005 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p006 $$(./nexus p006 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p006 $$(./oracle p006 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p006 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p006 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p007 $$(./nexus p007 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p007 $$(./oracle p007 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p007 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p007 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p008 $$(./nexus p008 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p008 $$(./oracle p008 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p008 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p008 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p009 $$(./nexus p009 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p009 $$(./oracle p009 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p009 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p009 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p010 $$(./nexus p010 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p010 $$(./oracle p010 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p010 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p010 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p011 $$(./nexus p011 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p011 $$(./oracle p011 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p011 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p011 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p012 $$(./nexus p012 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p012 $$(./oracle p012 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p012 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p012 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p013 $$(./nexus p013 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p013 $$(./oracle p013 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p013 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p013 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p014 $$(./nexus p014 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p014 $$(./oracle p014 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p014 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p014 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p015 $$(./nexus p015 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p015 $$(./oracle p015 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p015 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p015 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p016 $$(./nexus p016 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p016 $$(./oracle p016 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p016 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p016 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p017 $$(./nexus p017 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p017 $$(./oracle p017 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p017 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p017 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p018 $$(./nexus p018 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p018 $$(./oracle p018 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p018 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p018 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p019 $$(./nexus p019 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p019 $$(./oracle p019 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p019 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p019 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p020 $$(./nexus p020 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p020 $$(./oracle p020 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p020 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p020 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p021 $$(./nexus p021 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p021 $$(./oracle p021 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p021 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p021 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p022 $$(./nexus p022 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p022 $$(./oracle p022 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p022 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p022 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p023 $$(./nexus p023 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p023 $$(./oracle p023 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p023 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p023 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p024 $$(./nexus p024 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p024 $$(./oracle p024 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p024 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p024 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p025 $$(./nexus p025 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p025 $$(./oracle p025 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p025 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p025 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p026 $$(./nexus p026 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p026 $$(./oracle p026 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p026 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p026 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p027 $$(./nexus p027 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p027 $$(./oracle p027 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p027 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p027 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p028 $$(./nexus p028 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p028 $$(./oracle p028 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p028 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p028 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p029 $$(./nexus p029 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p029 $$(./oracle p029 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p029 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p029 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p030 $$(./nexus p030 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p030 $$(./oracle p030 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p030 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p030 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p031 $$(./nexus p031 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p031 $$(./oracle p031 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p031 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p031 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p032 $$(./nexus p032 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p032 $$(./oracle p032 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p032 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p032 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p033 $$(./nexus p033 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p033 $$(./oracle p033 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p033 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p033 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p034 $$(./nexus p034 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p034 $$(./oracle p034 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p034 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p034 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p035 $$(./nexus p035 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p035 $$(./oracle p035 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p035 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p035 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p036 $$(./nexus p036 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p036 $$(./oracle p036 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p036 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p036 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p037 $$(./nexus p037 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p037 $$(./oracle p037 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p037 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p037 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p038 $$(./nexus p038 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p038 $$(./oracle p038 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p038 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p038 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p039 $$(./nexus p039 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p039 $$(./oracle p039 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p039 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p039 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p040 $$(./nexus p040 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p040 $$(./oracle p040 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p040 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p040 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p041 $$(./nexus p041 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p041 $$(./oracle p041 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p041 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p041 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p042 $$(./nexus p042 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p042 $$(./oracle p042 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p042 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p042 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p043 $$(./nexus p043 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p043 $$(./oracle p043 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p043 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p043 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p044 $$(./nexus p044 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p044 $$(./oracle p044 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p044 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p044 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p045 $$(./nexus p045 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p045 $$(./oracle p045 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p045 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p045 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p046 $$(./nexus p046 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p046 $$(./oracle p046 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p046 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p046 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p047 $$(./nexus p047 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p047 $$(./oracle p047 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p047 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p047 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p048 $$(./nexus p048 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p048 $$(./oracle p048 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p048 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p048 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p049 $$(./nexus p049 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p049 $$(./oracle p049 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p049 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p049 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p050 $$(./nexus p050 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p050 $$(./oracle p050 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p050 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p050 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p051 $$(./nexus p051 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p051 $$(./oracle p051 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p051 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p051 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p052 $$(./nexus p052 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p052 $$(./oracle p052 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p052 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p052 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p053 $$(./nexus p053 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p053 $$(./oracle p053 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p053 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p053 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p054 $$(./nexus p054 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p054 $$(./oracle p054 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p054 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p054 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p055 $$(./nexus p055 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p055 $$(./oracle p055 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p055 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p055 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p056 $$(./nexus p056 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p056 $$(./oracle p056 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p056 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p056 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p057 $$(./nexus p057 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p057 $$(./oracle p057 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p057 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p057 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p058 $$(./nexus p058 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p058 $$(./oracle p058 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p058 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p058 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p059 $$(./nexus p059 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p059 $$(./oracle p059 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p059 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p059 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p060 $$(./nexus p060 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p060 $$(./oracle p060 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p060 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p060 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p061 $$(./nexus p061 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p061 $$(./oracle p061 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p061 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p061 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p062 $$(./nexus p062 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p062 $$(./oracle p062 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p062 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p062 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p063 $$(./nexus p063 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p063 $$(./oracle p063 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p063 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p063 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p064 $$(./nexus p064 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p064 $$(./oracle p064 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p064 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p064 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p065 $$(./nexus p065 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p065 $$(./oracle p065 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p065 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p065 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p066 $$(./nexus p066 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p066 $$(./oracle p066 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p066 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p066 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p067 $$(./nexus p067 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p067 $$(./oracle p067 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p067 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p067 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p068 $$(./nexus p068 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p068 $$(./oracle p068 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p068 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p068 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p069 $$(./nexus p069 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p069 $$(./oracle p069 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p069 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p069 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p070 $$(./nexus p070 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p070 $$(./oracle p070 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p070 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p070 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p071 $$(./nexus p071 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p071 $$(./oracle p071 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p071 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p071 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p072 $$(./nexus p072 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p072 $$(./oracle p072 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p072 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p072 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p073 $$(./nexus p073 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p073 $$(./oracle p073 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p073 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p073 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p074 $$(./nexus p074 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p074 $$(./oracle p074 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p074 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p074 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p075 $$(./nexus p075 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p075 $$(./oracle p075 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p075 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p075 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p076 $$(./nexus p076 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p076 $$(./oracle p076 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p076 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p076 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p077 $$(./nexus p077 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p077 $$(./oracle p077 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p077 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p077 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p078 $$(./nexus p078 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p078 $$(./oracle p078 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p078 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p078 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p079 $$(./nexus p079 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p079 $$(./oracle p079 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p079 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p079 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p080 $$(./nexus p080 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p080 $$(./oracle p080 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p080 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p080 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p081 $$(./nexus p081 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p081 $$(./oracle p081 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p081 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p081 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p082 $$(./nexus p082 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p082 $$(./oracle p082 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p082 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p082 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p083 $$(./nexus p083 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p083 $$(./oracle p083 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p083 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p083 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p084 $$(./nexus p084 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p084 $$(./oracle p084 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p084 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p084 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p085 $$(./nexus p085 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p085 $$(./oracle p085 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p085 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p085 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p086 $$(./nexus p086 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p086 $$(./oracle p086 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p086 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p086 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p087 $$(./nexus p087 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p087 $$(./oracle p087 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p087 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p087 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p088 $$(./nexus p088 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p088 $$(./oracle p088 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p088 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p088 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p089 $$(./nexus p089 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p089 $$(./oracle p089 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p089 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p089 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p090 $$(./nexus p090 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p090 $$(./oracle p090 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p090 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p090 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p091 $$(./nexus p091 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p091 $$(./oracle p091 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p091 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p091 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p092 $$(./nexus p092 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p092 $$(./oracle p092 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p092 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p092 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p093 $$(./nexus p093 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p093 $$(./oracle p093 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p093 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p093 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p094 $$(./nexus p094 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p094 $$(./oracle p094 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p094 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p094 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p095 $$(./nexus p095 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p095 $$(./oracle p095 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p095 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p095 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p096 $$(./nexus p096 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p096 $$(./oracle p096 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p096 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p096 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p097 $$(./nexus p097 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p097 $$(./oracle p097 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p097 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p097 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p098 $$(./nexus p098 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p098 $$(./oracle p098 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p098 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p098 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p099 $$(./nexus p099 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p099 $$(./oracle p099 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p099 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p099 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p100 $$(./nexus p100 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p100 $$(./oracle p100 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p100 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p100 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p101 $$(./nexus p101 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p101 $$(./oracle p101 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p101 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p101 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p102 $$(./nexus p102 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p102 $$(./oracle p102 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p102 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p102 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p103 $$(./nexus p103 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p103 $$(./oracle p103 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p103 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p103 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p104 $$(./nexus p104 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p104 $$(./oracle p104 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p104 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p104 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p105 $$(./nexus p105 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p105 $$(./oracle p105 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p105 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p105 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p106 $$(./nexus p106 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p106 $$(./oracle p106 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p106 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p106 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p107 $$(./nexus p107 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p107 $$(./oracle p107 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p107 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p107 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p108 $$(./nexus p108 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p108 $$(./oracle p108 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p108 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p108 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p109 $$(./nexus p109 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p109 $$(./oracle p109 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p109 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p109 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p110 $$(./nexus p110 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p110 $$(./oracle p110 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p110 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p110 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p111 $$(./nexus p111 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p111 $$(./oracle p111 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p111 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p111 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p112 $$(./nexus p112 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p112 $$(./oracle p112 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p112 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p112 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p113 $$(./nexus p113 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p113 $$(./oracle p113 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p113 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p113 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p114 $$(./nexus p114 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p114 $$(./oracle p114 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p114 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p114 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p115 $$(./nexus p115 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p115 $$(./oracle p115 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p115 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p115 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p116 $$(./nexus p116 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p116 $$(./oracle p116 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p116 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p116 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p117 $$(./nexus p117 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p117 $$(./oracle p117 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p117 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p117 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p118 $$(./nexus p118 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p118 $$(./oracle p118 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p118 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p118 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p119 $$(./nexus p119 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p119 $$(./oracle p119 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p119 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p119 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p120 $$(./nexus p120 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p120 $$(./oracle p120 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p120 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p120 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p121 $$(./nexus p121 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p121 $$(./oracle p121 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p121 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p121 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p122 $$(./nexus p122 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p122 $$(./oracle p122 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p122 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p122 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p123 $$(./nexus p123 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p123 $$(./oracle p123 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p123 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p123 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p124 $$(./nexus p124 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p124 $$(./oracle p124 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p124 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p124 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p125 $$(./nexus p125 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p125 $$(./oracle p125 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p125 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p125 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p126 $$(./nexus p126 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p126 $$(./oracle p126 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p126 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p126 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p127 $$(./nexus p127 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p127 $$(./oracle p127 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p127 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p127 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p128 $$(./nexus p128 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p128 $$(./oracle p128 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p128 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p128 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p129 $$(./nexus p129 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p129 $$(./oracle p129 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p129 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p129 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p130 $$(./nexus p130 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p130 $$(./oracle p130 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p130 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p130 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p131 $$(./nexus p131 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p131 $$(./oracle p131 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p131 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p131 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p132 $$(./nexus p132 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p132 $$(./oracle p132 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p132 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p132 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p133 $$(./nexus p133 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p133 $$(./oracle p133 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p133 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p133 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p134 $$(./nexus p134 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p134 $$(./oracle p134 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p134 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p134 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p135 $$(./nexus p135 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p135 $$(./oracle p135 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p135 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p135 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p136 $$(./nexus p136 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p136 $$(./oracle p136 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p136 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p136 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p137 $$(./nexus p137 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p137 $$(./oracle p137 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p137 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p137 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p138 $$(./nexus p138 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p138 $$(./oracle p138 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p138 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p138 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p139 $$(./nexus p139 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p139 $$(./oracle p139 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p139 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p139 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p140 $$(./nexus p140 7 2 2>> obs.err || echo failed)" >> run.txt
	echo "p140 $$(./oracle p140 7 2 2>> obs.err || echo failed)" >> oracle.txt
	echo "p140 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p140 7 2 2>> obs.err || echo failed)" >> asan.txt
	echo "p141 $$(./nexus p141 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p141 $$(./oracle p141 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p141 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p141 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p142 $$(./nexus p142 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p142 $$(./oracle p142 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p142 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p142 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p143 $$(./nexus p143 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p143 $$(./oracle p143 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p143 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p143 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p144 $$(./nexus p144 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p144 $$(./oracle p144 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p144 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p144 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p145 $$(./nexus p145 7 1 2>> obs.err || echo failed)" >> run.txt
	echo "p145 $$(./oracle p145 7 1 2>> obs.err || echo failed)" >> oracle.txt
	echo "p145 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p145 7 1 2>> obs.err || echo failed)" >> asan.txt
	echo "p146 $$(./nexus p146 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p146 $$(./oracle p146 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p146 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p146 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p147 $$(./nexus p147 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p147 $$(./oracle p147 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p147 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p147 7 3 2>> obs.err || echo failed)" >> asan.txt
	echo "p148 $$(./nexus p148 7 0 2>> obs.err || echo failed)" >> run.txt
	echo "p148 $$(./oracle p148 7 0 2>> obs.err || echo failed)" >> oracle.txt
	echo "p148 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p148 7 0 2>> obs.err || echo failed)" >> asan.txt
	echo "p149 $$(./nexus p149 7 3 2>> obs.err || echo failed)" >> run.txt
	echo "p149 $$(./oracle p149 7 3 2>> obs.err || echo failed)" >> oracle.txt
	echo "p149 $$(ASAN_OPTIONS=detect_leaks=0 ./asan p149 7 3 2>> obs.err || echo failed)" >> asan.txt
	python3 check.py run.txt oracle.txt asan.txt > obs.txt

clean:
	rm -rf nexus oracle asan pools __pycache__ *-build *build.tmp*
	rm -f *.err run.txt oracle.txt asan.txt
