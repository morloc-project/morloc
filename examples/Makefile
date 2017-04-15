.PHONY: 1
1:
	morloc -kt -o hello_world -x pools 1_hello_world.loc

.PHONY: 2
2:
	morloc -kt -o composition -x pools 2_composition.loc

.PHONY: 3
3:
	morloc -kt -o modifiers -x pools 3_modifiers.loc

.PHONY: 4
4:
	morloc -kt -o hooks -x pools 4_hooks.loc

.PHONY: 5
5:
	morloc -kt -o types -x pools 5_types.loc

.PHONY: 6
6:
	morloc -kt -o filesize -x pools 6_filesize.loc

.PHONY: 7
7:
	morloc -kt -o times -x pools 7_times.loc

.PHONY: 8
8:
	morloc -kt -o glue -x pools 8_glue.loc

.PHONY: 9
9:
	morloc -kt -o better_r -x pools 9_better_r.loc

.PHONY: 10
10:
	morloc -kt -o wc -x pools 10_wc.loc

.PHONY: 11
11:
	morloc -kt -o filter -x pools 11_filter.loc

.PHONY: 12
12:
	morloc -kt -o gff -x pools 12_read_gff.loc

.PHONY: clean
clean:
	rm -rf pools hello_world composition modifiers \
		hooks *pdf manifold-nexus.py \
		sample* results types filesize times \
		glue *gff3 result.txt \
		better_r wc [abc].txt filter \
		gff al/ \
		__pycache__
