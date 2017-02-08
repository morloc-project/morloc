.PHONY: 1
1:
	loc -k -o hello_world -x pools 1_hello_world.loc	

.PHONY: 2
2:
	loc -k -o composition -x pools 2_composition.loc	

.PHONY: 3
3:
	loc -k -o modifiers -x pools 3_modifiers.loc	


.PHONY: 4
4:
	loc -k -o hooks -x pools 4_hooks.loc	

.PHONY: clean
clean:
	rm -rf pools hello_world composition modifiers \
		hooks *pdf manifold-nexus.py \
		sample* results
