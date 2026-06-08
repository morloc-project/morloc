# Verifies the per-labeled-step workdir story:
#
#   1. Labeled call (foo) triggers materialization of MORLOC_RUN_BASE/<id>/
#      with start.json, summary.json (clean exit), and a/log (tee from
#      Stage 1 log emit).
#   2. Two consecutive labeled runs create two distinct run dirs (run
#      IDs are unique; existing dirs aren't overwritten or reaped).
#   3. Unlabeled call (bar) generates a run id internally but leaves
#      ZERO filesystem trace -- ensure_run_dir is never called.
#
# MORLOC_RUN_BASE is pinned to ./runs so the test is self-contained
# and doesn't pollute the user's ~/.cache/morloc/runs.

INPUT := '[1,2,3.5]'

all:
	rm -f *.err obs.txt
	rm -rf runs
	mkdir runs
	morloc make -o nexus main.loc 2> build.err
	MORLOC_RUN_BASE=$$PWD/runs ./nexus foo $(INPUT) >> obs.txt 2> obs.foo1.err
	MORLOC_RUN_BASE=$$PWD/runs ./nexus foo $(INPUT) >> obs.txt 2> obs.foo2.err
	@# Snapshot: 2 labeled runs -> 2 run dirs.
	@echo "run_dirs_after_foo: $$(ls runs | wc -l)" >> obs.txt
	@# Each run dir must contain the same three artifacts.
	@for d in runs/*; do \
	  ls $$d | sort | xargs echo files: >> obs.txt; \
	done
	@# Each a/log must contain exactly 2 lines (1 start + 1 pass per call).
	@for d in runs/*; do \
	  echo "a_log_lines: $$(wc -l < $$d/a/log)" >> obs.txt; \
	done
	@# Sentinel content: start.json has run_id+pid; summary.json reports exit 0.
	@for d in runs/*; do \
	  echo "start_run_id_field: $$(grep -c '"run_id"' $$d/start.json)" >> obs.txt; \
	  echo "start_pid_field: $$(grep -c '"pid"' $$d/start.json)" >> obs.txt; \
	  echo "summary_exit_0: $$(grep -c '\"exit_code\": 0' $$d/summary.json)" >> obs.txt; \
	done
	@# Lazy path: an unlabeled call must NOT add a third directory.
	MORLOC_RUN_BASE=$$PWD/runs ./nexus bar $(INPUT) >> obs.txt 2> obs.bar.err
	@echo "run_dirs_after_bar: $$(ls runs | wc -l)" >> obs.txt
	cat obs.foo1.err obs.foo2.err obs.bar.err > obs.err

clean:
	rm -rf nexus pools runs *.err obs.txt
