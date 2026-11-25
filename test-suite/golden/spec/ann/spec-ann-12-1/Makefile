export PATH := $(CURDIR)/mock-bin:$(PATH)
export MOCK_SLURM_DIR := $(CURDIR)/slurm
export MORLOC_CACHE_BASE := $(CURDIR)/cache
RUN = timeout 120 ./nexus
JOBS = printf 'jobs %s kept %s\n' $$(cat slurm/jobs 2>/dev/null | wc -l) $$(ls cache/_remote 2>/dev/null | wc -l) >> obs.txt

all:
	rm -rf *.err obs.txt slurm cache
	morloc make --slurm -o nexus main.loc 2> build.err
	$(RUN) plain 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) plain 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) cached 2 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) cached 2 >> obs.txt 2>> obs.err
	$(JOBS)
	MOCK_SBATCH_STATE=FAILED $(RUN) plain 3 >> obs.txt 2>> obs.err; echo "exit $$?" >> obs.txt
	$(JOBS)

clean:
	rm -rf nexus __pycache__ slurm cache *.err
	rm -rf *-build *build.tmp*
