export PATH := $(CURDIR)/mock-bin:$(PATH)
export MOCK_SLURM_DIR := $(CURDIR)/slurm
export MORLOC_CACHE_BASE := $(CURDIR)/cache
RUN = timeout 120 ./nexus
JOBS = printf 'jobs %s\n' $$(cat slurm/jobs 2>/dev/null | wc -l) >> obs.txt

all:
	rm -rf *.err obs.txt slurm cache
	morloc make --slurm -o nexus main.loc 2> build.err
	$(RUN) whole 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) mid 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) mapped [1,2] >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) defined 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) definedMap [1,2] >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) rec [1,2,3] >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) pyToCpp 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) cppToPy 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) pyToR 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) rToPy 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) pyToRs 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) rsToPy 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) effectful 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) heldSusp 1 >> obs.txt 2>> obs.err
	$(JOBS)
	$(RUN) failing 1 >> obs.txt 2>> obs.err; echo "exit $$?" >> obs.txt
	MOCK_SBATCH_STATE=FAILED $(RUN) mid 2 >> obs.txt 2>> obs.err; echo "exit $$?" >> obs.txt
	MOCK_SBATCH_STATE=TIMEOUT $(RUN) mid 3 >> obs.txt 2>> obs.err; echo "exit $$?" >> obs.txt
	sort -u slurm/options >> obs.txt

clean:
	rm -rf nexus __pycache__ slurm cache *.err
	rm -rf *-build *build.tmp*
