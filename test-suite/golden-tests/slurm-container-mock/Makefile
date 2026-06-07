# Container-mode SLURM dispatch round-trip with mocked sbatch/sacct.
#
# Prerequisites (run once):
#   1. `morloc init -f --slurm` — enables RemoteCall codegen
#   2. An Apptainer env exists and is active:
#        morloc-manager new e2e-slurm --engine apptainer
#        morloc-manager run -- morloc init -f --slurm
#        morloc-manager run -- morloc install root root-py
#        morloc-manager select e2e-slurm
#
# The mock sbatch on the host PATH strips the `apptainer exec` wrap the
# bridge produces and runs the inner nexus command on the test host.
# That's equivalent to "compute node = host". The container has
# neither sbatch nor sacct, so successful execution proves the bridge
# handled the submission.
all:
	rm -f *.err obs.txt
	PATH="$(CURDIR)/mock-bin:$$PATH" \
	morloc-manager run --slurm-bridge -- bash -c \
	  "morloc make -o nexus main.loc && ./nexus foo 5" \
	  > obs.txt 2> obs.err

clean:
	rm -rf nexus pools __pycache__ .morloc-cache
