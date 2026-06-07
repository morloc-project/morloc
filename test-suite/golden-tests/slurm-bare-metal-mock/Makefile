# Bare-metal SLURM dispatch round-trip with mocked sbatch/sacct.
#
# Prerequisite: `morloc init -f --slurm` must have been run at least
# once so libmorloc.so was compiled with the slurm path enabled and
# ~/.config/morloc/build.yaml has `slurm-support: true`. Without that,
# `decideRemoteness` returns ForeignCall instead of RemoteCall and no
# sbatch invocation is generated.
#
# The mock shims in ./mock-bin run the sbatch --wrap synchronously and
# report COMPLETED, so the whole pipeline (codegen -> remote_call ->
# sbatch -> nexus --call-packet -> result cache -> readback) exercises
# without a real cluster.
all:
	rm -f *.err obs.txt
	PATH="$(CURDIR)/mock-bin:$$PATH" morloc make -o nexus main.loc 2> build.err
	PATH="$(CURDIR)/mock-bin:$$PATH" ./nexus foo 5 > obs.txt 2> obs.err

clean:
	rm -rf nexus pools __pycache__ .morloc-cache
