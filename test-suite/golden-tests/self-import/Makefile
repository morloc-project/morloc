all:
	rm -f *.err obs.txt
	# Bound the run time so a regression to OOM is caught. A module
	# that imports itself must be rejected with a clear cycle
	# diagnostic, never an OOM.
	timeout 10 morloc typecheck main.loc > obs.txt 2>&1 || true

clean:
	rm -f *.err
