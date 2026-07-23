SHELL := /bin/bash

# Per-case helper: run the export, check that the expected substring
# appears in the run's stderr, print OK / FAIL on obs.txt. Failing
# cases (twoStd*, openDev*) exit non-zero; success cases (reopen,
# allThreeAtOnce) exit zero. All stderr is preserved in obs.err for
# post-mortem.
define check_err
	@./nexus $(1) > /dev/null 2> case.err ; true
	@if grep -q "$(2)" case.err; then \
	   echo "$(1): OK" >> obs.txt ; \
	 else \
	   echo "$(1): FAIL" >> obs.txt ; \
	   cat case.err >> obs.txt ; \
	 fi
	@cat case.err >> obs.err
	@rm -f case.err
endef

define check_ok
	@./nexus $(1) > /dev/null 2> case.err ; rc=$$? ; \
	  if [ $$rc -eq 0 ]; then echo "$(1): OK" >> obs.txt ; \
	  else echo "$(1): FAIL (rc=$$rc)" >> obs.txt ; cat case.err >> obs.txt ; fi
	@cat case.err >> obs.err
	@rm -f case.err
endef

all:
	rm -f *.err obs.txt obs.err
	morloc make -o nexus main.loc 2> build.err
	$(call check_err,twoStdin,already open)
	$(call check_err,twoStdout,already open)
	$(call check_err,twoStderr,already open)
	$(call check_ok,reopenAfterClose)
	$(call check_ok,allThreeAtOnce)
	$(call check_err,openDevStdin,evaluated directly by the nexus)
	$(call check_err,openDevStdout,refused)
	$(call check_err,openDevStderr,refused)

clean:
	rm -rf nexus pools *.err
