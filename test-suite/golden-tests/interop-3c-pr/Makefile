# DIAGNOSTIC HARNESS (temporary): this test flakes on macOS with a silent
# "Connection closed by peer". Run the re-entrant pmap call in a loop with
# MORLOC_TRACE_CLOSE=1 (every socket close logs pid + backtrace) and, on the
# first failing iteration, dump that iteration's full stderr -- including the
# callee pool's close_socket backtrace -- into obs.txt/obs.err so we can see
# exactly which close() fires and from what stack. Passes (matches exp.txt)
# when all iterations succeed; fails and surfaces the trace when the flake hits.
N := 200

all:
	rm -f *.err obs.txt run.out run.err
	morloc make -o nexus main.loc 2> build.err
	@n=0; fail=0; \
	while [ $$n -lt $(N) ]; do \
	  n=$$((n+1)); \
	  MORLOC_TRACE_CLOSE=1 RUST_BACKTRACE=1 ./nexus foo 4 [1,2,3] > run.out 2> run.err; \
	  rc=$$?; \
	  out=$$(cat run.out); \
	  if [ $$rc -ne 0 ] || [ "$$out" != "[5,6,7]" ]; then \
	    echo "FAILED at iteration $$n (rc=$$rc)" >> obs.txt; \
	    echo "=== stdout ===" >> obs.txt; cat run.out >> obs.txt; \
	    echo "=== IPC marker counts (dispatch vs receive vs serve gap) ===" >> obs.txt; \
	    for m in "accept fd=" "wait_for_client return" "send_fd client=" "recv_fd OK" "recv_fd FAIL" "recv_fd NOCMSG" "recv_fd EINTR" "run_job ENTER" "run_job EXIT" "recv EOF" "sent request" "ping-answered"; do \
	      c=$$(grep -c "$$m" run.err 2>/dev/null || echo 0); \
	      echo "  [$$c] $$m" >> obs.txt; \
	    done; \
	    echo "=== stderr (nexus error + [MLC_CLOSE]/[MLC_IPC] pid/fd traces) ===" >> obs.txt; \
	    cat run.err >> obs.txt; \
	    fail=1; break; \
	  fi; \
	done; \
	if [ $$fail -eq 0 ]; then echo "no failure in $(N) iterations" >> obs.txt; fi
	cp obs.txt obs.err
	rm -f run.out run.err

clean:
	rm -rf nexus pools *pdf run.out run.err
	rm -rf *-build *build.tmp*
