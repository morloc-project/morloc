all:
	: > obs.txt
	rm -f *.err
	# The build is expected to fail: IntrMap synth in Typecheck.hs
	# leaks an unsolved existential (`map_result_<N>`) into the
	# serialization codegen for `.[0:2].0 xs` on `[(Int, Int)]`.
	# When the underlying bug is fixed, the build will succeed and
	# the grep below will fail, breaking this test on purpose so
	# someone comes back and deletes the pin.
	morloc make -o prog main.loc > build.log 2> build.err || true
	# Print a stable single-line witness. The substring
	# `map_result_` is the existential name from IntrMap's synth
	# clause -- any wording change to the codegen diagnostic that
	# still names the leaked existential will keep the pin intact.
	if grep -q "cannot serialize type: map_result_" build.err; then \
	    echo "cannot serialize type: map_result_<N>" > obs.txt; \
	fi

clean:
	rm -f build.log build.err obs.err prog *.manifest
	rm -rf pools __pycache__
