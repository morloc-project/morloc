all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus keys '{"body":"{\"a\":1,\"b\":2}","tag":"x"}' >> obs.txt 2>> obs.err
	grep -c "#include <nlohmann/json.hpp>" nexus-build/pools/types/cpp/mlc/types/_local/main.hpp >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools *.err *-build *build.tmp*
