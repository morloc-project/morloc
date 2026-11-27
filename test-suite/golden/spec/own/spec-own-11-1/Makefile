all:
	rm -f *.err obs.txt
	morloc make -o ncpp main-cpp.loc 2>> build.err
	echo "--- cpp tDiff 5 ---" >> obs.txt
	./ncpp tDiff 5 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tSwap 5 ---" >> obs.txt
	./ncpp tSwap 5 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tOnce 4 ---" >> obs.txt
	./ncpp tOnce 4 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tFail 3 ---" >> obs.txt
	./ncpp tFail 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tGuard 5 ---" >> obs.txt
	./ncpp tGuard 5 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tGuard 1 ---" >> obs.txt
	./ncpp tGuard 1 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tWild 2 ---" >> obs.txt
	./ncpp tWild 2 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tNested 0 ---" >> obs.txt
	./ncpp tNested 0 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tNested 3 ---" >> obs.txt
	./ncpp tNested 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tShadow 3 ---" >> obs.txt
	./ncpp tShadow 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tTwo 3 ---" >> obs.txt
	./ncpp tTwo 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tIO 4 ---" >> obs.txt
	./ncpp tIO 4 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tTry 3 ---" >> obs.txt
	./ncpp tTry 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tTryOk 3 ---" >> obs.txt
	./ncpp tTryOk 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- cpp tLabel 3 ---" >> obs.txt
	./ncpp tLabel 3 >> obs.txt 2> label.err || echo failed >> obs.txt
	grep -c ' start' label.err >> obs.txt || echo 0 >> obs.txt
	echo "--- cpp tFrame 3 ---" >> obs.txt
	./ncpp tFrame 3 2> frame.err >> obs.txt || grep -c 'at failStep' frame.err >> obs.txt
	morloc make -o nrust main-rust.loc 2>> build.err
	echo "--- rust tDiff 5 ---" >> obs.txt
	./nrust tDiff 5 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tSwap 5 ---" >> obs.txt
	./nrust tSwap 5 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tOnce 4 ---" >> obs.txt
	./nrust tOnce 4 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tFail 3 ---" >> obs.txt
	./nrust tFail 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tGuard 5 ---" >> obs.txt
	./nrust tGuard 5 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tGuard 1 ---" >> obs.txt
	./nrust tGuard 1 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tWild 2 ---" >> obs.txt
	./nrust tWild 2 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tNested 0 ---" >> obs.txt
	./nrust tNested 0 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tNested 3 ---" >> obs.txt
	./nrust tNested 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tShadow 3 ---" >> obs.txt
	./nrust tShadow 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tTwo 3 ---" >> obs.txt
	./nrust tTwo 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tIO 4 ---" >> obs.txt
	./nrust tIO 4 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tTry 3 ---" >> obs.txt
	./nrust tTry 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tTryOk 3 ---" >> obs.txt
	./nrust tTryOk 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- rust tLabel 3 ---" >> obs.txt
	./nrust tLabel 3 >> obs.txt 2> label.err || echo failed >> obs.txt
	grep -c ' start' label.err >> obs.txt || echo 0 >> obs.txt
	echo "--- rust tFrame 3 ---" >> obs.txt
	./nrust tFrame 3 2> frame.err >> obs.txt || grep -c 'at failStep' frame.err >> obs.txt
	morloc make -o npy main-py.loc 2>> build.err
	echo "--- py tDiff 5 ---" >> obs.txt
	./npy tDiff 5 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tSwap 5 ---" >> obs.txt
	./npy tSwap 5 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tOnce 4 ---" >> obs.txt
	./npy tOnce 4 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tFail 3 ---" >> obs.txt
	./npy tFail 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tGuard 5 ---" >> obs.txt
	./npy tGuard 5 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tGuard 1 ---" >> obs.txt
	./npy tGuard 1 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tWild 2 ---" >> obs.txt
	./npy tWild 2 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tNested 0 ---" >> obs.txt
	./npy tNested 0 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tNested 3 ---" >> obs.txt
	./npy tNested 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tShadow 3 ---" >> obs.txt
	./npy tShadow 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tTwo 3 ---" >> obs.txt
	./npy tTwo 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tIO 4 ---" >> obs.txt
	./npy tIO 4 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tTry 3 ---" >> obs.txt
	./npy tTry 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tTryOk 3 ---" >> obs.txt
	./npy tTryOk 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt
	echo "--- py tLabel 3 ---" >> obs.txt
	./npy tLabel 3 >> obs.txt 2> label.err || echo failed >> obs.txt
	grep -c ' start' label.err >> obs.txt || echo 0 >> obs.txt
	echo "--- py tFrame 3 ---" >> obs.txt
	./npy tFrame 3 2> frame.err >> obs.txt || grep -c 'at failStep' frame.err >> obs.txt
	morloc make -o ncross main-cross.loc 2>> build.err
	echo "--- cross tCross 3 ---" >> obs.txt
	./ncross tCross 3 >> obs.txt 2>> obs.err || echo failed >> obs.txt

clean:
	rm -rf ncpp nrust npy ncross pools __pycache__
	rm -rf *-build *build.tmp*
	rm -f *.err
