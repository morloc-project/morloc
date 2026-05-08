all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	echo "--- fusedCppChain ---" > obs.txt
	./nexus fusedCppChain >> obs.txt 2>> obs.err
	echo "--- effectOrderCpp ---" >> obs.txt
	./nexus effectOrderCpp >> obs.txt 2>> obs.err
	echo "--- effectOrderPolyglot ---" >> obs.txt
	./nexus effectOrderPolyglot >> obs.txt 2>> obs.err
	echo "--- doBlockBareThenBind ---" >> obs.txt
	./nexus doBlockBareThenBind >> obs.txt 2>> obs.err
	echo "--- diamondReuse ---" >> obs.txt
	./nexus diamondReuse >> obs.txt 2>> obs.err
	echo "--- doInLet ---" >> obs.txt
	./nexus doInLet >> obs.txt 2>> obs.err
	echo "--- letInDo ---" >> obs.txt
	./nexus letInDo >> obs.txt 2>> obs.err
	echo "--- doInDoForced ---" >> obs.txt
	./nexus doInDoForced >> obs.txt 2>> obs.err
	echo "--- doInDoLet ---" >> obs.txt
	./nexus doInDoLet >> obs.txt 2>> obs.err
	echo "--- letBndTailEffect ---" >> obs.txt
	./nexus letBndTailEffect >> obs.txt 2>> obs.err
	echo "--- constTailWithEffect ---" >> obs.txt
	./nexus constTailWithEffect >> obs.txt 2>> obs.err
	echo "--- deepLetChain ---" >> obs.txt
	./nexus deepLetChain >> obs.txt 2>> obs.err
	echo "--- factWithLetChain ---" >> obs.txt
	./nexus factWithLetChain 5 >> obs.txt 2>> obs.err
	echo "--- forcedThrice ---" >> obs.txt
	./nexus forcedThrice >> obs.txt 2>> obs.err
	echo "--- mixedSameLangFanOut ---" >> obs.txt
	./nexus mixedSameLangFanOut >> obs.txt 2>> obs.err
	echo "--- doOnlyBareStatements ---" >> obs.txt
	./nexus doOnlyBareStatements >> obs.txt 2>> obs.err
	echo "--- shadowedBinding ---" >> obs.txt
	./nexus shadowedBinding >> obs.txt 2>> obs.err
	echo "--- crossLangIntoSameLangFinish ---" >> obs.txt
	./nexus crossLangIntoSameLangFinish >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools log __pycache__
