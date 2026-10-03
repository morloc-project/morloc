all:
	rm -f *.err obs.txt tick.log
	morloc make -o nexus main.loc 2> build.err
	rm -f tick.log; ./nexus oneArm 0 > obs.txt 2> obs.err
	rm -f tick.log; ./nexus oneArm 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus bothArms 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus inCondition 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus conditionAndNestedArm 9 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus nestedArms 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus nestedArms 9 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus underLambda 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus underForeignLambda 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus inRecursiveHelper 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus inRecursiveHelperForeignUse 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus throughBinding 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus onlyThroughUnused 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus cafTwoFunctions 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus cafUnderLambda 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus cafOneArm 0 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus cafOneArm 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus letUnused 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus letOneArm 0 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus oneValue 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus suspendedTwice 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus cafBarrier 1 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus nestedHelpers 2 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus recShare 3 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus wholeWhere >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus closedLetTwice 3 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus closedWhereTwice 3 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus closedLetRecursive 3 >> obs.txt 2>> obs.err
	rm -f tick.log; ./nexus closedLetUnused 3 >> obs.txt 2>> obs.err

clean:
	rm -rf nexus __pycache__ *.err tick.log
	rm -rf *-build *build.tmp*
