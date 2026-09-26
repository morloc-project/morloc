all:
	rm -f *.err obs.txt
	-morloc make -o topConstant topConstant.loc 2>> build.err
	-echo -n 'topConstant ' >> obs.txt; ./topConstant foo 5 >> obs.txt 2>> obs.err
	-morloc make -o whereBinding whereBinding.loc 2>> build.err
	-echo -n 'whereBinding ' >> obs.txt; ./whereBinding foo 5 >> obs.txt 2>> obs.err
	-morloc make -o letBinding letBinding.loc 2>> build.err
	-echo -n 'letBinding ' >> obs.txt; ./letBinding foo 5 >> obs.txt 2>> obs.err
	-morloc make -o pointFree pointFree.loc 2>> build.err
	-echo -n 'pointFree ' >> obs.txt; ./pointFree foo 5 >> obs.txt 2>> obs.err
	-morloc make -o shapes shapes.loc 2>> build.err
	-echo -n 'getter ' >> obs.txt; ./shapes getter 5 >> obs.txt 2>> obs.err
	-echo -n 'tuple ' >> obs.txt; ./shapes tuple 5 >> obs.txt 2>> obs.err
	-echo -n 'listed ' >> obs.txt; ./shapes listed 5 >> obs.txt 2>> obs.err
	-echo -n 'conditional ' >> obs.txt; ./shapes conditional 5 >> obs.txt 2>> obs.err
	-echo -n 'shown ' >> obs.txt; ./shapes shown 5 >> obs.txt 2>> obs.err
	-echo -n 'generic ' >> obs.txt; ./shapes generic 5 >> obs.txt 2>> obs.err
	-morloc make -o mirror mirror.loc 2>> build.err
	-echo -n 'sourced ' >> obs.txt; ./mirror sourced 5 >> obs.txt 2>> obs.err
	-echo -n 'updated ' >> obs.txt; ./mirror updated 5 >> obs.txt 2>> obs.err
	-morloc make -o mirror-function mirror-function.loc 2>> build.err
	-echo -n 'function ' >> obs.txt; ./mirror-function function 5 >> obs.txt 2>> obs.err
	-echo -n 'rustFunction ' >> obs.txt; ./mirror-function rustFunction 5 >> obs.txt 2>> obs.err
	-morloc make -o mirror-crossing mirror-crossing.loc 2>> build.err
	-echo -n 'crossed ' >> obs.txt; ./mirror-crossing crossed 5 >> obs.txt 2>> obs.err
	-echo -n 'rustCaptured ' >> obs.txt; ./mirror-crossing rustCaptured 5 >> obs.txt 2>> obs.err
	-morloc make -o dataField dataField.loc 2>> build.err
	-echo -n 'dataField ' >> obs.txt; ./dataField foo 5 >> obs.txt 2>> obs.err

clean:
	rm -rf dataField mirror-function mirror-crossing mirror shapes topConstant whereBinding letBinding pointFree __pycache__ *.err
	rm -rf *-build *build.tmp*
