all:
	rm -f *.err obs.txt
	morloc make -o nexus main.loc 2> build.err
	./nexus deepCity    '{"name":"c","address":{"city":"NYC","zip":"10001"}}' > obs.txt 2> obs.err
	./nexus deepCityLet '{"name":"c","address":{"city":"NYC","zip":"10001"}}' >> obs.txt 2>> obs.err
	./nexus deepZip     '{"name":"c","address":{"city":"NYC","zip":"10001"}}' >> obs.txt 2>> obs.err
	./nexus triple      '{"ofirst":0,"middle":{"mfirst":0,"inner":{"ifirst":0,"ival":42}}}' >> obs.txt 2>> obs.err
	./nexus tripleLet   '{"ofirst":0,"middle":{"mfirst":0,"inner":{"ifirst":0,"ival":42}}}' >> obs.txt 2>> obs.err
	./nexus combo       '{"name":"c","address":{"city":"NYC","zip":"10001"}}' '{"ofirst":0,"middle":{"mfirst":0,"inner":{"ifirst":0,"ival":42}}}' >> obs.txt 2>> obs.err

clean:
	rm -rf nexus pools __pycache__ *.err
