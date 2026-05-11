all:
	rm -f *.err obs.txt
	rm -f /tmp/load-nexus-only-*.dat
	morloc make -o nexus main.loc 2> build.err
	./nexus --keep-null binInt   42 '"/tmp/load-nexus-only-bin-int.dat"'    >  obs.txt 2>  obs.err
	./nexus --keep-null msgInt   43 '"/tmp/load-nexus-only-msg-int.dat"'    >> obs.txt 2>> obs.err
	./nexus --keep-null jsonInt  44 '"/tmp/load-nexus-only-json-int.dat"'   >> obs.txt 2>> obs.err
	./nexus --keep-null binReal  3.14 '"/tmp/load-nexus-only-bin-real.dat"'   >> obs.txt 2>> obs.err
	./nexus --keep-null msgReal  2.71 '"/tmp/load-nexus-only-msg-real.dat"'   >> obs.txt 2>> obs.err
	./nexus --keep-null jsonReal 1.41 '"/tmp/load-nexus-only-json-real.dat"'  >> obs.txt 2>> obs.err
	./nexus --keep-null binStr   '"hello"' '"/tmp/load-nexus-only-bin-str.dat"'   >> obs.txt 2>> obs.err
	./nexus --keep-null msgStr   '"world"' '"/tmp/load-nexus-only-msg-str.dat"'   >> obs.txt 2>> obs.err
	./nexus --keep-null jsonStr  '"morloc"' '"/tmp/load-nexus-only-json-str.dat"' >> obs.txt 2>> obs.err
	./nexus --keep-null binList  '["a","b","c"]' '"/tmp/load-nexus-only-bin-list.dat"'   >> obs.txt 2>> obs.err
	./nexus --keep-null msgList  '["d","e","f"]' '"/tmp/load-nexus-only-msg-list.dat"'   >> obs.txt 2>> obs.err
	./nexus --keep-null jsonList '["g","h","i"]' '"/tmp/load-nexus-only-json-list.dat"'  >> obs.txt 2>> obs.err
	./nexus --keep-null binTup   '[1,"alpha"]' '"/tmp/load-nexus-only-bin-tup.dat"'   >> obs.txt 2>> obs.err
	./nexus --keep-null msgTup   '[2,"beta"]'  '"/tmp/load-nexus-only-msg-tup.dat"'   >> obs.txt 2>> obs.err
	./nexus --keep-null jsonTup  '[3,"gamma"]' '"/tmp/load-nexus-only-json-tup.dat"'  >> obs.txt 2>> obs.err
	./nexus --keep-null binFixedTupList  '[[1,1.5],[2,2.5]]' '"/tmp/load-nexus-only-bin-ftl.dat"'  >> obs.txt 2>> obs.err
	./nexus --keep-null msgFixedTupList  '[[3,3.5],[4,4.5]]' '"/tmp/load-nexus-only-msg-ftl.dat"'  >> obs.txt 2>> obs.err
	./nexus --keep-null jsonFixedTupList '[[5,5.5],[6,6.5]]' '"/tmp/load-nexus-only-json-ftl.dat"' >> obs.txt 2>> obs.err
	./nexus --keep-null loadMissing '"/tmp/load-nexus-only-does-not-exist.dat"' >> obs.txt 2>> obs.err
	rm -f /tmp/load-nexus-only-*.dat

clean:
	rm -rf nexus pools __pycache__ *.err
	rm -f /tmp/load-nexus-only-*.dat
