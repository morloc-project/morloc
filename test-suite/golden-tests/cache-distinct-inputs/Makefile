# Distinct inputs must produce distinct cache entries -- per pool
# language. Four inputs * three languages = 12 keys total. Reruns
# must return the originally-stored value per input (not whichever
# was stored last), guarding against a hash function that would
# silently collapse arguments onto a constant key.

INPUT_A := '[1.0,2.0]'
INPUT_B := '[1.0,2.0,3.0]'
INPUT_C := '[10.0,20.0]'
INPUT_D := '[0.5,0.5,0.5,0.5]'

define RUN
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo$(1) $(INPUT_A) >> obs.txt 2>> obs.run.$(1).err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo$(1) $(INPUT_B) >> obs.txt 2>> obs.run.$(1).err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo$(1) $(INPUT_C) >> obs.txt 2>> obs.run.$(1).err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo$(1) $(INPUT_D) >> obs.txt 2>> obs.run.$(1).err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo$(1) $(INPUT_A) >> obs.txt 2>> obs.run.$(1).err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo$(1) $(INPUT_B) >> obs.txt 2>> obs.run.$(1).err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo$(1) $(INPUT_C) >> obs.txt 2>> obs.run.$(1).err
	MORLOC_CACHE_BASE=$$PWD/cache ./nexus foo$(1) $(INPUT_D) >> obs.txt 2>> obs.run.$(1).err

endef

all:
	rm -f *.err obs.txt
	rm -rf cache
	mkdir cache
	morloc make -o nexus main.loc 2> build.err
	$(call RUN,P)
	$(call RUN,R)
	$(call RUN,C)
	echo "a_packets: $$(ls cache/a/*.dat 2>/dev/null | wc -l)" >> obs.txt
	echo "dats:      $$(ls cache/data/*.dat 2>/dev/null | wc -l)" >> obs.txt
	cat obs.run.*.err > obs.err

clean:
	rm -rf nexus pools cache *.err
