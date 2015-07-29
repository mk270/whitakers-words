PROGRAMMES := bin/words bin/makedict bin/makestem bin/makeefil bin/makeewds bin/makeinfl bin/meanings

all: $(PROGRAMMES) data

$(PROGRAMMES):
	gprbuild -Pwords

DICTFILE.GEN: DICTLINE.GEN bin/makedict
	echo g | bin/makedict $< > /dev/null

EWDSFILE.GEN:
	bin/makeefil

EWDSLIST.GEN: DICTLINE.GEN bin/makeewds
	echo g | bin/makeewds $< > /dev/null

INFLECTS.SEC: INFLECTS.LAT bin/makeinfl
	bin/makeinfl $<

STEMFILE.GEN: STEMLIST.GEN bin/makestem
	echo g | bin/makestem $< > /dev/null

GENERATED_DATA_FILES := DICTFILE.GEN STEMFILE.GEN INDXFILE.GEN EWDSLIST.GEN \
					INFLECTS.SEC

data: $(GENERATED_DATA_FILES)

clean_data:
	rm -f -- $(GENERATED_DATA_FILES)

clean:
	rm -f -- obj/commands/* obj/latin_utils/* obj/support_utils/* bin/* lib/*
	rm -f -- CHECKEWD.
	rm -f -- DICTFILE.GEN STEMFILE.GEN INDXFILE.GEN EWDSLIST.GEN INFLECTS.SEC

.PHONY: test

test:
	(cd test; ./run-tests.sh)
