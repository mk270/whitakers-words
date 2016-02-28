PROGRAMMES := bin/words bin/makedict bin/makestem bin/makeefil bin/makeewds bin/makeinfl bin/meanings

all: $(PROGRAMMES) data

$(PROGRAMMES):
	gprbuild -Pwords $(notdir $@)

DICTFILE.GEN: DICTLINE.GEN bin/makedict
	echo g | bin/makedict $< > /dev/null

EWDSFILE.GEN: EWDSLIST.GEN
	bin/makeefil

EWDSLIST.GEN: DICTLINE.GEN bin/makeewds
	echo g | bin/makeewds $< > /dev/null

INFLECTS.SEC: INFLECTS.LAT bin/makeinfl
	bin/makeinfl $< > /dev/null

STEMFILE.GEN: STEMLIST.GEN bin/makestem
	echo g | bin/makestem $< > /dev/null

GENERATED_DATA_FILES := DICTFILE.GEN STEMFILE.GEN INDXFILE.GEN EWDSLIST.GEN \
					INFLECTS.SEC

data: $(GENERATED_DATA_FILES)

clean_data:
	rm -f -- $(GENERATED_DATA_FILES)

clean:
	gprclean -q -r -Pwords
	rm -f -- CHECKEWD.
	rm -f -- DICTFILE.GEN STEMFILE.GEN INDXFILE.GEN EWDSLIST.GEN INFLECTS.SEC

.PHONY: test $(PROGRAMMES)

test:
	(cd test; ./run-tests.sh)
