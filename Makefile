BUILD := gprbuild

PROGRAMMES := bin/words bin/makedict bin/wakedict bin/makestem bin/makeefil bin/makeewds bin/makeinfl bin/meanings

.PHONY: all

all: $(PROGRAMMES) data

$(PROGRAMMES):
	$(BUILD) -p -j4 -Pwords $(notdir $@)

bin/sorter:
	$(BUILD) -p -j4 -Ptools $(notdir $@)

DICTFILE.GEN: DICTLINE.GEN bin/wakedict
	echo g | bin/wakedict $< > /dev/null
	mv STEMLIST.GEN STEMLIST_generated.GEN

STEMLIST.GEN: DICTLINE.GEN bin/sorter
	rm -f -- $@
	bin/sorter < stemlist-sort.txt > /dev/null
	mv -f -- STEMLIST_new.GEN $@
	rm -f STEMLIST_generated.GEN
	rm -f WORK.

EWDSFILE.GEN: EWDSLIST.GEN bin/makeefil
	bin/makeefil

EWDSLIST.GEN: DICTLINE.GEN bin/makeewds
	echo g | bin/makeewds $< > /dev/null
	sort -o $@ $@

INFLECTS.SEC: INFLECTS.LAT bin/makeinfl
	bin/makeinfl $< > /dev/null

STEMFILE.GEN: STEMLIST.GEN bin/makestem
	echo g | bin/makestem $< > /dev/null

GENERATED_DATA_FILES := DICTFILE.GEN STEMFILE.GEN INDXFILE.GEN EWDSLIST.GEN \
					INFLECTS.SEC EWDSFILE.GEN STEMLIST.GEN

.PHONY: data

data: $(GENERATED_DATA_FILES)

.PHONY: clean_data

clean_data:
	rm -f $(GENERATED_DATA_FILES) CHECKEWD.

.PHONY: clean

clean: clean_data
	rm -fr bin lib obj
	rm -f WORK. STEMLIST_generated.GEN STEMLIST_new.GEN

.PHONY: test

test: $(PROGRAMMES) $(GENERATED_DATA_FILES)
	cd test && ./run-tests.sh
