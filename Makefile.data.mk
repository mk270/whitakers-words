# Helper for Makefile

GENERATED_DATA_FILES := \
  DICTFILE.GEN \
  EWDSFILE.GEN \
  EWDSLIST.GEN \
  INDXFILE.GEN \
  INFLECTS.SEC \
  STEMFILE.GEN \
  STEMLIST.GEN

.PHONY: all
all: $(GENERATED_DATA_FILES)

DICTFILE.GEN: bin/wakedict DICTLINE.GEN
	echo g | $^
	mv STEMLIST.GEN STEMLIST_generated.GEN

STEMLIST.GEN: bin/sorter DICTLINE.GEN
	rm -f -- $@
	$< < stemlist-sort.txt
	mv -f -- STEMLIST_new.GEN $@
	rm -f STEMLIST_generated.GEN
	rm -f WORK.

EWDSFILE.GEN: bin/makeefil EWDSLIST.GEN
	$<

EWDSLIST.GEN: bin/makeewds DICTLINE.GEN
	echo g | $^
	LC_COLLATE=C sort -o $@ $@

INFLECTS.SEC: bin/makeinfl INFLECTS.LAT
	$^

STEMFILE.GEN INDXFILE.GEN: bin/makestem STEMLIST.GEN
	echo g | $^

.PHONY: clean
clean:
	rm -f $(GENERATED_DATA_FILES) CHECKEWD. WORK. STEMLIST_generated.GEN STEMLIST_new.GEN

# This Makefile does not support parallelism.
.NOTPARALLEL:
