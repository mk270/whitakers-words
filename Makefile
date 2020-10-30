# Makefile for Whitaker's words

GPRBUILD                                := gprbuild
GPRBUILD_OPTIONS                        := -j4

# Build flags are commonly found in the environment, but if they are
# set on our Make command line, forward them to GNAT projects.
export ADAFLAGS                         ?=
export LDFLAGS                          ?=

# For each library, a static archive is built by default but a
# non-empty shared object version selects a relocatable library
export latin_utils_soversion            :=
export support_utils_soversion          :=
export words_engine_soversion           :=

# Directory where dictionnary files are created and searched for.
# This variable is expected to be overridden at build time, with some
# architecture-specific value like $(prefix)/share/whitakers-words).
# At run time, another directory can be selected via the
# WHITAKERS_WORDS_DATADIR environment variable.
datadir                                 := .
# During (re)builds, the tools must read and test the fresh data and
# ignore any previous version already installed in $(datadir).
export WHITAKERS_WORDS_DATADIR := .

generated_sources := src/latin_utils/latin_utils-config.adb

PROGRAMMES := makedict makeefil makeewds makeinfl makestem meanings wakedict \
  words

.PHONY: all

all: commands data

# This target is more efficient than separate gprbuild runs because
# the dependency graph is only constructed once.
.PHONY: commands
commands: $(generated_sources)
	$(GPRBUILD) -p $(GPRBUILD_OPTIONS) commands.gpr

# Targets delegated to gprbuild are declared phony even if they build
# concrete files, because Make ignores all about Ada dependencies.
.PHONY: $(PROGRAMMES)
$(PROGRAMMES): $(generated_sources)
	$(GPRBUILD) -p $(GPRBUILD_OPTIONS) commands.gpr $@

.PHONY: sorter
sorter: $(generated_sources)
	$(GPRBUILD) -p $(GPRBUILD_OPTIONS) tools.gpr $@

# Executable targets are phony (see above), so we tell Make to only
# check that they exist but ignore the timestamp.  This is not
# perfect, but at least Make
# * updates the output data if the input data changes
# * builds the generator if it does not exist yet

DICTFILE.GEN: DICTLINE.GEN | wakedict
	echo g | bin/wakedict $< > /dev/null
	mv STEMLIST.GEN STEMLIST_generated.GEN

STEMLIST.GEN: DICTLINE.GEN | sorter
	rm -f -- $@
	bin/sorter < stemlist-sort.txt > /dev/null
	mv -f -- STEMLIST_new.GEN $@
	rm -f STEMLIST_generated.GEN
	rm -f WORK.

EWDSFILE.GEN: EWDSLIST.GEN | makeefil
	bin/makeefil

EWDSLIST.GEN: DICTLINE.GEN | makeewds
	echo g | bin/makeewds $< > /dev/null
	LC_COLLATE=C sort -o $@ $@

INFLECTS.SEC: INFLECTS.LAT | makeinfl
	bin/makeinfl $< > /dev/null

STEMFILE.GEN INDXFILE.GEN: STEMLIST.GEN | makestem
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
	rm -f WORK. STEMLIST_generated.GEN STEMLIST_new.GEN $(generated_sources)

$(generated_sources): %: %.in Makefile
	sed 's|@datadir@|$(datadir)|' $< > $@

.PHONY: test

test: all
	cd test && ./run-tests.sh

# This Makefile does not support parallelism (but gprbuild does).
.NOTPARALLEL:
