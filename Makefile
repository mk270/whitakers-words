# Makefile for Whitaker's words

GPRBUILD                                := gprbuild
GPRBUILD_OPTIONS                        := -j4

# Build flags are commonly found in the environment, but if they are
# set on our Make command line, forward them to GNAT projects.
ADAFLAGS                                ?=
LDFLAGS                                 ?=

# For the library, a static archive is built by default but a
# non-empty shared object version selects a relocatable library
latin_utils_soversion                   :=

# Directory where dictionnary files are created and searched for.
# This variable is expected to be overridden at build time, with some
# architecture-specific value like $(prefix)/share/whitakers-words).
# At run time, another directory can be selected via the
# WHITAKERS_WORDS_DATADIR environment variable.
datadir                                 := .

generated_sources := src/latin_utils/latin_utils-config.adb

# If a relocatable library has been selected, tell the dynamic loader
# where to find it during tests or generation of the dictionaries.
ifneq (,$(latin_utils_soversion))
  maybe_lib_path := \
    LD_LIBRARY_PATH=$(if $(LD_LIBRARY_PATH),'$(LD_LIBRARY_PATH)':)lib/latin_utils-dynamic \
    $()
endif

# It is convenient to let gprbuild deal with Ada dependencies, and
# only then let a submake decide which data must be refreshed
# according to the now up-to-date timestamps of the generators.
.PHONY: all
all: commands
	$(maybe_lib_path)WHITAKERS_WORDS_DATADIR=. $(MAKE) -f Makefile.data.mk

# This target is more efficient than separate gprbuild runs because
# the dependency graph is only constructed once.
# It is run either because by an explicit 'commands' target, or
# because a generated file requires it.
.PHONY: commands
commands: $(generated_sources)
	$(GPRBUILD) -p $(GPRBUILD_OPTIONS) commands.gpr \
	  $(foreach v,ADAFLAGS LDFLAGS latin_utils_soversion,-X$(v)='$($(v))')

.PHONY: clean
clean:
	$(MAKE) -f Makefile.data.mk $@
	rm -fr bin lib obj
	rm -f $(generated_sources)

$(generated_sources): %: %.in Makefile
	sed 's|@datadir@|$(datadir)|' $< > $@

.PHONY: test
test: all
	$(maybe_lib_path)test/run-tests.sh
