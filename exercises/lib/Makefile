# Utilities.

SHELL := bash
SED   := $(shell if command -v gsed >/dev/null ; then echo gsed ; else echo sed ; fi)

# Where to find learn-ocaml (TEMPORARY).

LEARN := $(HOME)/dev/learn-ocaml/_opam/bin/learn-ocaml
export LEARN

# ------------------------------------------------------------------------------

# General organisation:

# descr.md and meta.json contain the exercise description.

# master.ml contains INCLUDE/EXCLUDE directives. By preprocessing it,
# we create the files template.ml and solution.ml.

# wrong/ contains a number of incorrect solutions (foo.ml) as well as
# the reports that we expect (foo.report.txt).

# ------------------------------------------------------------------------------

.PHONY: all
all: template.ml solution.ml
	cd ../.. && $(LEARN) build --repo=`pwd` -v

template.ml: master.ml
	$(SED) -f $(LIB)/sanitize.template $< > $@

solution.ml: master.ml
	$(SED) -f $(LIB)/sanitize.solution $< > $@

# ------------------------------------------------------------------------------

.PHONY: test
test: all
	$(LIB)/test.sh

# ------------------------------------------------------------------------------

.PHONY: clean
clean:
	rm -f *.report.html wrong/*.report.html
	rm -f *~

# ------------------------------------------------------------------------------

.PHONY: lint
lint:
	mdl descr.md
	jsonlint meta.json -q

.PHONY: view
view:
	open -a "/Applications/Visual Studio Code.app" descr.md
