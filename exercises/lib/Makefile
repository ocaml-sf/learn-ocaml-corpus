# Utilities.

SHELL := bash
SED   := $(shell if command -v gsed >/dev/null ; then echo gsed ; else echo sed ; fi)

# Where to find learn-ocaml (TEMPORARY).

LEARN := $(HOME)/dev/learn-ocaml/_opam/bin/learn-ocaml

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
# Make sure that the proposed solution is (quickly) accepted.
	@ echo "Grading the proposed solution..."
	@ time -p for f in solution.ml ; do \
	    if $(LEARN) grade --dump-reports=$${f%.ml} >$${f%.ml}.log 2>&1 ; then \
	      echo " [OK]  $$f is correctly accepted." ; \
	    else \
	      echo "[FAIL] $$f is rejected!" ; \
	    fi ; \
	  done
# Make sure that a number of known incorrect solutions are rejected.
	@ echo "Grading known incorrect solutions..."
	@ for f in wrong/*.ml ; do \
	    rm -f $${f%.ml}.report.txt $${f%.ml}.report.html ; \
	    if $(LEARN) grade --grade-student=$$f --dump-reports=$${f%.ml} >$${f%.ml}.log 2>&1 ; then \
	      echo "[FAIL] $$f is incorrectly accepted!" ; \
	    else \
	      echo " [OK]  $$f is correctly rejected." ; \
	    fi ; \
	  done
# Use git status to display any differences with respect to the expected
# output and expected log (which should be checked in).
	@ git status -- $(wildcard *.report.txt *.log wrong/*.report.txt wrong/*.log)

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
