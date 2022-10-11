.PHONY: all build clean

ifndef FRAMAC_SHARE
FRAMAC_SHARE :=$(shell frama-c-config -print-share-path)
endif

sinclude ${FRAMAC_SHARE}/Makefile.common

##########################################################################
# Build

all:: build frama-c-lannotate.opam

frama-c-lannotate.opam: frama-c-lannotate.opam.template dune-project
	rm -f $@
	dune build $@

build::
	dune build @install

clean:: purge-tests
	dune clean
	rm -rf _build .merlin

##########################################################################
# Tests

# PTEST_OPTS has to be defined when Frama-Clang plugin is external
# in order to redefine the macro @FRAMAC_SHARE@ for a FILTER directive
PTEST_OPTS?= -macro-frama-c-share=$(FRAMAC_SHARE)

include ${FRAMAC_SHARE}/Makefile.testing

##########################################################################
# Install

include ${FRAMAC_SHARE}/Makefile.installation

##########################################################################
# Headers

HDRCK_EXTRA:=-headache-config-file ./headers/headache_config.txt
include ${FRAMAC_SHARE}/Makefile.headers

##########################################################################
