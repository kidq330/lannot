##########################################################################
#                                                                        #
#  This file is part of the Frama-C's Lannotate plug-in.                 #
#                                                                        #
#  Copyright (C) 2012-2022                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  you can redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful,                 #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of        #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         #
#  GNU Lesser General Public License for more details.                   #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1                 #
#  for more details (enclosed in the file LICENSE)                       #
#                                                                        #
##########################################################################

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

HDRCK_EXTRA:=-headache-config-file $(FRAMAC_SHARE)/headache_config.txt
include ${FRAMAC_SHARE}/Makefile.headers

##########################################################################
