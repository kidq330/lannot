##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2013-2014                                               #
#    CEA (Commissariat à l'énergie atomique et aux énergies              #
#         alternatives)                                                  #
#                                                                        #
#  You may redistribute it and/or modify it under the terms of the GNU   #
#  Lesser General Public License as published by the Free Software       #
#  Foundation, version 2.1.                                              #
#                                                                        #
#  It is distributed in the hope that it will be useful, but WITHOUT     #
#  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY    #
#  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General      #
#  Public License for more details.                                      #
#                                                                        #
#  See the GNU Lesser General Public License version 2.1 for more        #
#  details (enclosed in the file LICENSE).                               #
#                                                                        #
##########################################################################

FRAMAC_SHARE	:=$(shell frama-c.byte -print-path)
FRAMAC_LIBDIR	:=$(shell frama-c.byte -print-libpath)
PLUGIN_NAME	= LAnnotate


PLUGIN_TESTS_DIRS:=bubblesort

PLUGIN_CMO	= options config utils annotators instru condition decision function register
include $(FRAMAC_SHARE)/Makefile.dynamic
