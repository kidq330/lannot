##########################################################################
#                                                                        #
#  This file is part of Frama-C.                                         #
#                                                                        #
#  Copyright (C) 2007-2018                                               #
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
#  for more details (enclosed in the file licenses/LGPLv2.1).            #
#                                                                        #
##########################################################################

# Do not use ?= to initialize both below variables
# (fixed efficiency issue, see GNU Make manual, Section 8.11)
ifndef FRAMAC_SHARE
FRAMAC_SHARE  :=$(shell frama-c-config -print-share-path)
endif
ifndef FRAMAC_LIBDIR
FRAMAC_LIBDIR :=$(shell frama-c-config -print-libpath)
endif

###################
# Plug-in Setting #
###################

PLUGIN_DIR ?=.
PLUGIN_ENABLE:=yes
PLUGIN_NAME:=LAnnotate
PLUGIN_CMO:= options utils ast_const bes simplify annotators wm \
             logical partition ldataflow context function statement lloop \
             register
PLUGIN_DISTRIBUTED:=$(PLUGIN_ENABLE)
PLUGIN_DISTRIB_EXTERNAL:= Makefile.in configure.ac configure
PLUGIN_TESTS_DIRS:=options criteria

################
# Generic part #
################

include $(FRAMAC_SHARE)/Makefile.dynamic

#####################################
# Regenerating the Makefile on need #
#####################################

ifeq ("$(FRAMAC_INTERNAL)","yes")
CONFIG_STATUS_DIR=$(FRAMAC_SRC)
else
CONFIG_STATUS_DIR=.
endif
$(LAnnotate_DIR)/Makefile: $(LAnnotate_DIR)/Makefile.in \
                         $(CONFIG_STATUS_DIR)/config.status
	cd $(CONFIG_STATUS_DIR) && ./config.status --file $@
