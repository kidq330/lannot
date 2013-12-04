
FRAMAC_SHARE	:=$(shell frama-c.byte -print-path)
FRAMAC_LIBDIR	:=$(shell frama-c.byte -print-libpath)
PLUGIN_NAME	= Genlabels


PLUGIN_TESTS_DIRS:=bubblesort

PLUGIN_CMO	= options config utils verdict instru slicing register
include $(FRAMAC_SHARE)/Makefile.dynamic

 
