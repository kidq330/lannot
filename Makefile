FRAMAC_SHARE	:=$(shell frama-c.byte -print-path)
FRAMAC_LIBDIR	:=$(shell frama-c.byte -print-libpath)
PLUGIN_NAME	= LAnnotate


PLUGIN_TESTS_DIRS:=bubblesort

PLUGIN_CMO	= options config utils annotators instru decision function register
include $(FRAMAC_SHARE)/Makefile.dynamic
