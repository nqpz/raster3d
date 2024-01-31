PROGNAME=demo
PROG_FUT_DEPS:=$(shell ls *.fut; find -name \*.fut)
include lib/github.com/diku-dk/lys/common.mk
