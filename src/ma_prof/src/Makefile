# Copyright (C) 2014-2015 RIKEN AICS
# This library is released under the terms of the MIT license.
# http://fiber-miniapp.mit-license.org/2014-2015


ifdef MAPROF_CXX
ifndef MAPROF_C
CC = $($(word 1,$(MAPROF_CXX)))
CFLAGS = $($(word 2,$(MAPROF_CXX)))
endif
endif

C_OBJS = maprof.o maprof_time.o maprof_yaml.o maprof_proc.o
F_OBJS = mod_maprof.o

all: c

c_mpi cxx_mpi f_mpi: CFLAGS += -DUSE_MPI
f_mpi: FFLAGS += -DUSE_MPI

c c_mpi cxx cxx_mpi: libmaprof.a
f f_mpi: libmaprof_f.a

AR ?= ar
ARFLAGS ?= rv
RANLIB ?= ranlib

config.h:
	./make_config.sh

libmaprof.a: $(C_OBJS)
	$(AR) $(ARFLAGS) $@ $(C_OBJS)
	$(RANLIB) $@

libmaprof_f.a: $(C_OBJS) $(F_OBJS)
	$(AR) $(ARFLAGS) $@ $(C_OBJS) $(F_OBJS)
	$(RANLIB) $@

a.out: main.o libmaprof.a
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@

proc_test: proc_test.o maprof_proc.o
	$(CC) $(CFLAGS) $(LDFLAGS) $^ -o $@

.SUFFIXES:
.SUFFIXES: .F90 .c .o .mod

.c.o:
	$(CC) $(CFLAGS) -c $<

.F90.o:
	$(FC) $(FFLAGS) -c $<

clean:
	rm -f config.h *.o *.mod *.i90 *.a proc_test


maprof.o maprof_proc.o: config.h
maprof_yaml.o: maprof_yaml.h
maprof_proc.o: maprof_proc.h
maprof.o: maprof_yaml.h maprof_proc.h
maprof.o: maprof.h
maprof_time.o: maprof.h
main.o: maprof.h
proc_test.o: maprof_proc.h

