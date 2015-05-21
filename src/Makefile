LD =
LDFLAGS =

include ./make_setting

ifndef LD
LD = $(FC)
endif
ifndef LDFLAGS
LDFLAGS = $(FFLAGS)
endif

LES3X.MPI = ../bin/les3x.mpi
FFB_MINI = ../bin/ffb_mini

VERSION = 1.0.0

FFLAGS += -DFFB_MINI_VERSION=\"$(VERSION)\"

all: $(LES3X.MPI) $(FFB_MINI)

OBJS = \
  les3x.o  bcgs3x.o bcgsxe.o calax3.o          \
           callap.o caluel.o clrcrs.o \
  csin3x.o datcnv.o dgnscl.o e2plst.o e2pmtr.o \
           elm3dx.o          errchk.o          \
  fild3x.o fld3x2.o                            \
  grad3x.o          icalel.o          int3dx.o \
           lesrop.o lesrpx.o lessfx.o lumpex.o \
  match4.o matgau.o          mkcrs.o  neibr2.o \
  nodlex.o pres3e.o rcmelm.o          reordr.o \
  rfname.o sethex.o srfexx.o          subcnv.o \
           vel3d1.o vel3d2.o          \
  mfname.o \
  miniapp_util.o

ifeq (, $(findstring -DNO_METIS, $(FFLAGS)))
OBJS += metis_wrapper.o
endif

ifeq (, $(findstring -DNO_REFINER, $(FFLAGS)))
OBJS += lrfnms.o extrfn.o
endif

param.h: param.h.in
	./make_param_h.sh $< > $@

ifneq (, $(findstring -DPROF_MAPROF, $(FFLAGS)))

MAPROF_DIR = ma_prof/src
MAPROF_LIB = $(MAPROF_DIR)/libmaprof_f.a

FFLAGS += -I$(MAPROF_DIR)
LDFLAGS += -L$(MAPROF_DIR)
LIBS += -lmaprof_f

$(OBJS): $(MAPROF_LIB)

export
$(MAPROF_LIB):
	$(MAKE) -C $(MAPROF_DIR) f_mpi MAPROF_F="FC FFLAGS" MAX_SECTIONS=30

endif

LIB_GF2 = gf2/libgf2.a
LIB_DD_MPI = dd_mpi/libdd_mpi.a

$(LIB_GF2):
	$(MAKE) -C gf2

$(LIB_DD_MPI):
	$(MAKE) -C dd_mpi

$(LES3X.MPI): $(LIB_GF2) $(LIB_DD_MPI) $(OBJS) les3x_main.o
	$(LD) $(LDFLAGS) -o $@ $(OBJS) les3x_main.o $(LIB_GF2) $(LIB_DD_MPI) $(LIBS)

$(FFB_MINI): $(LIB_GF2) $(LIB_DD_MPI) $(OBJS) ffb_mini_main.o makemesh.o
	$(LD) $(LDFLAGS) -o $@ $(OBJS) ffb_mini_main.o makemesh.o $(LIB_GF2) $(LIB_DD_MPI) $(LIBS)

.SUFFIXES:
.SUFFIXES: .f .f90 .F .F90 .c .o

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<
.f90.o:
	$(FC) $(FFLAGS) -c $<
.F.o:
	$(FC) $(FFLAGS) -c $<
.F90.o:
	$(FC) $(FFLAGS) -c $<

clean:
	rm -rf *.o *.lst param.h
	$(MAKE) -C gf2 clean
	$(MAKE) -C dd_mpi clean
ifneq (, $(findstring -DPROF_MAPROF, $(FFLAGS)))
	$(MAKE) -C $(MAPROF_DIR) clean
endif

distclean: clean
	rm -rf ../bin/*
	$(MAKE) -C ../test clean

test: $(FFB_MINI)
	$(MAKE) -C ../test

test_k: $(FFB_MINI)
	$(MAKE) -C ../test test_k

test_fx10: $(FFB_MINI)
	$(MAKE) -C ../test test_fx10


ffb_mini_main.o: param.h
ffb_mini_main.o: makemesh.o

les3x.o: timing.h
vel3d1.o: timing.h
bcgs3x.o: timing.h
pres3e.o: timing.h
bcgs3x.o: timing.h
rcmelm.o: timing.h
bcgsxe.o: timing.h
callap.o: timing.h
calax3.o: timing.h
grad3x.o: timing.h
