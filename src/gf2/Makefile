include ../make_setting

AR ?= ar
ARFLAGS ?= rv
RANLIB ?= ranlib

all: libgf2.a

OBJS = gfall.o gfrd_c.o gfwrt_c.o gfutil_c.o gfutil.o

libgf2.a: $(OBJS)
	$(AR) $(ARFLAGS) $@ $(OBJS)
	$(RANLIB) $@

.SUFFIXES:
.SUFFIXES: .f .c .o

.c.o:
	$(CC) $(CFLAGS) -c $<
.f.o:
	$(FC) $(FFLAGS) -c $<

clean:
	rm -rf *.o *.a
