##include ../src/make_setting

#FC = gfortran
#FFLAGS = -g -Wall -fconvert=big-endian -frecord-marker=4
#FFLAGS = -g -Wall
FC = mpif90

BIN_DIR = ../bin

# which wall is moving (default: MOVE_Y_TOP)
#FFLAGS += -DMOVE_X_TOP
#FFLAGS += -DMOVE_Z_TOP

LDFLAGS=
LIBS=

all: makemesh_para makemesh_all

.SUFFIXES:
.SUFFIXES: .o .f90 .F90

.f90.o:
	$(FC) $(FFLAGS) -c $<
.F90.o:
	$(FC) $(FFLAGS) -c $<


makemesh_para: makemesh_para.o makemesh.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(BIN_DIR)/$@ makemesh_para.o makemesh.o $(LIBS)

makemesh_all: makemesh_all.o makemesh.o
	$(FC) $(FFLAGS) $(LDFLAGS) -o $(BIN_DIR)/$@ makemesh_all.o makemesh.o $(LIBS)

clean:
	rm -f *.o *.mod *.i90

distclean: clean
	rm -f $(BIN_DIR)/makemesh_para $(BIN_DIR)/makemesh_all

makemesh_para.o: makemesh.o
makemesh_all.o: makemesh.o
