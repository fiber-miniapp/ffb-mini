CC = mpifccpx
FC = mpifrtpx

#DEFINE += -DNO_METIS
DEFINE += -DNO_REFINER

# timing option
DEFINE += -DPROF_MAPROF

# timing option (fapp on K/FX10)
#DEFINE += -DPROF_FAPP

CFLAGS += $(DEFINE) -Kvisimpact,ocl
FFLAGS += $(DEFINE) -Kvisimpact,ocl -Qt


ifeq (, $(findstring -DNO_METIS, $(FFLAGS)))
CFLAGS += -I/opt/klocal/include
LIBS += -L/opt/klocal/lib -lmetis
endif

ifeq (, $(findstring -DNO_REFINER, $(FFLAGS)))
REFINER = $(HOME)/opt/REVOCAP_Refiner
FLAGS += -I$(REFINER)/include
LIBS += -L$(REFINER)/lib -lRcapRefiner
LIBS += -lRcapRefiner
LD = mpiFCCpx
LDFLAGS = --linkfortran -Kvisimpact
endif

