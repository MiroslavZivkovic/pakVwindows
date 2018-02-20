MUMPS = $(HOME)/pak/libs-mcm-large/MUMPS_5.0.0
include $(MUMPS)/Makefile.inc

CC=gcc
CPP=g++
FC=gfortran

MPICH = $(HOME)/pak/libs-mcm-large/mpich-3.0.4/lib/.libs
MPICHINC = $(MPICH)/../../src/include
MUMPSINC = $(MUMPS)/include

FLAGS= -fPIC -O2 -finit-local-zero
INC=-I./src -I$(MUMPSINC) -I$(MPICHINC)
FLIB = $(FLIBPAK) $(MPICH)/libfmpich.a $(MPICH)/libmpich.a $(MPICH)/libmpl.a -L/usr/lib64/ -lpthread
FLIBPAK = $(MUMPS)/lib/libdmumps.a $(MUMPS)/lib/libmumps_common.a $(SCALAP) $(HOME)/pak/libs-mcm-large/BLAS/blas_LINUX.a -L/usr/lib64/ -lpthread $(MUMPS)/lib/libpord.a

SRC=src
ODIRF=obj
ODIRF90=obj
ODIRC=obj
ODIRCPP=obj

_OBJF90= mod.o
OBJF90 = $(patsubst %,$(ODIRF90)/%,$(_OBJF90))

_OBJF:= $(patsubst %.for,%.o,$(subst src/,,$(wildcard src/*.for)))
OBJF = $(patsubst %,$(ODIRF)/%,$(_OBJF))

_OBJC= $(patsubst %.c,%.o,$(subst src/,,$(wildcard src/*.c)))
OBJC = $(patsubst %,$(ODIRC)/%,$(_OBJC))

_OBJCPP= $(patsubst %.cpp,%.o,$(subst src/,,$(wildcard src/*.cpp)))
OBJCPP = $(patsubst %,$(ODIRCPP)/%,$(_OBJCPP))

all: PakV

PakV: $(OBJF90) $(OBJF) $(OBJC) $(OBJCPP) makefile
	$(FC)  $(FLAGS) -static $(OBJF90) $(OBJF) $(OBJC) $(OBJCPP) $(FLIB) -lstdc++ -o pakv
	
$(ODIRF90)/%.o: $(SRC)/%.f90
	$(FC) $(FLAGS) $(INC) -c -o $@ $<
	
$(ODIRF)/%.o: $(SRC)/%.for
	$(FC) $(FLAGS) $(INC) -c -o $@ $<
	
$(ODIRC)/%.o: $(SRC)/%.c
	$(CC) $(CFLAGS) $(INC) -c -o $@ $<
	
$(ODIRCPP)/%.o: $(SRC)/%.cpp
	$(CPP) $(CFLAGS) $(INC) -c -o $@ $<
	
clean:
	rm obj/*.o
	rm *.mod
	rm pakv

# DO NOT DELETE THIS LINE -- make depend depends on it.
