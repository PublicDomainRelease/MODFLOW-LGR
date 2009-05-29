# Makefile example for compiling MODFLOW-lgr:

# Makefile for non-USGS distribution
#
PROGRAM = mflgr

# Define the Fortran compile flags

F90FLAGS= 
F90= f90

# 
# Define the C compile flags
# -D_UF defines UNIX naming conventions for mixed language compilation.
# 
CFLAGS= -D_UF -O3
CC= gcc

# Define GMG objects
#
GMG = r_vector.o\
      solvers.o\
      ccfd.o\
      mf2kgmg.o\

# Define the libraries

#SYSLIBS= -lc
USRLIB  = 

# Define all Fortran object files which make up MODFLOW

OBJECTS = \
        gwf2bas7.o \
        de47.o \
        pcg7.o \
        sip7.o \
        gmg7.o \
        mhc7.o\
        gwf2bcf7.o \
        gwf2lpf7.o \
        gwf2huf7.o \
        gwf2sfr7.o \
        gwf2lak7.o \
        gwf2uzf1.o \
        gwf2gag7.o \
        gwf2chd7.o \
        gwf2drn7.o \
        gwf2drt7.o \
        gwf2ets7.o \
        gwf2evt7.o \
        gwf2fhb7.o \
        gwf2ghb7.o \
        gwf2hfb7.o \
        gwf2ibs7.o \
        gwf2rch7.o \
        gwf2res7.o \
        gwf2riv7.o \
        gwf2str7.o \
        gwf2sub7.o \
        gwf2swt7.o\
        gwf2wel7.o \
        hufutl7.o \
        obs2bas7.o \
        obs2drn7.o \
        obs2ghb7.o \
        obs2riv7.o \
        obs2chd7.o \
        parutl7.o \
        gwf2mnw7.o \
        gwf2bfh1.o \
        gwf2lgr1.o \
        utl7.o \
        lmt7.o \
        gwf2hydmod7.o \
        mflgr.o \

# Define Task Function

all: mflgr

# Define what mflgr

mflgr: $(OBJECTS) $(GMG)
	-$(F90) $(F90FLAGS) -o mflgr $(OBJECTS) $(GMG) $(USRLIB) $(SYSLIBS)

# Object codes

.f.o:
	$(F90) $(F90FLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<

clean:
	-del *.o core *.mod

#  end
