# Makefile example for compiling GMG and linking GMG to MF2K:

# Makefile for USGS distribution
#
# @ (#) Makefile for Modtools 27June1997
# (Supplied by L. Orzol -- erb)
#
# SHELL=/bin/ksh
SOURCELOC = 
UTILITYLOC = 
NEWMOD = 
PROGRAM = mflgr
INCDIR= .

# Define the Fortran compiler flags

#F90FLAGS= -O3 -tpp7 -axW -Vaxlib -cm

#F90= ifc

#F90FLAGS= -O2 -fno-second-underscore -r8
#F90FLAGS= -g -fbounds-check -fno-second-underscore -Wall -ftrace=full
F90FLAGS= -O2 -fno-second-underscore 
F90= g95



# 
# Define the C compile flags
# -D_UF defines UNIX naming conventions for mixed language compilation.
# 
#CFLAGS= -D_UF -O3 -ansi -pedantic
CFLAGS= -D_UF -O3 
CC= gcc

# Define GMG objects
#
GMG = r_vector.o\
      solvers.o\
      ccfd.o\
      mf2kgmg.o 

# Define the libraries

#SYSLIBS= -lmisalign -ldgc -lm 
#SYSLIBS= -lc
USRLIB  = 

# Define all object files which make up Modtools
# NOTE: main must be last and LGR must be after BCF, LPF, and HUF, and
#       MNW must be after solvers and flow packages.

OBJECTS = \
        gwf2bas7.o \
        gwf2bcf7.o \
        gwf2chd7.o \
        gwf2drn7.o \
        gwf2evt7.o \
        gwf2fhb7.o \
        gwf2ibs7.o \
        gwf2ghb7.o \
        gwf2hfb7.o \
	gwf2huf7.o \
        gwf2lpf7.o \
        gwf2rch7.o \
        gwf2riv7.o \
        gwf2str7.o \
        gwf2wel7.o \
        gwf2drt7.o \
        gwf2ets7.o \
        gwf2res7.o \
	gwf2lgr1.o \
	gwf2bfh1.o \
        parutl7.o \
	hufutl7.o \
	gwf2sfr7.o\
	gwf2lak7.o\
	gwf2gag7.o\
	gwf2uzf1.o\
	obs2bas7.o \
	obs2chd7.o \
	obs2drn7.o \
	obs2ghb7.o \
	obs2riv7.o \
        de47.o \
        gmg7.o \
        pcg7.o \
        sip7.o \
        gwf2sub7.o \
	gwf2mnw7.o \
        utl7.o \
        mflgr.o 

install: mflgr

# Define Task Function Program Modtools

all: mflgr

# Define what Modtools is

mflgr: $(OBJECTS) $(GMG)
	-$(F90) $(F90FLAGS) -o mflgr $(OBJECTS) $(GMG) $(USRLIB) $(SYSLIBS)

# Modtools_Object codes

mflgr.o: mflgr.f
	$(F90) $(F90FLAGS) -c mflgr.f


# Pth_Object codes of Modtools

.f.o:
	$(F90) $(F90FLAGS) -c $<

.c.o:
	$(CC) $(CFLAGS) -c $<

clean:
	- rm *.o core *.mod
#
#  end
