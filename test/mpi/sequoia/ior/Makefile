#/*****************************************************************************\
#*                                                                             *
#*       Copyright (c) 2003, The Regents of the University of California       *
#*     See the file COPYRIGHT for a complete copyright notice and license.     *
#*                                                                             *
#*******************************************************************************
#*
#* CVS info:
#*   $RCSfile: Makefile,v $
#*   $Revision: 1.8 $
#*   $Date: 2005/10/17 23:50:18 $
#*   $Author: loewe $
#*
#* Purpose:
#*       Make IOR executable.
#*
#*       gmake posix      -- IOR with only POSIX interface
#*       gmake mpiio      -- IOR with only POSIX and MPIIO interfaces
#*       gmake hdf5       -- IOR with POSIX, MPIIO, and HDF5 interfaces
#*       gmake ncmpi      -- IOR with POSIX, MPIIO, and NCMPI interfaces
#*       gmake all        -- IOR with POSIX, MPIIO, HDF5, and NCMPI interfaces
#*       gmake clean      -- remove executable and object files
#*
#\*****************************************************************************/

SRC = ./src/C

posix:
	(cd $(SRC) && $(MAKE) posix)

%:
	(cd $(SRC) && $(MAKE) $@)
