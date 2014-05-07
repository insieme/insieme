/************************************************************************
 *
 *  tmndec.h, TMN specific defines for tmndecode (H.263 decoder)
 *  Copyright (C) 1996  Telenor R&D, Norway
 *        Karl Olav Lillevold <Karl.Lillevold@nta.no>
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 *  Karl Olav Lillevold               <Karl.Lillevold@nta.no>
 *  Telenor Research and Development
 *  P.O.Box 83                        tel.:   +47 63 84 84 00
 *  N-2007 Kjeller, Norway            fax.:   +47 63 81 00 76
 *
 *  Robert Danielsen                  e-mail: Robert.Danielsen@nta.no
 *  Telenor Research and Development  www:    http://www.nta.no/brukere/DVC/
 *  P.O.Box 83                        tel.:   +47 63 84 84 00
 *  N-2007 Kjeller, Norway            fax.:   +47 63 81 00 76
 *  
 ************************************************************************/

/*
 * based on mpeg2decode, (C) 1994, MPEG Software Simulation Group
 * and mpeg2play, (C) 1994 Stefan Eckart
 *                         <stefan@lis.e-technik.tu-muenchen.de>
 *
 */



/* From sim.h */
#define PSC        1
#define PSC_LENGTH        17
#define SE_CODE                         31

#define MODE_INTER                      0
#define MODE_INTER_Q                    1
#define MODE_INTER4V                    2
#define MODE_INTRA                      3
#define MODE_INTRA_Q                    4

#define PBMODE_NORMAL                   0
#define PBMODE_MVDB                     1
#define PBMODE_CBPB_MVDB                2

#define ESCAPE                          7167
#define ESCAPE_INDEX                    102

#define PCT_INTER                       1
#define PCT_INTRA                       0
#define ON                              1
#define OFF                             0

#define SF_SQCIF                        1  /* 001 */
#define SF_QCIF                         2  /* 010 */
#define SF_CIF                          3  /* 011 */
#define SF_4CIF                         4  /* 100 */
#define SF_16CIF                        5  /* 101 */


/* this is necessary for the max resolution 16CIF */
#define MBC                             88
#define MBR                             72

#define NO_VEC                          999

