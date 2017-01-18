/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#ifndef FELDSPAR_H
#define FELDSPAR_H

#include "missingFunc.h"
#include "feldspar_intel_functions.h"

signed int mod_fun_signed_int( signed int, signed int );
int mod_fun_unsigned_int( unsigned int, unsigned int );
long mod_fun_signed_long( long, long );
long mod_fun_unsigned_long( unsigned long, unsigned long );

int pow_fun_signed_int( int, int );
int pow_fun_unsigned_int( unsigned int, unsigned int );

int bit_fun_signed_int( int );
int setBit_fun_signed_int( int, int );
int clearBit_fun_signed_int( int, int );
int complementBit_fun_signed_int( int, int );
int testBit_fun_signed_int( int, int );
int rotateL_fun_signed_int( int, int );
int rotateR_fun_signed_int( int, int );
int bitSize_fun_signed_int( int );
int isSigned_fun_signed_int( int );

int abs_fun_signed_int( int );
int abs_fun_unsigned_int( unsigned int );
long abs_fun_signed_long( long );
long abs_fun_unsigned_long( unsigned long );

int signum_fun_signed_int( int );
int signum_fun_unsigned_int( unsigned int );
long signum_fun_signed_long( long );
long signum_fun_unsigned_long( unsigned long );

void copy_arrayOf_signed_int( int*, int, int* );
void copy_arrayOf_unsigned_int( unsigned int*, int, unsigned int* );
void copy_arrayOf_signed_long( long*, int, long* );
void copy_arrayOf_unsigned_long( unsigned long*, int, unsigned long* );

#endif
