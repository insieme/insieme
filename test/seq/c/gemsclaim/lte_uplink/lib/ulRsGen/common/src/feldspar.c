/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include "feldspar.h"

signed int mod_fun_signed_int( signed int a, signed int b ) {
  return ((int)a % (int)b); // * (-1);
}

int mod_fun_unsigned_int( unsigned int a, unsigned int b ) {
  return a % b;
}

long mod_fun_signed_long( long a, long b ) {
  if ((a > 0 && b > 0) || (a < 0 && b < 0)) return a % b;
  return (a % b) * (-1);
}

long mod_fun_unsigned_long( unsigned long a, unsigned long b ) {
  return a % b;
}

int pow_fun_signed_int( int a, int b) {
  int out = 1;
  int i;
  for(i=0; i<b; i++) out *= a;
  return out;
}

int pow_fun_unsigned_int( unsigned int a, unsigned int b ) {
  int out = 1;
  unsigned int i;
  for(i=0; i<b; i++) out *= a;
  return out;
}

int bit_fun_signed_int( int i ) {
  return 1 << i;
}

int setBit_fun_signed_int( int x, int i ) {
  return x ^ 1 << i;
}

int clearBit_fun_signed_int( int x, int i ) {
  return x & ~(1 << i);
}

int complementBit_fun_signed_int( int x, int i ) {
  return x | 1 << i;
}

int testBit_fun_signed_int( int x, int i ) {
  return (x & (1 << i)) != 0;
}

int bit_shift_fun_signed_int( int x, int i ) {
  if (i < 0) return x >> -i;
  if (i > 0) return x << i;
  return x;
}

int bit_rotate_fun_signed_int( int x, int i ) {
  if (i < 0 && x < 0) {
    int left = i + sizeof(x) * 8;
    return ((x >> -i) & ~bit_shift_fun_signed_int(-1, left)) ^ bit_shift_fun_signed_int(x, left);
  }
  if (i < 0) return x >> -i ^ bit_shift_fun_signed_int(x, i + sizeof(x) * 8);
  else if (i == 0) return x;
  else return x << i ^ bit_shift_fun_signed_int(x, i - sizeof(x) * 8);
}

int rotateL_fun_signed_int( int x, int i ) {
  return bit_rotate_fun_signed_int(x, i);
}

int rotateR_fun_signed_int( int x, int i ) {
  return bit_rotate_fun_signed_int(x, -i);
}

int bitSize_fun_signed_int( int x ) {
  return sizeof x * 8;
}

int isSigned_fun_signed_int( int x ) {
  (void) x;
  return 1;
}

int abs_fun_signed_int( int a ) {
  if (a < 0) return a*(-1);
  return a;
}

int abs_fun_unsigned_int( unsigned int a ) {
  return a;
}

long abs_fun_signed_long( long a ) {
  if (a < 0) return a*(-1);
  return a;
}

long abs_fun_unsigned_long( unsigned long a ) {
  return a;
}

int signum_fun_signed_int( int a ) {
  if (a < 0) return -1;
  if (a > 0) return 1;
  return 0;
}

int signum_fun_unsigned_int( unsigned int a ) {
  if (a > 0) return 1;
  return 0;
}

long signum_fun_signed_long( long a ) {
  if (a < 0) return -1;
  if (a > 0) return 1;
  return 0;
}

long signum_fun_unsigned_long( unsigned long a ) {
  if (a > 0) return 1;
  return 0;
}

void copy_arrayOf_signed_int( int* a, int a1, int* b) {
  int i;
  for( i=0; i<a1; ++i )
    b[i] = a[i];
}

void copy_arrayOf_unsigned_int( unsigned int* a, int a1, unsigned int* b ) {
  int i;
  for( i=0; i<a1; ++i )
    b[i] = a[i];
}

void copy_arrayOf_signed_long( long* a, int a1, long* b ) {
  int i;
  for( i=0; i<a1; ++i )
    b[i] = a[i];
}

void copy_arrayOf_unsigned_long( unsigned long* a, int a1, unsigned long* b ) {
  int i;
  for( i=0; i<a1; ++i )
    b[i] = a[i];
}
