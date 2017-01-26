/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include "feldspar.h"

void r__alpha(signed int var0_0_0, signed int var0_0_1, signed int var0_1_0, signed int var0_1_1, RealNum var0_2, signed int *out_0, complex *out_1) {
  signed int var10[35] = {11,23,31,47,59,71,89,107,113,139,179,191,211,239,283,293,317,359,383,431,479,523,571,599,647,719,761,863,887,953,971,1069,1151,1193,1291};
  signed int var13[35] = {12,24,36,48,60,72,96,108,120,144,180,192,216,240,288,300,324,360,384,432,480,540,576,600,648,720,768,864,900,960,972,1080,1152,1200,1296};
  signed int var21;
  signed int var26;
  signed int var28;
  signed int var29;
  signed int var32;
  int var36;
  signed int var37;
  signed int var42;
  signed int var43;
  signed int var46;
  int var50;
  signed int var51;
  signed int var56;
  signed int var61;
  signed int var63;
  
  (* out_0) = (var0_1_1 - var0_1_0);
  var21 = 0;
  {
    int var17;
    int var18;
        
    var17 = (var21 < 35);
    if(var17) {
      var18 = (var13[var21] != var0_0_1);
    } else {
      var18 = 0;
    }
    while(var18) {
      var21 = (var21 + 1);
      var17 = (var21 < 35);
      if(var17) {
	var18 = (var13[var21] != var0_0_1);
      } else {
	var18 = 0;
      }
    }
  }
  var26 = (var0_0_0 % 30);
  var28 = (var10[var21] * (var26 + 1));
  quot(var28, 31, &var29);
  quot(var28, 31, &var32);
  var36 = (((var28 % 31) != 0) && (var28 < 0));
  if(var36) {
    var37 = (var29 - 1);
  } else {
    var37 = var32;
  }
  var42 = ((var10[var21] * (var26 + 1)) * 2);
  quot(var42, 31, &var43);
  quot(var42, 31, &var46);
  var50 = (((var42 % 31) != 0) && (var42 < 0));
  if(var50) {
    var51 = (var43 - 1);
  } else {
    var51 = var46;
  }
  var56 = ((var37 + 1) + (0 * (1 - (2 * (var51 % 2)))));
  var61 = (2 * var10[var21]);
  quot(32767, var10[var21], &var63);
  {
    signed int var2;

    for(var2 = 0; var2 < (* out_0); var2 += 1) {
      signed int var3;
      RealNum var4;
      RealNum var5;
      RealNum var6;
      RealNum var7;
      complex var8;
      signed int var57;
      RealNum var66;
      RealNum var67;
      RealNum var68;
      complex var69;

      var3 = (var2 + var0_1_0);
      intToReal(var3, &var4);
      mul_RealNum(var4, var0_2, &var5);
      cos_feldspar(var5, &var6);
      sin_feldspar(var5, &var7);
      mkComplexReal(var6, var7, &var8);
      var57 = (var3 % var10[var21]);
      intToReal((65536 - ((((var56 * var57) * (var57 + 1)) % var61) * var63)), &var66);
      cos_feldspar(var66, &var67);
      sin_feldspar(var66, &var68);
      mkComplexReal(var67, var68, &var69);
      mul_ComplexReal(var8, var69, &(out_1[var2]));
    }
  }
}
