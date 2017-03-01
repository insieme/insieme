/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include "feldspar.h"

void alpha__n__prs(signed int var0_0_0, signed int var0_0_1, signed int var0_1_0, signed int var0_1_1, signed int * out_0, RealNum * out_1) {
  signed int var3[1][8] = {{0,2,3,4,6,8,9,10}};
  signed int var8[2][8] = {{0,2,3,4,6,8,9,10},{5,7,8,9,11,1,2,3}};
  signed int var13[4][8] = {{0,2,3,4,6,8,9,10},{3,4,5,6,9,11,0,1},{6,7,8,9,0,2,3,4},{9,10,11,0,3,5,6,7}};
  int var17;
  int var19;
  int var21;
  signed int var25[8] = {0,6,3,4,2,8,10,9};
  int var29;
  signed int var30;
    
  var17 = (var0_0_1 == 4);
  var19 = (var0_0_1 == 2);
  var21 = (var0_0_1 == 1);
  var29 = ((var0_1_1 > 7) || (var0_1_1 < 0));
  if(var29) {
    var30 = -1;
  } else {
    var30 = var25[var0_1_1];
  }
  {
    signed int var1;
    for(var1 = 0; var1 < var0_0_1; var1 += 1) {
      signed int var18;
      signed int var20;
      signed int var22;
            
      if(var17) {
	var18 = var13[var1][var0_1_0];
      } else {
	var18 = -1;
      }
      if(var19) {
	var20 = var8[var1][var0_1_0];
      } else {
	var20 = var18;
      }
      if(var21) {
	var22 = var3[var1][var0_1_0];
      } else {
	var22 = var20;
      }
    }
  }
}
