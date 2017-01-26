/*******************************************************************************
 *                      LTE UPLINK RECEIVER PHY BENCHMARK                      *
 *                                                                             *
 * This file is distributed under the license terms given by LICENSE.TXT       *
 ******************************************************************************/

#include "n_prs_all_ns.h"
#include "feldspar.h"

/** Function: n__prs__all__ns
 ******************************************************************************
 *
 * @brief Generates pseudo random sequence n_prs for the cell id of the current cell.
 * This should be static during the lifetime of the process.
 *
 *
 * @param[in]   var0: N_Cell_ID   Input data, cell id
 * @param[in]   out_0:            Pointer to output data area for length of n_prs vector
 * @param[in]   out_1: n_prs      Pointer to output data area for n_prs consisting of 20 U16
 * @return      void
 *
 *****************************************************************************/
void n__prs__all__ns(signed int var0, signed int * out_0, signed int * out_1) {
  signed int var49;
  signed int var52;
  int var56;
  signed int var57;
  signed int var63_0;
  U32 var63_1_0;
  U32 var63_1_1;

  quot(var0, 30, &var49);
  quot(var0, 30, &var52);
  var56 = (((var0 % 30) != 0) && (var0 < 0));
  if(var56) {
    var57 = (var49 - 1);
  } else {
    var57 = var52;
  }
  intToU32(((var57 * 32) + (var0 % 30)), &var63_1_1);
  var63_0 = 31;
  var63_1_0 = 1;
  {
    while((var63_0 <= 1600)) {
      U32 var28_1_0;
      U32 var28_1_1;
      U32 var36;
      U32 var47;

      var28_1_0 = var63_1_0;
      var28_1_1 = var63_1_1;
      var63_0 = (var63_0 + 1);
      var36 = ((var28_1_0 | ((((var28_1_0 >> 3) ^ (var28_1_0 >> 0)) & 1) << 31)) >> 1);
      var47 = ((var28_1_1 | ((((var28_1_1 >> 3) ^ ((var28_1_1 >> 2) ^ ((var28_1_1 >> 1) ^ (var28_1_1 >> 0)))) & 1) << 31)) >> 1);
      var63_1_0 = var36;
      var63_1_1 = var47;
    }
  }
  {
    signed int var1;
    for(var1 = 0; var1 < 20; var1 += 1) {
      signed int var4;
      signed int var64_0;
      U32 var64_1_0;
      U32 var64_1_1;
      
      var4 = ((56 * var1) + 8);
      var64_0 = 0;
      var64_1_0 = var63_1_0;
      var64_1_1 = var63_1_1;
      {
	while((var64_0 <= var4)) {
	  U32 var6_1_0;
	  U32 var6_1_1;
	  U32 var14;
	  U32 var25;

	  var6_1_0 = var64_1_0;
	  var6_1_1 = var64_1_1;
	  var64_0 = (var64_0 + 1);
	  var14 = ((var6_1_0 | ((((var6_1_0 >> 3) ^ (var6_1_0 >> 0)) & 1) << 31)) >> 1);
	  var25 = ((var6_1_1 | ((((var6_1_1 >> 3) ^ ((var6_1_1 >> 2) ^ ((var6_1_1 >> 1) ^ (var6_1_1 >> 0)))) & 1) << 31)) >> 1);
	  var64_1_0 = var14;
	  var64_1_1 = var25;
	}
      }
      u32ToInt((((var64_1_0 ^ var64_1_1) >> 21) & 255), &(out_1[var1]));
    }
  }
}
