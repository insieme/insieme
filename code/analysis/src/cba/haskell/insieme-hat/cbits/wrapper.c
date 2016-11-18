#include <stdlib.h>
#include "HsFFI.h"

int insieme_analysis_haskell_init(void)
{
  hs_init(NULL, NULL);
  return 0;
}

void insieme_analysis_haskell_exit(void)
{
  hs_exit();
}
