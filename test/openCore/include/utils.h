#include "CL/cl.h"

#include "config.h"
#include "Material.h"

#include "ght_hash_table.h"

#define CHKERRQ(err) if(err != CL_SUCCESS){ printf("Error %d\n", err), exit(-1); }

int errorCodeOut(cl_int error, const char* ownMessage);
const char* print_cl_errstring(const int err);
int splitfunction(const int elements,const int maxElements,const int allign,size_t** numElemPerCalc,size_t* numElemPerCalcSize,const int device);
