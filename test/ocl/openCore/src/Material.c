//
// Material.cpp
//----------------------------------------

#include "Material.h"

void setMaterialElastic(struct Material* mat, const double eModul_,const double nue_) {
	mat->eModul=eModul_;
	mat->nue=nue_;
}


