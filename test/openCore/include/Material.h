//
// Material.h
//----------------------------------------

#ifndef MATERIAL_H_
#define MATERIAL_H_

/** \class Material
 * \brief class storing information on one material
 *
 * \li material name
 * \li E-modul
 * \li nue
 * \li AlphaT
 */
struct Material {
	double eModul;
	double nue;
	double alphaT;	

	const char* materialName;
};

void setMaterialElastic(struct Material* mat, const double eModul_,const double nue_);

#endif /* MATERIAL_H_ */
