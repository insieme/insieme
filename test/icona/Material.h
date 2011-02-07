/*
 * Material.h
 *
 *  Created on: 18.08.2010
 *      Author: clemens
 */

#ifndef MATERIAL_H_
#define MATERIAL_H_

class Material {
public:
	Material();
	Material( std::string name);
	virtual ~Material();

	//public methods
	std::string getMaterialName();
	void setElastic( double eModul_, double nue_);
	double getEModul();
	double getNue();
	double getAlphaT();
	void setAlphaT( double value);
	
private:
	double eModul;
	double nue;
	double alphaT;	

	std::string materialName;
};

#endif /* MATERIAL_H_ */
