/*
 * Material.cpp
 *
 *  Created on: 18.08.2010
 *      Author: clemens
 */

#include<vector>
#include<string>

using namespace std;

#include "Material.h"

//constructors+destructor
Material::Material() {
}

Material::Material( string name) {
	materialName=name;
}

Material::~Material() {
}

//public methods
//get- + set-methods
string Material::getMaterialName() {
	return materialName;
}

void Material::setElastic( double eModul_, double nue_) {
	eModul=eModul_;
	nue=nue_;
}

double Material::getEModul() {
	return eModul;
}

double Material::getNue() {
	return nue;
}

double Material::getAlphaT() {
	return alphaT;
}

void Material::setAlphaT( double value) {
	alphaT=value;
}
