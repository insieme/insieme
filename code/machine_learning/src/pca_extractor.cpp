/**
 * Copyright (c) 2002-2013 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * We provide the software of this file (below described as "INSIEME")
 * under GPL Version 3.0 on an AS IS basis, and do not warrant its
 * validity or performance.  We reserve the right to update, modify,
 * or discontinue this software at any time.  We shall have no
 * obligation to supply such updates or modifications or any other
 * form of support to you.
 *
 * If you require different license terms for your intended use of the
 * software, e.g. for proprietary commercial or industrial use, please
 * contact us at:
 *                   insieme@dps.uibk.ac.at
 *
 * We kindly ask you to acknowledge the use of this software in any
 * publication or other disclosure of results by referring to the
 * following citation:
 *
 * H. Jordan, P. Thoman, J. Durillo, S. Pellegrini, P. Gschwandtner,
 * T. Fahringer, H. Moritsch. A Multi-Objective Auto-Tuning Framework
 * for Parallel Codes, in Proc. of the Intl. Conference for High
 * Performance Computing, Networking, Storage and Analysis (SC 2012),
 * IEEE Computer Society Press, Nov. 2012, Salt Lake City, USA.
 *
 * All copyright notices must be kept intact.
 *
 * INSIEME depends on several third party software packages. Please 
 * refer to http://www.dps.uibk.ac.at/insieme/license.html for details 
 * regarding third party software licenses.
 */

#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"

#include "insieme/machine_learning/machine_learning_exception.h"
#include "insieme/machine_learning/pca_extractor.h"

namespace insieme {
namespace ml {

PcaExtractor::PcaExtractor(const std::string& myDbPath, size_t nInFeatures, size_t nOutFeatures)
	: map(nInFeatures, nOutFeatures), pDatabase(new Kompex::SQLiteDatabase(myDbPath, SQLITE_OPEN_READWRITE, 0)),
	  pStmt(new Kompex::SQLiteStatement(pDatabase)) {
	try {
		pca.init(map);
	} catch(SharkException& e) {
		LOG(ERROR) << "In PcaExtractor constructor " << e.what() << std::endl;
	}

}

PcaExtractor::~PcaExtractor() {
	delete pStmt;
	delete pDatabase;
}

void PcaExtractor::setStaticFeaturesByIndex(const std::vector<std::string>& featureIndices) {
	for(std::vector<std::string>::const_iterator I = featureIndices.begin(); I != featureIndices.end(); ++I)
		staticFeatures.push_back(*I);
}
void PcaExtractor::setStaticFeatureByIndex(const std::string featureIndex) {
	staticFeatures.push_back(featureIndex);
}

void PcaExtractor::setStaticFeaturesByName(const std::vector<std::string>& featureNames){
	for(std::vector<std::string>::const_iterator I = featureNames.begin(); I != featureNames.end(); ++I) {
		setStaticFeatureByName(*I);
	}
}

void PcaExtractor::setStaticFeatureByName(const std::string featureName){
	// build query for name
	std::string tmp;
	try {
		std::stringstream qss;
		qss << "SELECT id FROM static_features f WHERE f.name = \"" << featureName << "\"";

		// query for the index of that name
		tmp = pStmt->GetSqlResultString(qss.str());

		// store feature index in field
		staticFeatures.push_back(tmp);
	} catch(Kompex::SQLiteException &exception)
	{
		tmp = "";
	}
	if(tmp == "") {
		std::string err = "\nCannot find feature " + featureName;
		LOG(ERROR) << err << std::endl;
		throw ml::MachineLearningException(err);
	}
}

void PcaExtractor::setDynamicFeaturesByIndex(const std::vector<std::string>& featureIndices) {
	for(std::vector<std::string>::const_iterator I = featureIndices.begin(); I != featureIndices.end(); ++I)
		dynamicFeatures.push_back(*I);
}
void PcaExtractor::setDynamicFeatureByIndex(const std::string featureIndex) {
	dynamicFeatures.push_back(featureIndex);
}

void PcaExtractor::setDynamicFeaturesByName(const std::vector<std::string>& featureNames){
	for(std::vector<std::string>::const_iterator I = featureNames.begin(); I != featureNames.end(); ++I) {
		setDynamicFeatureByName(*I);
	}
}

void PcaExtractor::setDynamicFeatureByName(const std::string featureName){
	// build query for name
	std::string tmp;
	try {
		std::stringstream qss;
		qss << "SELECT id FROM dynamic_features f WHERE f.name = \"" << featureName << "\"";

		// query for the index of that name
		tmp = pStmt->GetSqlResultString(qss.str());

		// store feature index in field
		dynamicFeatures.push_back(tmp);
	} catch(Kompex::SQLiteException &exception)
	{
		tmp = "";
	}
	if(tmp == "") {
		std::string err = "\nCannot find feature " + featureName;
		LOG(ERROR) << err << std::endl;
		throw ml::MachineLearningException(err);
	}
}


} // end namespace ml
} // end namespace insieme
