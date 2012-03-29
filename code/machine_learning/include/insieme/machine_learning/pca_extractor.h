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

#include "ReClaM/PCA.h"
#include "ReClaM/LinearModel.h"


/*
 * This class is designed to read a set of features from a database and generate a certain number of principal components from them
 * The principal components will be written back to the database
 */

class PcaExtractor {
	AffineLinearMap map;
	PCA pca;

	Kompex::SQLiteDatabase *pDatabase;
	std::string dbPath;
	Kompex::SQLiteStatement *pStmt;

	std::vector<std::string> staticFeatures, dynamicFeatures;
	std::string query;

//	Array<double> featureNormalization;
//	std::ostream& out;

public:
	PcaExtractor(const std::string& myDbPath, size_t nInFeatures, size_t nOutFeatures)
		: map(nInFeatures, nOutFeatures), pDatabase(new Kompex::SQLiteDatabase(myDbPath, SQLITE_OPEN_READWRITE, 0)),
		  pStmt(new Kompex::SQLiteStatement(pDatabase)){}

	/*
	 * generates the default query, querying for all patterns which have all features set
	 * using the current values for the features
	 */
	virtual void genDefaultQuery() =0;

	/*
	 * calculates the principal components based on the given query and stores them in the database
	 */
	virtual void calcPca() =0;

	/**
	 * adds a vector of static features indices to the internal feature vector
	 * @param featureIndices a vector holding the column (in the database) indices of some static features
	 */
	void setStaticFeaturesByIndex(const std::vector<std::string>& featureIndices);
	/**
	 * adds one feature index to the internal static feature vector
	 * @param featureIndex the index of the column (in the database) holding a feature
	 */
	void setStaticFeatureByIndex(const std::string featureIndex);

	/**
	 * adds a vector of static features to the internal feature vector by name
	 * @param featureNames a vector holding the name (in the database) of some static features
	 */
	void setStaticFeaturesByName(const std::vector<std::string>& featureNames);
	/**
	 * adds one feature to the internal static feature vector by name
	 * @param featureName the name of a feature (in the database)
	 */
	void setStaticFeatureByName(const std::string featureName);

	/**
	 * adds a vector of dynamic features indices to the internal feature vector
	 * @param featureIndices a vector holding the column (in the database) indices of some dynamic features
	 */
	void setDynamicFeaturesByIndex(const std::vector<std::string>& featureIndices);
	/**
	 * adds one feature index to the internal dynamic feature vector
	 * @param featureIndex the index of the column (in the database) holding a feature
	 */
	void setDynamicFeatureByIndex(const std::string featureIndex);

	/**
	 * adds a vector of dynamic features to the internal feature vector by name
	 * @param featureNames a vector holding the name (in the database) of some dynamic features
	 */
	void setDynamicFeaturesByName(const std::vector<std::string>& featureNames);
	/**
	 * adds one feature to the internal dynamic feature vector by name
	 * @param featureName the name of a feature (in the database)
	 */
	void setDynamicFeatureByName(const std::string featureName);

	/**
	 * returns the number of all (static + dynamic) features
	 * @return the number of features
	 */
	size_t nFeatures() { return staticFeatures.size() + dynamicFeatures.size(); }

};

