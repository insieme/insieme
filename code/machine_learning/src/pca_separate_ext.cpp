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

#include <iostream>

#include "Array/Array.h"

#include "insieme/utils/logging.h"

#include "insieme/machine_learning/pca_separate_ext.h"

// needed to plot Array
#include "ReClaM/Rprop.h"
#include "ReClaM/MeanSquaredError.h"


namespace insieme {
namespace ml {

/*
 * applies query to read the static features from the database
 */
size_t PcaSeparateExt::readFromDatabase(Array<double>& in, Array<int64>& ids, std::vector<std::string> features) throw(ml::MachineLearningException) {
	if(query.size() == 0)
		genDefaultQuery();

	try {
		return readDatabase(in, ids, features);
	}catch(Kompex::SQLiteException& sqle) {
		const std::string err = "\nSQL query for static features failed\n" ;
		LOG(ERROR) << err << std::endl;
		sqle.Show();
		throw ml::MachineLearningException(err);
	}

	return 0u;
}

/*
 * calculates the pca for the static code features or dynamic setup features
 */
size_t PcaSeparateExt::calcSpecializedPca(double toBeCovered, bool dynamic) {
	Array<double> in;
	Array<int64> ids;

	dynamic ? genDefaultDynamicQuery() : genDefaultQuery();
	readFromDatabase(in, ids, dynamic ? dynamicFeatures : staticFeatures);

	AffineLinearMap model(in.cols(), in.cols());
	Array<double> eigenvalues;

	genPCAmodel(model, in, eigenvalues);

	double sum = 0, partSum = 0;

	for(size_t i = 0; i < eigenvalues.nelem(); ++i) {
		sum += eigenvalues(i);
	}

	size_t nPCs = 0;
	toBeCovered /= 100.0;
	for(size_t i = 0; i < model.getOutputDimension(); ++i) {
		partSum += eigenvalues(i);
		if(partSum / sum > toBeCovered) {
			nPCs = i+1;
			break;
		}
	}


	AffineLinearMap reductionModel(in.cols(), nPCs);

	genPCAmodel(reductionModel, in);

//	(reductionModel.getOutputDimension(), dynamic ? dynamicFeatures.size() : staticFeatures.size());

	LOG(INFO) << reductionModel.getOutputDimension() << " PCs cover " << (partSum/sum)*100.0 << "% of the static feature's total variance\n";

 	Array<double> out = genPCs(reductionModel, in);

// 	std::cout << "REsult: " << trans << std::endl;
//	std::cout << "AFTER " << eigenvalues << std::endl;
//    std::cout << "modeld " << out << std::endl;

 	writeToCode(out, ids);

    return out.cols();

}

/*
 * calculates the principal components of static features based on the given query and stores them in the database
 */
double PcaSeparateExt::calcSpecializedPca(size_t nInFeatures, size_t nOutFeatures, bool dynamic) {
	Array<double> in;
	Array<int64> ids;

	dynamic ? genDefaultDynamicQuery() : genDefaultQuery();
	readFromDatabase(in, ids, dynamic ? dynamicFeatures : staticFeatures);

	AffineLinearMap model(dynamic ? dynamicFeatures.size() : staticFeatures.size(), nOutFeatures);
	Array<double> eigenvalues;

	genPCAmodel(model, in, eigenvalues);

	// calculate the percentage of covered variance
	double sum = 0, partSum = 0;
	size_t i = 0;
	for(; i < nOutFeatures; ++i)
		partSum += eigenvalues(i);
	sum = partSum;
	for(; i < eigenvalues.nelem(); ++i)
		sum += eigenvalues(i);
	double covered = (partSum / sum) * 100.0;
	LOG(INFO) << nOutFeatures << " PCs cover " << covered << "% of the " << (dynamic ? "dynamic" : "static") << " feature's total variance\n";

 	Array<double> out = genPCs(model, in);

// 	std::cout << "REsult: " << trans << std::endl;
//	std::cout << "AFTER " << eigenvalues << std::endl;
//    std::cout << "modeld " << out << std::endl;

 	if(dynamic)
 		writeToSetup(out, ids);
 	else
 		writeToCode(out, ids);

	return covered;

}

/*
 * generates the default query, querying for all static features which share a common cid and have been specified
 * using setStaticFeatures before
 * The first n columns of the query must contain the values of the n features, the n+1 column must hold the [c|s]id
 * The rows represent features of different codes/setups
 */
void PcaSeparateExt::genDefaultQuery() {
	std::stringstream qss;
	qss << "SELECT \n";
	size_t c = staticFeatures.size();
	for(size_t i = 1; i < c; ++i) {
		qss << " c" << i << ".value AS Feature" << i << ",\n";
	}
	qss << "c0.value AS Feature0, c0.cid FROM code c0\n";
	for(size_t i = 1; i < c; ++i) {
		qss << " JOIN code c" << i << " ON c" << i << ".cid = c0.cid ";
		qss << "AND c" << i << ".fid=" << staticFeatures[i] << " AND c0.fid=" << staticFeatures[0] << std::endl;
	}

//std::cout << "Query: \n" << qss.str() << std::endl;
	query = qss.str();

/*
SELECT
 c1.value AS Feature1,
 c2.value AS Feature2,
c0.value AS Feature0, c0.cid FROM code c0
 JOIN code c1 ON c1.cid = c0.cid AND c1.fid=2 AND c0.fid=1
 JOIN code c2 ON c2.cid = c0.cid AND c2.fid=3 AND c0.fid=1
*/
}

void PcaSeparateExt::genDefaultDynamicQuery() {
	std::stringstream qss;
	qss << "SELECT \n";
	size_t s = dynamicFeatures.size();
	for(size_t i = 1; i < s; ++i) {
		qss << " s" << i << ".value AS Feature" << i << ",\n";
	}
	qss << "s0.value AS Feature0, s0.sid FROM setup s0\n";
	for(size_t i = 1; i < s; ++i) {
		qss << " JOIN setup s" << i << " ON c" << i << ".sid = c0.sid ";
		qss << "AND s" << i << ".fid=" << dynamicFeatures[i] << " AND c0.fid=" << dynamicFeatures[0] << std::endl;
	}

//std::cout << "Query: \n" << qss.str() << std::endl;
	Array<double> in;

	query = qss.str();
}


size_t PcaSeparateExt::calcPca(double toBeCovered) {
	assert((staticFeatures.size() + dynamicFeatures.size()) > 0 && "Cannot do PCA without any features set");

	size_t nPCs = 0;

	if(staticFeatures.size() != 0)
		nPCs += calcSpecializedPca(toBeCovered, false);

	if(dynamicFeatures.size() != 0)
		nPCs += calcSpecializedPca(toBeCovered, true);

	return nPCs;
}

/*
 * calculates the principal components of static features based on the given query and stores them in the database
 */
double PcaSeparateExt::calcPca(size_t nDynamicOutFeatures, size_t nStaticOutFeatures) {
	assert((staticFeatures.size() + dynamicFeatures.size()) > 0 && "Cannot do PCA without any features set");

	double covered = 0.0, divisor = 0.0;

	if(dynamicFeatures.size() != 0) {
		assert(dynamicFeatures.size() >= nDynamicOutFeatures && "Number of dynamic pcs must be lower than number of dynamic features");
		covered += calcSpecializedPca(nDynamicOutFeatures, true);
		++divisor;
	}

	if(staticFeatures.size() != 0) {
		assert(staticFeatures.size() >= nStaticOutFeatures && "Number of static pcs must be lower than number of static features");
		covered += calcSpecializedPca(nStaticOutFeatures, false);
		++divisor;
	}

	if(divisor == 0.0)
		return 0.0;

	return covered / divisor;
}


} // end namespace ml
} // end namespace insieme
