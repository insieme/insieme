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

#include <boost/functional/hash.hpp>

#include "Array/Array.h"

#include "insieme/utils/logging.h"

#include "insieme/machine_learning/pca_combined_ext.h"

// needed to plot Array
#include "ReClaM/Rprop.h"
#include "ReClaM/MeanSquaredError.h"


namespace insieme {
namespace ml {

/*
 * generates pids for the new principal components and writes them into the measurements table
 */
Array<int64> PcaCombinedExt::genPids(Array<int64> ids, size_t nPids) {
	Array<int64> pids(nPids);
	boost::hash<std::string> string_hash;

	try {
		pStmt->BeginTransaction();
		Kompex::SQLiteStatement readPid(pDatabase);

		for(size_t i = 0; i < nPids; ++i) {
			// check if a pid is already there. If yes, use it
			std::stringstream readPids;
			readPids << "SELECT pid FROM measurement \nWHERE cid = " << ids(i,0) << " AND sid = " << ids(i,1);
			if(int64 pid = readPid.GetSqlResultInt64(readPids.str())) {
				pids(i) = pid;
				continue;
			}

			std::stringstream writePids;
			writePids << mangling << i;
			size_t pcaIds = string_hash(writePids.str());
			pids(i) = pcaIds;

			std::stringstream updateM;
			updateM << "UPDATE measurement\n SET pid = " << pcaIds << "\nWHERE cid = " << ids(i,0) << " AND sid = " << ids(i,1);

			pStmt->Sql(updateM.str());
			pStmt->ExecuteAndFree();

		}

		pStmt->CommitTransaction();
	} catch(Kompex::SQLiteException& sqle) {
		const std::string err = "\ninserting pids into measurement table failed\n" ;
		LOG(ERROR) << err << std::endl;
		sqle.Show();
		throw ml::MachineLearningException(err);
	}

	return pids;
}

/*
 * generates the default query, querying for all static features which share a common cid and have been specified
 * using setStaticFeatures or setDynamicFeatures before
 * The first n columns of the query must contain the values of the n features, the n+1 column must hold the [c|s]id
 * The rows represent features of different codes/setups
 */
void PcaCombinedExt::genDefaultQuery() {
	std::stringstream qss;
	qss << "SELECT \n ";
	size_t s = staticFeatures.size();
	size_t d = dynamicFeatures.size();
	for(size_t i = 0; i < s; ++i) {
		qss << " c" << i << ".value AS Feature" << i << ",\n";
	}
	for(size_t i = 0; i < d; ++i) {
		qss << " s" << i << ".value AS Feature" << i + s << ",\n";
	}
	qss << " m.cid, m.sid AS target FROM measurement m \n";
	for(size_t i = 0; i < s; ++i) {
		qss << " JOIN code c" << i << " ON m.cid=c" << i << ".cid AND c" << i << ".fid=" << staticFeatures[i] << std::endl;
	}
	for(size_t i = 0; i < d; ++i) {
		qss << " JOIN setup s" << i << " ON m.sid=s" << i << ".sid AND s" << i << ".fid=" << dynamicFeatures[i] << std::endl;
	}
//std::cout << "Query: \n" << qss.str() << std::endl;
	query = qss.str();
}

/*
 * calculates the principal components of static features based on the given query and stores them in the database
 */
size_t PcaCombinedExt::calcPca(double toBeCovered) {
	size_t nFeatures = staticFeatures.size() + dynamicFeatures.size();
	assert(nFeatures > 0 && "Cannot do PCA without any features set");

	Array<double> in;
	Array<int64> ids;

	if(query.size() == 0)
		genDefaultQuery();

	readDatabase(in, ids, nFeatures, 2);

	// ids to connect measurements with pca values
	Array<int64> pcaIds = genPids(ids, in.rows());

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

	writeToPca(out, pcaIds);

    return out.cols();
}


/*
 * calculates the principal components of static features based on the given query and stores them in the database
 */
double PcaCombinedExt::calcPca(size_t nOutFeatures, size_t unused) {
	size_t nFeatures = staticFeatures.size() + dynamicFeatures.size();
	assert(nFeatures > 0 && "Cannot do PCA without any features set");

	Array<double> in;
	Array<int64> ids;

	if(query.size() == 0)
		genDefaultQuery();

	size_t nPatterns = readDatabase(in, ids, nFeatures, 2);

	// ids to connect measurements with pca values
	Array<int64> pcaIds = genPids(ids, nPatterns);

	AffineLinearMap model(nFeatures, nOutFeatures);
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
	LOG(INFO) << nOutFeatures << " PCs cover " << covered << "% of the feature's total variance\n";

 	Array<double> out = genPCs(model, in);

// 	std::cout << "REsult: " << trans << std::endl;
//	std::cout << "AFTER " << eigenvalues << std::endl;
//    std::cout << "modeld " << out << std::endl;

 	writeToPca(out, pcaIds);

	return covered;
}


} // end namespace ml
} // end namespace insieme
