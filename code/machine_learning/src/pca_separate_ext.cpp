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
#include "insieme/machine_learning/machine_learning_exception.h"

// needed to plot Array
#include "ReClaM/Rprop.h"


namespace insieme {
namespace ml {

void PcaSeparateExt::genDefaultQuery() {
	std::stringstream qss;
	qss << "SELECT \n";
	size_t c = staticFeatures.size();
	for(size_t i = 1; i < c; ++i) {
		qss << " c" << i << ".value AS Feature" << i << ",\n";
	}
	qss << "c0.value AS Feature0 FROM code c0\n";
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
c0.value AS Feature0 FROM code c0
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
	qss << "s0.value AS Feature0 FROM setup c0\n";
	for(size_t i = 1; i < s; ++i) {
		qss << " JOIN setup s" << i << " ON c" << i << ".sid = c0.sid ";
		qss << "AND s" << i << ".fid=" << dynamicFeatures[i] << " AND c0.fid=" << dynamicFeatures[0] << std::endl;
	}

std::cout << "Query: \n" << qss.str() << std::endl;
	query = qss.str();
}


size_t PcaSeparateExt::readDatabase(Array<double>& in) throw(Kompex::SQLiteException) {
	Kompex::SQLiteStatement *localStmt = new Kompex::SQLiteStatement(pDatabase);

	localStmt->Sql(query);

	size_t nRows = localStmt->GetNumberOfRows();
	in = Array<double>(nRows, staticFeatures.size());
	LOG(INFO) << "Queried Rows: " << nRows << ", Number of static features: " << staticFeatures.size() << std::endl;

	if(nRows == 0)
		throw MachineLearningException("No dataset for the requested features could be found");

	// load data
	size_t i = 0;
	// fetch all results
	while(localStmt->FetchRow()){
		// construct feature vectors
		for(size_t j = 0; j < staticFeatures.size(); ++j) {
			in(i, j) = localStmt->GetColumnDouble(j);
		}

		++i;
	}

	// reset the prepared statement
	localStmt->Reset();

	// do not forget to clean-up
	localStmt->FreeQuery();
	delete localStmt;

//	FeaturePreconditioner fp;
//	featureNormalization = fp.normalize(in, -1, 1);
	return nRows;
}

void PcaSeparateExt::calcPca() {
	assert((staticFeatures.size() + dynamicFeatures.size()) > 0 && "Cannot do PCA without any features set");
	if(query.size() == 0)
		genDefaultQuery();

	Array<double> in, out;
	try {
		readDatabase(in);
	}catch(Kompex::SQLiteException& sqle) {
		const std::string err = "\nSQL query for training data failed\n" ;
		LOG(ERROR) << err << std::endl;
		sqle.Show();
		throw ml::MachineLearningException(err);
	}

	std::cout << "BEFORE " << in << std::endl;

	std::cout << pca.optimize(map, in) << std::endl;
	std::cout << "AFTER " << in << std::endl;

// 	std::cout << "REsult: " << out << std::endl;
}

} // end namespace ml
} // end namespace insieme
