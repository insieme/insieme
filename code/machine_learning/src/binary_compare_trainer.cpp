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

/*
 * binary_compare_trainer.cpp
 *
 *  Created on: Nov 26, 2011
 *      Author: klaus
 */

#include <math.h>
#include <iostream>

#include "ReClaM/ValidationError.h"
#include "ReClaM/EarlyStopping.h"

#include "insieme/machine_learning/binary_compare_trainer.h"
#include "insieme/machine_learning/feature_preconditioner.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace ml {

void BinaryCompareTrainer::generateCrossProduct(const Array<double>& in, Array<double>& crossProduct, const std::vector<double>& measurements, Array<double>& target) {
	size_t n = in.dim(0);
	size_t m = in.dim(1);

	// create an array for the both possible cases:
	// measurement for first dataset is bigger
	Array<double> first(2);
	first(0) = POS;
	first(1) = NEG;
	// measurment for secondt dataset is bigger
	Array<double> second(2);
	second(0) = NEG;
	second(1) = POS;
	size_t row = 0;

	for(size_t i = 0; i < n; ++i) {
		for(size_t j = 0; j < n; ++j) {
			// do not create a new entry from a old entry and itself
			if(i == j)
				continue;
			for(size_t l = 0; l < m; ++l) {
				crossProduct(row, l) = in(i, l);
			}
			for(size_t l = 0; l < m; ++l) {
				crossProduct(row, m + l) = in(j, l);
			}
			++row;
			target.append_rows(measurements.at(i) > measurements.at(j) ? first : second);
		}
	}

}


double BinaryCompareTrainer::train(Optimizer& optimizer, ErrorFunction& errFct, size_t iterations) {
	if(features.size() * 2 != model.getInputDimension())
		throw MachineLearningException("Number of selected features is not half of the model's input size");

	if(model.getOutputDimension() != 2)
		throw MachineLearningException("Model must have a binary output");

	std::stringstream qss;
	qss << "SELECT \n m.id AS id, m.ts AS ts, \n";
	size_t n = features.size();
	for(size_t i = 0; i < n; ++i) {
		qss << " d" << i << ".value AS Feature" << i << ",\n";
	}
	qss << " m." << trainForName << " AS target FROM measurement m \n";
	for(size_t i = 0; i < n; ++i) {
		qss << " JOIN data d" << i << " ON m.id=d" << i << ".mid AND d" << i << ".fid=" << features[i] << std::endl;
	}

	std::string query = qss.str();
	double error = 0;

//		std::cout << "Query: \n" << query << std::endl;
	try {
		Kompex::SQLiteStatement *localStmt = new Kompex::SQLiteStatement(pDatabase);

		localStmt->Sql(query);

		size_t nRows = localStmt->GetNumberOfRows();
		Array<double> in(nRows, n);
		LOG(INFO) << "Queried Rows: " << nRows << ", Number of features: " << n << std::endl;
		if(nRows == 0)
			throw MachineLearningException("No dataset for the requested features could be found");

		std::vector<double> measurements;

		//Train machine
		size_t i = 0;
		// fetch all results
		while(localStmt->FetchRow()){
//				std::cout << "Result: " << localStmt->GetColumnName(2) << " " << localStmt->GetColumnName(3) << " " << localStmt->GetColumnName(4) << std::endl;
//				std::cout << "Data:   " << localStmt->GetColumnInt(2) << " " << localStmt->GetColumnInt(3) << " " << localStmt->GetColumnInt(4) << std::endl;

			// construct training vectors
			for(size_t j = 0; j < features.size(); ++j) {
				in(i, j) = localStmt->GetColumnDouble(j+2);
			}

			measurements.push_back(localStmt->GetColumnDouble(2+features.size()));

			++i;
		}


		FeaturePreconditioner fp(in);
		fp.normalize(-1, 1);

		// reset the prepared statement
		localStmt->Reset();

		// do not forget to clean-up
		localStmt->FreeQuery();
		delete localStmt;

		// do the actual training
		optimizer.init(model);

		Array<double> crossProduct(in.dim(0)*in.dim(0)-in.dim(0), 2*in.dim(1)), target;

		generateCrossProduct(in, crossProduct, measurements, target);

//std::cout << crossProduct << std::endl;
//std::cout << target << std::endl;
		if(iterations != 0) {
			for(size_t i = 0; i < iterations; ++i)
				optimizer.optimize(model, errFct, crossProduct, target);
			error = errFct.error(model, crossProduct, target);
		}
		else
			error = this->earlyStopping(optimizer, errFct, crossProduct, target, 10);

		Array<double> out(2);
		size_t misClass = 0;
		for(size_t i = 0; i < crossProduct.dim(0); ++i ) {
			model.model(crossProduct.subarr(i,i), out);
//				std::cout << target.subarr(i,i) << out << std::endl;
//				std::cout << oneOfNtoIdx(target.subarr(i,i)) << " - " <<  oneOfNtoIdx(out) << std::endl;
			if(oneOfNtoIdx(target.subarr(i,i)) != oneOfNtoIdx(out))
				++misClass;
		}
		LOG(INFO) << "Misclassification rate: " << (double(misClass)/crossProduct.dim(0)) * 100.0 << "%\n";

	} catch(Kompex::SQLiteException sqle) {
		const std::string err = "\nSQL query for data failed\n" ;
		LOG(ERROR) << err << std::endl;
		sqle.Show();
		throw ml::MachineLearningException(err);
	}catch (std::exception &exception) {
		const std::string err = "\nQuery for data failed\n" ;
		LOG(ERROR) << err << exception.what() << std::endl;
		throw ml::MachineLearningException(err);
	}
	return error;
}

} // end namespace ml
} // end namespace insieme
