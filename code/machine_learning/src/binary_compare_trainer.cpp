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

#include "ReClaM/Svm.h"
#include <ReClaM/ClassificationError.h>

#include "insieme/machine_learning/binary_compare_trainer.h"
#include "insieme/machine_learning/feature_preconditioner.h"
#include "insieme/machine_learning/evaluator.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace ml {

void BinaryCompareTrainer::generateCrossProduct(const Array<double>& in, Array<double>& crossProduct, const Array<double>& measurements,
		Array<double>& target, size_t outDim) {
	size_t n = in.dim(0);
	size_t m = in.dim(1);

	// create an array for the both possible cases:
	// measurement for first dataset is bigger
	Array<double> first;
	// measurment for second dataset is bigger
	Array<double> second;
	if(outDim == 1) {
		first.append_elem(NEG);
		second.append_elem(POS);
	} else {
		first.append_elem(POS);
		first.append_elem(NEG);
		second.append_elem(NEG);
		second.append_elem(POS);
	}
	size_t row = 0;

	crossProduct = Array<double>(in.dim(0)*in.dim(0)-in.dim(0), 2*in.dim(1));

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
			target.append_rows(measurements(i) > measurements(j) ? first : second);
		}
	}

}

void BinaryCompareTrainer::appendToTrainArray(Array<double>& target, Kompex::SQLiteStatement* localStmt, size_t queryIdx,
		double max, double min, Array<double>& oneOfN){
	target.append_elem(localStmt->GetColumnDouble(queryIdx));
}

/*
 * trains the model using the patterns returned by the given query or the default query if none is given
  */
double BinaryCompareTrainer::train(Optimizer& optimizer, ErrorFunction& errFct, size_t iterations) throw(MachineLearningException) {
	Array<double> input;
	return train(optimizer, errFct, input, iterations);
}

double BinaryCompareTrainer::train(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, size_t iterations) throw(MachineLearningException) {

	if(TRAINING_OUTPUT)
		writeHeader("Binary compare trainer", optimizer, errFct, iterations);


	double error = 0;
	try {
		// svms don't set their input/output sizes. But they only have tow parameter, they are recognized like that
		if(model.iterativeTraining() && nFeatures() * 2 != model.getInputDimension())
			throw MachineLearningException("Number of selected features is not half of the model's input size");

		size_t outDim = model.getOutputDimension();
		if(outDim > 2)
			throw MachineLearningException("MyModel must have a binary output");

		Array<double> measurements;

		size_t nRows = readDatabase(in, measurements);

		// do the actual training
		optimizer.init(model.getModel());

		Array<double> crossProduct, target;

		generateCrossProduct(in, crossProduct, measurements, target, outDim);

		// check if we are dealing with an svm, in this case the iterations argument is ignored
		if(SVM_Optimizer* svmOpt = dynamic_cast<SVM_Optimizer*>(&optimizer)){
			MyC_SVM* csvm = dynamic_cast<MyC_SVM*>(&model);
			svmOpt->optimize(csvm->getSVM(), crossProduct, target, true);
			error = errFct.error(model.getModel(), crossProduct, target);
		}  else if(iterations != 0) {
			for(size_t i = 0; i < iterations; ++i){
				optimizer.optimize(model.getModel(), errFct, crossProduct, target);

				if(TRAINING_OUTPUT)
					writeStatistics(i, crossProduct, target, errFct, -1.0);
			}
			error = errFct.error(model.getModel(), crossProduct, target);
		}
		else
			error = myEarlyStopping(optimizer, errFct, crossProduct, target, 10, std::max(static_cast<size_t>(1),nRows*nRows/2000));

		Array<double> out(model.getOutputDimension());
		size_t misClass = 0;
		for(size_t i = 0; i < crossProduct.dim(0); ++i ) {
			model.getModel().model(crossProduct.subarr(i,i), out);
//				std::cout << oneOfNtoIdx(target.subarr(i,i)) << " - " <<  oneOfNtoIdx(out) << std::endl;
//				std::cout << target.subarr(i,i) << out << std::endl;
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

/*
 * Reads data form the database according to the current query, tests all patterns with the current model
 * and returns the error according to the error function
 */
double BinaryCompareTrainer::evaluateDatabase(ErrorFunction& errFct) throw(MachineLearningException) {
	try {
		if(!model.iterativeTraining() && nFeatures() * 2 != model.getInputDimension()) {
			std::cerr << "Number of features: " << nFeatures() << "\nModel input size: " << model.getInputDimension() << std::endl;
			throw MachineLearningException("Number of selected features is not equal to the model's input size");
		}

		Array<double> in, target;

		readDatabase(in, target);

		return errFct.error(model.getModel(), in, target);
	} catch(Kompex::SQLiteException& sqle) {
		const std::string err = "\nSQL query for data failed\n" ;
		LOG(ERROR) << err << std::endl;
		sqle.Show();
		throw ml::MachineLearningException(err);
	}catch (std::exception &exception) {
		const std::string err = "\nQuery for data failed\n" ;
		LOG(ERROR) << err << exception.what() << std::endl;
		throw ml::MachineLearningException(err);
	}
	return -1.0;
}

/*
 * Evaluates a pattern using the internal model.
 */
size_t BinaryCompareTrainer::evaluate(Array<double>& pattern){
	Evaluator eval(model, featureNormalization);

	return eval.binaryCompare(pattern);
}

/*
 * Evaluates a pattern using the internal model
 */
size_t BinaryCompareTrainer::evaluate(const Array<double>& pattern1, const Array<double>& pattern2){
	Evaluator eval(model, featureNormalization);

	return eval.binaryCompare(pattern1, pattern2);
}


} // end namespace ml
} // end namespace insieme
