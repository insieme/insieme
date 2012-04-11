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
 * evaluator.cpp
 *
 *  Created on: Dec 1, 2011
 *      Author: klaus
 */
#include "Array/ArrayOp.h"
#include "insieme/machine_learning/evaluator.h"
#include "insieme/machine_learning/machine_learning_exception.h"

#include "insieme/utils/numeric_cast.h"

namespace insieme {
namespace ml {

namespace {
size_t getMaxIdx(Array<double> arr) {
	size_t idx = 0;
	double tmp = arr(0);

	for(size_t i = 1; i < arr.dim(0); ++i)
		if(arr(i) > tmp) {
			idx = i;
			tmp = arr(i);
		}

	return idx;
}
} // end anonymous namespace

/*
 * Evaluates a pattern using the internal model
 */
size_t Evaluator::eval_impl(Array<double>& pattern) {
	if(pattern.ndim() != 1 || ((model.getParameterDimension() > 2) && (pattern.dim(0) != model.getInputDimension())))
		throw MachineLearningException("Number of features in pattern does not match the model's input size");

	Array<double> out;
	model.model(pattern, out);

	// search the maximum in the output
	return getMaxIdx(out);
}


size_t Evaluator::evaluate(Array<double> pattern) {
	// apply the same transformations to the pattern to be tested as to the training dataset
	fp.transformData(pattern);

	return eval_impl(pattern);
}

size_t Evaluator::evaluate(const double* pattern, size_t nElems) {
	Array<double> arrayPattern;

	for(size_t i = 0; i < nElems; ++i)
		arrayPattern.append_elem(pattern[i]);

	return evaluate(arrayPattern);
}

size_t Evaluator::evaluate(const double* pattern) {
	if(model.getParameterDimension() <= 2)
		throw MachineLearningException("Cannot use 'evlauate(const double* pattern)' on this model. Use 'evaluate(const double* pattern, size_t nElems)'.");

	size_t nElems = model.getInputDimension();
	return evaluate(pattern, nElems);
}

/*
 * Evaluates a pattern using the internal model.
 */
size_t Evaluator::binaryCompare(Array<double> pattern){
	if(pattern.ndim() > 2)
		throw MachineLearningException("Feature Array has two many dimensions, only two are allowed");

	if(pattern.ndim() == 1) {
		if((model.getParameterDimension() > 2) && (pattern.dim(0) != model.getInputDimension()))
			throw MachineLearningException("Feature Array has unexpected shape");

		// if two patterns are passed in one line of the array one after another, restructure them to do preprocessing
		pattern.resize(2, pattern.dim(0)/2, false);
	}



	if(pattern.ndim() == 2) {
		// apply the same transformations to the pattern to be tested as to the training dataset
		fp.transformData(pattern);

		if(model.getParameterDimension() <= 2) {
			pattern.resize(pattern.dim(1)*2);
			return eval_impl(pattern);
		}

		if(pattern.dim(1)*2 != model.getInputDimension() || pattern.dim(0) != 2)
			throw MachineLearningException("Feature Array has unexpected shape");
		pattern.resize(model.getInputDimension());
	}

	return eval_impl(pattern);
}

/*
 * Evaluates a pattern using the internal model
 */
size_t Evaluator::binaryCompare(const Array<double>& pattern1, const Array<double>& pattern2){
	if(pattern1.nelem() != pattern2.nelem())
		throw MachineLearningException("The two patterns to evaluate must have equal size");

	Array<double> pattern(pattern1);
	pattern.append_rows(pattern2);

	// apply the same transformations to the pattern to be tested as to the training dataset
	fp.transformData(pattern);

	pattern.resize(pattern1.nelem() + pattern2.nelem());

	return eval_impl(pattern);
}

Evaluator Evaluator::loadEvaluator(MyModel& tmpModel, const std::string& filename, const std::string& path){
	Array<double> tmpFeatureNormalization;
	std::string filePath = path + "/" + filename;
	// load model
	tmpModel.load((filePath + ".mod").c_str());

	// load the feature normalization parameters
	std::ifstream fnp;
	fnp.open(filePath + ".fnp", std::ios::in);

	if(!fnp.is_open())
		throw MachineLearningException("Cannot open " + filePath + ".fnp for reading");

	tmpFeatureNormalization.resize(0,0,false);
	std::string line, buf;
	size_t nFeatures;

	// read parameters from ssv, one line for each feature in forom: avgerage, min, max
	while(getline(fnp, line)) {
		nFeatures = 0;
		std::stringstream linestream(line);
		// read the four features
		while(linestream >> buf) {
			++nFeatures;
			tmpFeatureNormalization.append_elem(insieme::utils::numeric_cast<double>(buf));
		}

	}
	tmpFeatureNormalization.resize(4, nFeatures, false);

	return Evaluator(tmpModel, tmpFeatureNormalization);
}

} // end namespace ml
} // end namespace insieme
