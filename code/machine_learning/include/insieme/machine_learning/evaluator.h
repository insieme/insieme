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
 * evaluator.h
 *
 *  Created on: Dec 1, 2011
 *      Author: klaus
 */
#pragma once

#include "Array/Array.h"
//#include "ReClaM/MeanSquaredError.h"

#include "insieme/machine_learning/myModel.h"
#include "insieme/machine_learning/machine_learning_exception.h"
#include "insieme/machine_learning/feature_preconditioner.h"

namespace insieme {
namespace ml {

class Evaluator {
	MyModel& model;
	FeaturePreconditioner fp;
//	ErrorFunction* errFct;

	/**
	 * Evaluates a pattern using the internal model.
	 * @param pattern An Array holding the features of the pattern to be evaluated
	 * @param out An Array that will be filled with the output of the model using pattern
	 * @return the index of the winning class
	 */
	size_t eval_impl(Array<double>& pattern, Array<double>& out);


public:
	/**
	 * constructor to build an evaluator out of a given model/featureNormalization combination
	 */
	Evaluator(MyModel& model, Array<double>& featureNormalization)//, ErrorFunction* errorFunction = new MeanSquaredError())
		: model(model), fp(featureNormalization) {} //, errFct(errorFunction) { }

	/**
	 * destructor deleting errFct
	 */
/*	~Evaluator() {
		delete errFct;
	}
*/
	/**
	 * copy constructor
	 */
	Evaluator(const Evaluator& source): model(source.model), fp(source.fp) { }

	/**
	 * Evaluates a pattern using the internal model.
	 * @param pattern An Array holding the features of the pattern to be evaluated
	 * @return the index of the winning class
	 */
	size_t evaluate(Array<double> pattern);

	/**
	 * Evaluates a pattern using the internal model.
	 * @param pattern An Array holding the features of the pattern to be evaluated
	 * @param out An Array that will be filled with the output of the model using pattern
	 * @return the index of the winning class
	 */
	size_t evaluate(Array<double> pattern, Array<double>& out);

	/**
	 * Evaluates a pattern using the internal model
	 * WARNING size of pointer is not checked
	 * @param pattern A C pointer holding the features of the pattern to be evaluated
	 * @param nElems The number of elements in the pointer
	 * @return the index of the winning class
	 */
	size_t evaluate(const double* pattern, size_t nElems);

	/**
	 * Evaluates a pattern using the internal model
	 * WARNING size of pointer is not checked
	 * @param pattern A C pointer holding the features of the pattern to be evaluated
	 * @return the index of the winning class
	 */
	size_t evaluate(const double* pattern);


	/**
	 * Compares two patterns using the internal model.
	 * @param pattern An Array with two rows, each of them holding the features of one pattern to be evaluated or
	 *         holding the features of the two patterns one after another
	 * @return the index of the winning class
	 */
	size_t binaryCompare(Array<double> pattern);

	/**
	 * Compares two patterns using the internal model.
	 * @param pattern1 An Array holding the features of first the pattern to be evaluated
	 * @param pattern2 An Array holding the features of second the pattern to be evaluated
	 * @return the index of the winning class
	 */
	size_t binaryCompare(const Array<double>& pattern1, const Array<double>& pattern2);

	/**
	 * generates an evaluator out of a .mod and .fnp file from disk and a model instance. +
	 * The user is responsible that all these three components match
	 * @param filename The name of the files to load the data from. The appropriate file extension will be added automatically
	 * @param path The path were to load the two files from, the current directory is the default
	 * @return the loaded evaluator
	 */
	static Evaluator loadEvaluator(MyModel& model, const std::string& filename, const std::string& path = ".") throw(MachineLearningException);
};

} // end namespace ml
} // end namespace insieme
