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
 * binary_compare_trainer.h
 *
 *  Created on: Nov 26, 2011
 *      Author: klaus
 */

#pragma once

#include "insieme/machine_learning/trainer.h"

namespace insieme {
namespace ml {

class BinaryCompareTrainer : public Trainer {

	/**
	 * Generates the cross product of the input dataset by aggregating always two datasets to a feature vector of size 2*number-of-features
	 * The generated dataset is stored in crossProduct which will have the size (nPatterns*nPatterns)-nPatterns by 2*numberOfFeatures
	 * @param in Array holding the features read form the database
	 * @param crossProduct Array in which the cross product of in will be stored
	 * @param measurements Array holding the measurment values for each pattern
	 * @param target Array which is filled with a one-of-2 coding, according to the patterns and the measurements
	 * @param outDim the output dimension of the network, must be 0 or 1
	 */
	void generateCrossProduct(const Array<double>& in, Array<double>& crossProduct, const Array<double>& measurements, Array<double>& target, size_t outDim);

	/**
	 * Reads an entry for the training values form the database and appends it to the Array target
	 * @param target an Array where the target values should be written to
	 * @param stmt An sql statement holdin the line with de desired target value
	 * @param queryIdx the index of the desired target value in the sql statement
	 * @param max ignored
	 * @param min ignored
	 * @param oneOfN ignored
	 */
	virtual void appendToTrainArray(Array<double>& target, Kompex::SQLiteStatement* stmt, size_t queryIdx, double max, double min, Array<double>& oneOfN);

	/**
	 * Reads an entry for the training values form the database and appends it to the Array target
	 * @param target an Array where the target values should be written to
	 * @param stmt An sql statement holdin the line with de desired target value
	 * @param queryIdx the index of the desired target value in the sql statement
	 */
	virtual void appendToTrainArray(Array<double>& target, Kompex::SQLiteStatement* localStmt, size_t queryIdx) {
		Array<double> array;
		appendToTrainArray(target, localStmt, queryIdx, 0.0, 0.0, array);
	}


public:
	BinaryCompareTrainer(const std::string& myDbPath, MyModel& myModel) : Trainer(myDbPath, myModel, GenNNoutput::ML_KEEP_INT) {}

	/**
	 * trains the model using the patterns returned by the given query or the default query if none is given
	 * @param optimizer the Shark Optimizer to be used, eg. Quickprop, Bfgs etc.
	 * @param errFct the Shark error function to be used, eg. MeanSquaredError
	 * @param in Array that will be filled with the training data read form the database
	 * @param iterations the number of training operations to perform. If a number >0 is given, the trainer performs this
	 * number of training iterations on the whole dataset and returns the error on it. If 0 is passed, the trainer
	 * will use a customized early stopping approach:
	 * - splits data in training and validation set in ratio 10:1 randomly
	 * - training is only done on training set
	 * - maximum of training iterations is set to 1000
	 * - stopping training earlier if there is no improvement for 5 iterations
	 * - the returned error is the one on the validation set only (the error on the training set and classification
	 *   error on the validation set is printed to LOG(INFO)
	 * in the reference of an Array where the input, read from the database, is stored
	 * @return the error after training
	 */
	virtual double train(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, size_t iterations = 1) throw(MachineLearningException);

	/**
	 * trains the model using the patterns returned by the given query or the default query if none is given
	 * @param optimizer the Shark Optimizer to be used, eg. Quickprop, Bfgs etc.
	 * @param errFct the Shark error function to be used, eg. MeanSquaredError,
	 * @param iterations the number of training operations to perform. If a number >0 is given, the trainer performs this
	 * number of training iterations on the whole dataset and returns the error on it. If 0 is passed, the trainer
	 * will use a customized early stopping approach:
	 * - splits data in training and validation set in ration 10:1 randomly
	 * - training is only done on training set
	 * - maximum of training iterations is set to 1000
	 * - stopping training earlier if there is no improvement for 5 iterations
	 * - the returned error is the one on the validation set only (the error on the training set and classification
	 *   error on the validation set is printed to LOG(INFO)
	 * @return the error after training
	 */
	virtual double train(Optimizer& optimizer, ErrorFunction& errFct, size_t iterations = 0) throw(MachineLearningException);

	/**
	 * Reads data form the database according to the current query, tests all patterns with the current model
	 * and returns the error according to the error function
	 * @param errFct the error function to be used
	 * @return the error calculated with the given error function
	 */
	virtual double evaluateDatabase(ErrorFunction& errFct) throw(MachineLearningException);

	/**
	 * Compares two patterns using the internal model.
	 * @param pattern An Array with two rows, each of them holding the features of one pattern to be evaluated or
	 *         holding the features of the two patterns one after another
	 * @return the index of the winning class
	 */
	virtual size_t evaluate(Array<double>& pattern);

	/**
	 * Compares two patterns using the internal model.
	 * @param pattern1 An Array holding the features of first the pattern to be evaluated
	 * @param pattern2 An Array holding the features of second the pattern to be evaluated
	 * @return the index of the winning class
	 */
	virtual size_t evaluate(const Array<double>& pattern1, const Array<double>& pattern2);

};

} // end namespace ml
} // end namespace insieme
