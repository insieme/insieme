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

//#include <float.h>

#include "insieme/machine_learning/train.h"

#include "ReClaM/ValidationError.h"
#include "ReClaM/EarlyStopping.h"

#include <iostream>

namespace ml {
	double Trainer::getMaximum(const std::string& param) {
		try {
			std::stringstream qss;
			qss << "SELECT \n MAX(m." << param << ") \n FROM measurement m \n";
			for(size_t i = 0; i < features.size(); ++i ) {
				qss << " JOIN data d" << i << " ON m.id=d" << i << ".mid AND d" << i << ".fid=" << features[i] << std::endl;
			}

			return pStmt->GetSqlResultDouble(qss.str());
		} catch (Kompex::SQLiteException &exception)
		{
			std::stringstream err;
			err << "\nUnable to read maximum value of column " << param ;
			std::cerr << err << std::endl;
			exception.Show();
			throw ml::MachineLearningException(err.str());
		}
		return 0;
	}

	size_t Trainer::valToOneOfN(Kompex::SQLiteStatement* stmt, size_t index, double max) {
		switch(genOut) {
		case GenNNoutput::ML_KEEP_INT :
			return stmt->GetColumnInt(index);
		case GenNNoutput::ML_MAP_FLOAT_LIN:
			if(stmt->GetColumnDouble(index) == max) return model.getOutputDimension()-1;
			return (stmt->GetColumnDouble(index) / max) * model.getOutputDimension();
		default:
			throw MachineLearningException("Requested output generation not defined");
		}
	}

	double Trainer::sharkEarlyStopping(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, Array<double>& target, size_t validatonSize) {
		ValidationError ve(&errFct, &optimizer, 0, double(validatonSize)/100);

		return ve.error(model, in, target);
	}

	double Trainer::earlyStopping(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, Array<double>& target, size_t validatonSize) {
		size_t n = in.dim(0); // the number of training patterns
		size_t nVal = double(n) / 100 * validatonSize;
		size_t nTrain = n - nVal;

		// generate a vector containing the indices for all training patterns
		std::vector<size_t> trainIndices(n);
		for(size_t i = 0; i < n; ++i)
			trainIndices[i] = i;

		// copy validation patterns to a new array since they can be used always in the same order
		Array<double> valData, valTarget;
		for(size_t i = 0; i < nVal; ++i) {
			valData.append_rows(in.subarr(trainIndices[i],trainIndices[i])[0]);
			valTarget.append_rows(target.subarr(trainIndices[i], trainIndices[i])[0]);
		}
		trainIndices.resize(nTrain);

//		double err = DBL_MAX;
//		size_t cnt = 0;
		size_t striplen = 5;
//		Model* bestModel;
		EarlyStopping estop(striplen);//, worsen(1);
		size_t trainErr = 0, valErr = 0;
		trainErr = 0;

		for(int epoch = 0; epoch < 100; ++epoch) {
			// permute training data
			std::random_shuffle(trainIndices.begin(), trainIndices.end());

			//perform online training
/*			for(std::vector<size_t>::const_iterator I = trainIndices.begin(); I != trainIndices.end(); ++I) {
				trainErr += optimizer.optimize(model, errFct, in.subarr(*I*inDim, (*I+1)*inDim-1), target.subarr(*I*outDim, (*I+1)*outDim-1));
			}
*/
			optimizer.optimize(model, errFct, in, target);
			trainErr = errFct.error(model, in, target);
			valErr = errFct.error(model, valData, valTarget);
/*
 	 	 	 implement rollback only if needed
			worsen.update(trainErr, valErr);
			if(!worsen.one_of_all( 1.0, 1.0, 1.0, 3)) {
				Mode;
			}
*/
			estop.update(trainErr, valErr);
			if(estop.one_of_all( 1.0, 1.0, 1.0, 3)) {
				std::cout << "Early stopping after " << epoch << " iterations\n";
			}
		}

		std::cout << "Train error " << trainErr << std::endl;
		return valErr;
	}


	void Trainer::setFeaturesByIndex(const std::vector<std::string>& featureIndices) {
		for(std::vector<std::string>::const_iterator I = featureIndices.begin(); I != featureIndices.end(); ++I)
			features.push_back(*I);
	}
	void Trainer::setFeatureByIndex(const std::string featureIndex) {
		features.push_back(featureIndex);
	}

	void Trainer::setFeaturesByName(const std::vector<std::string>& featureNames){
		for(std::vector<std::string>::const_iterator I = featureNames.begin(); I != featureNames.end(); ++I) {
			setFeatureByName(*I);
		}
	}
	void Trainer::setFeatureByName(const std::string featureName){
		// build query for name
		std::string tmp;
		try {
			std::stringstream qss;
			qss << "SELECT id FROM features f WHERE f.name = \"" << featureName << "\"";

			// query for the index of that name
			tmp = pStmt->GetSqlResultString(qss.str());

			// store feature index in field
			features.push_back(tmp);
		} catch(Kompex::SQLiteException &exception)
		{
			tmp = "";
		}
		if(tmp == "") {
			std::string err = "\nCannot find feature " + featureName;
			std::cerr << err << std::endl;
			throw ml::MachineLearningException(err);
		}
	}

	double Trainer::train(Optimizer& optimizer, ErrorFunction& errFct, size_t iterations) {
		if(features.size() != model.getInputDimension())
			throw MachineLearningException("Number of selected features is not equal to the model's input size");

		std::stringstream qss;
		qss << "SELECT \n m.id AS id, m.ts AS ts, \n";
		size_t n = features.size();
		for(size_t i = 0; i < n; ++i) {
			qss << " d" << i << ".value AS Feature" << i << ",\n";
		}
		qss << " m.time AS method FROM measurement m \n";
		for(size_t i = 0; i < n; ++i) {
			qss << " JOIN data d" << i << " ON m.id=d" << i << ".mid AND d" << i << ".fid=" << features[i] << std::endl;
		}

		std::string query = qss.str();
		double error = 0;

		try
		{
			// read the maximum of the column in measurement for which to train
			double max = getMaximum("time");

			Kompex::SQLiteStatement *localStmt = new Kompex::SQLiteStatement(pDatabase);
			unsigned int nClasses = model.getOutputDimension();

			localStmt->Sql(query);

			Array<double> in(localStmt->GetNumberOfRows(), model.getInputDimension()), target;
			std::cout << "Queried Rows: " << localStmt->GetNumberOfRows() << ", Number of features: " << n << std::endl;

			Array<double> oneOfN(nClasses);
			for(Array<double>::iterator I = oneOfN.begin(); I != oneOfN.end(); ++I) {
				*I = NEG;
			}

			//Train machine
			size_t i = 0;
			// fetch all results
			while(localStmt->FetchRow()){
//				std::cout << "Result: " << localStmt->GetColumnName(2) << " " << localStmt->GetColumnName(3) << " " << localStmt->GetColumnName(4) << std::endl;
//				std::cout << "Data:   " << localStmt->GetColumnInt(2) << " " << localStmt->GetColumnInt(3) << " " << localStmt->GetColumnInt(4) << std::endl;

				// construct training vectors
//				for(size_t j = 2; j < 2+features.size(); ++j) {
//					in(i, j) = localStmt->GetColumnDouble(j);
//				}

				//FIXME remove manual normalization
				in(i, 0) = (localStmt->GetColumnDouble(2) / 1.6 - 5);
				in(i, 1) = (localStmt->GetColumnDouble(3) / 25.5 - 5);
				in(i, 2) = (localStmt->GetColumnDouble(4) / 25.5 - 5);
				in(i, 3) = (localStmt->GetColumnDouble(5) / 6.4 - 5);

				// translate index to one-of-n coding
				size_t theOne = valToOneOfN(localStmt, 2+features.size(), max);

				if(theOne >= nClasses){
					std::stringstream err;
					err << "Measurement value (" << theOne << ") is bigger than the number of the model's output dimension (" << nClasses << ")";
					std::cerr << err.str() << std::endl;
					throw ml::MachineLearningException(err.str());
				}

				oneOfN[theOne] = POS;
				target.append_rows(oneOfN);
				oneOfN[theOne] = NEG;
				++i;
			}

			// reset the prepared statement
			localStmt->Reset();

			// do not forget to clean-up
			localStmt->FreeQuery();
			delete localStmt;

		// do the actual training
		optimizer.init(model);


	//		for(Array<double>::iterator I = in.begin(); I != in.end(); ++I) {
	//			*I = (*I / (255/10)) - 5;
	//		}
	//std::cout << target << std::endl;
			if(iterations != 0) {
				for(size_t i = 0; i < iterations; ++i)
					optimizer.optimize(model, errFct, in, target);
				error = errFct.error(model, in, target);
			}
			else
				error = this->earlyStopping(optimizer, errFct, in, target, 10);

		} catch (Kompex::SQLiteException &exception)
		{
			const std::string err = "\nQuery for data failed" ;
			std::cerr << err << std::endl;
			exception.Show();
			throw ml::MachineLearningException(err);
		}
		return error;
	}
}

