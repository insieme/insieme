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
#include <math.h>
#include <iostream>
#include <algorithm>

#include "ReClaM/ValidationError.h"
#include "ReClaM/EarlyStopping.h"

#include "insieme/machine_learning/trainer.h"
#include "insieme/machine_learning/feature_preconditioner.h"
#include "insieme/utils/logging.h"

namespace insieme {
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
			LOG(ERROR) << err << std::endl;
			exception.Show();
			throw ml::MachineLearningException(err.str());
		}
		return 0;
	}

	double Trainer::getMinimum(const std::string& param) {
		try {
			std::stringstream qss;
			qss << "SELECT \n MIN(m." << param << ") \n FROM measurement m \n";
			for(size_t i = 0; i < features.size(); ++i ) {
				qss << " JOIN data d" << i << " ON m.id=d" << i << ".mid AND d" << i << ".fid=" << features[i] << std::endl;
			}

			return pStmt->GetSqlResultDouble(qss.str());
		} catch (Kompex::SQLiteException &exception)
		{
			std::stringstream err;
			err << "\nUnable to read maximum value of column " << param ;
			LOG(ERROR) << err << std::endl;
			exception.Show();
			throw ml::MachineLearningException(err.str());
		}
		return 0;
	}
	size_t s;

	/*
	 * Converts the value read from the database to an index in one of n coding, according to the policy defined in the variable genOut.
	 * The returned should be set to POS, the rest to NEG
	 */
	size_t Trainer::valToOneOfN(Kompex::SQLiteStatement* stmt, size_t index, double max, double min) {
		switch(genOut) {
		case GenNNoutput::ML_KEEP_INT :
			return stmt->GetColumnInt(index);
		case GenNNoutput::ML_MAP_FLOAT_LIN:
			if(stmt->GetColumnDouble(index) == max) return model.getOutputDimension()-1;
			return ((stmt->GetColumnDouble(index)-min) / (max-min)) * model.getOutputDimension();
		case GenNNoutput::ML_MAP_FLOAT_LOG:
			if(stmt->GetColumnDouble(index) == min) return 0;
			if(stmt->GetColumnDouble(index) == max) return model.getOutputDimension()-1;
			return (log(stmt->GetColumnDouble(index) - min) / log(max-min) ) * model.getOutputDimension();
		case GenNNoutput::ML_MAP_FLOAT_HYBRID:
			if(stmt->GetColumnDouble(index) == min) return 0;
			if(stmt->GetColumnDouble(index) == max) return model.getOutputDimension()-1;
			return fabs( ((log(stmt->GetColumnDouble(index) - min) + (stmt->GetColumnDouble(index) - min)) / (log(max-min)  + (max-min) ))
					+ (log(stmt->GetColumnDouble(index) - min) / log(max-min) ) ) * 0.5 * model.getOutputDimension();
		default:
			throw MachineLearningException("Requested output generation not defined");
		}
	}

	/*
	 * Returns the index of the maximum of all elements in coded
	 */
	size_t Trainer::oneOfNtoIdx(Array<double> coded) {
		double max = 0.0;
		size_t theOne = 0.0;
		for(size_t i = 0; i < coded.nelem(); ++i) {
			if(max < coded(i)) {
				max = coded(i);
				theOne = i;
			}
		}

		return theOne;
	}


	double Trainer::earlyStopping(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, Array<double>& target, size_t validatonSize) {
		ValidationError ve(&errFct, &optimizer, 1000, double(validatonSize)/100);

		return ve.error(model, in, target);
	}

	double Trainer::myEarlyStopping(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, Array<double>& target, size_t validatonSize,
			size_t nBatches) {
		size_t n = in.dim(0); // the number of training patterns
		size_t nVal = double(n) / 100 * validatonSize;
		size_t nTrain = n - nVal;

		// generate a vector containing the indices for all training patterns
		std::vector<size_t> trainIndices(n);
		for(size_t i = 0; i < n; ++i)
			trainIndices[i] = i;

		std::random_shuffle(trainIndices.begin(), trainIndices.end());
		// copy validation patterns to a new array since they can be used always in the same order
		Array<double> valData, valTarget;
		for(size_t i = 0; i < nVal; ++i) {
			valData.append_rows(in.subarr(trainIndices[i],trainIndices[i])[0]);
			valTarget.append_rows(target.subarr(trainIndices[i], trainIndices[i])[0]);
		}

		// create one Array for each batch
		size_t batchsize = nTrain/nBatches;
		size_t bulge = nTrain%nBatches;

		std::vector<Array<double>> trainBatchesData, trainBatchesTarget;
		trainBatchesData.reserve(nBatches);
		trainBatchesTarget.reserve(nBatches);
		size_t cnt = nVal;
		for(size_t i = 0; i < nBatches; ++i) {
			trainBatchesData.push_back(in.subarr(trainIndices[cnt],trainIndices[cnt]));
			trainBatchesTarget.push_back(target.subarr(trainIndices[cnt], trainIndices[cnt]));
			++cnt;
			for(size_t j = 1; j < batchsize + ((i < bulge) ? 1 : 0); ++j) {
				trainBatchesData[i].append_rows(in.subarr(trainIndices[cnt],trainIndices[cnt])[0]);
				trainBatchesTarget[i].append_rows(target.subarr(trainIndices[cnt], trainIndices[cnt])[0]);
				++cnt;
			}
		}

		trainIndices.resize(nBatches);
		for(size_t i = 0; i < nBatches; ++i)
			trainIndices[i] = i;

//		double err = DBL_MAX;
//		size_t cnt = 0;
		size_t striplen = 5;
//		Model* bestModel;
		EarlyStopping estop(striplen);//, worsen(1);
		double trainErr = 0, valErr = 0;

		for(int epoch = 0; epoch < 1000; ++epoch) {
			// permute training data
			std::random_shuffle(trainIndices.begin(), trainIndices.end());
			trainErr = 0;

			//perform online training
			for(size_t i = 0; i < nBatches; ++i) {
				optimizer.optimize(model, errFct, trainBatchesData[trainIndices[i]], trainBatchesTarget[trainIndices[i]]);
				trainErr += errFct.error(model, trainBatchesData[trainIndices[i]], trainBatchesTarget[trainIndices[i]]);
			}

			trainErr /= nBatches;
//			trainErr = errFct.error(model, in, target);
			valErr = errFct.error(model, valData, valTarget);
//			std::cout << epoch << ": " << trainErr << " - " << valErr << std::endl;
/*
 	 	 	 implement rollback only if needed
			worsen.update(trainErr, valErr);
			if(!worsen.one_of_all( 1.0, 1.0, 1.0, 3)) {
				Mode;
			}
*/
			estop.update(trainErr, valErr);
//			std::cout << "GL " << estop.GL(12.0) << "\nTP " << estop.TP(0.5) << "\nPQ " <<  estop.PQ(15.0) << "\nUP " << estop.UP(5) << std::endl;
			if(estop.one_of_all(12.0, 0.5, 15.0, 5)) {
				LOG(INFO) << "Early stopping after " << epoch << " iterations\n";
				break;
			}
		}

		LOG(INFO) << "Train error " << trainErr << std::endl;
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
			LOG(ERROR) << err << std::endl;
			throw ml::MachineLearningException(err);
		}
	}

	double Trainer::train(Optimizer& optimizer, ErrorFunction& errFct, size_t iterations) {
		if(features.size() != model.getInputDimension()) {
			std::cerr << "Number of features: " << features.size() << "\nModel input size: " << model.getInputDimension() << std::endl;
			throw MachineLearningException("Number of selected features is not equal to the model's input size");
		}

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
			// read the maximum of the column in measurement for which to train
			double max = getMaximum(trainForName), min = getMinimum(trainForName);

			Kompex::SQLiteStatement *localStmt = new Kompex::SQLiteStatement(pDatabase);
			unsigned int nClasses = model.getOutputDimension();

			localStmt->Sql(query);

			size_t nRows = localStmt->GetNumberOfRows();
			Array<double> in(nRows, model.getInputDimension()), target;
			LOG(INFO) << "Queried Rows: " << nRows << ", Number of features: " << n << std::endl;
			if(nRows == 0)
				throw MachineLearningException("No dataset for the requested features could be found");

			std::list<std::pair<double, size_t> > measurements;

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
				for(size_t j = 0; j < features.size(); ++j) {
					in(i, j) = localStmt->GetColumnDouble(j+2);
				}

				// translate index to one-of-n coding
				if(genOut == ML_MAP_TO_N_CLASSES)
					measurements.push_back(std::make_pair(localStmt->GetColumnDouble(2+features.size()), i));
				else {
					size_t theOne = valToOneOfN(localStmt, 2+features.size(), max, min);

					if(theOne >= nClasses){
						std::stringstream err;
						err << "Target value (" << theOne << ") is bigger than the number of the model's output dimension (" << nClasses << ")";
						LOG(ERROR) << err.str() << std::endl;
						throw ml::MachineLearningException(err.str());
					}

					oneOfN[theOne] = POS;
					target.append_rows(oneOfN);
					oneOfN[theOne] = NEG;
				}
				++i;
			}


			FeaturePreconditioner fp(in);
			fp.normalize(-1, 1);

			if(genOut == ML_MAP_TO_N_CLASSES)
				fp.mapToNClasses(measurements, model.getOutputDimension(), NEG, POS, target);

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
				error = this->myEarlyStopping(optimizer, errFct, in, target, 10, std::max(static_cast<size_t>(1),nRows/1000));

			Array<double> out(5);
			size_t misClass = 0;
			for(size_t i = 0; i < in.dim(0); ++i ) {
				model.model(in.subarr(i,i), out);
//				std::cout << target.subarr(i,i) << out << std::endl;
//				std::cout << oneOfNtoIdx(target.subarr(i,i)) << " - " <<  oneOfNtoIdx(out) << std::endl;
				if(oneOfNtoIdx(target.subarr(i,i)) != oneOfNtoIdx(out))
					++misClass;
			}
			LOG(INFO) << "Misclassification rate: " << (double(misClass)/in.dim(0)) * 100.0 << "%\n";

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

