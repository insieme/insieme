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

#include <float.h>

#include "insieme/machine_learning/cmd_line_utils.h"
#include "insieme/machine_learning/train.h"

#include "ReClaM/MeanSquaredError.h"
#include "ReClaM/Quickprop.h"
#include "ReClaM/createConnectionMatrix.h"
#include "ReClaM/EarlyStopping.h"
#include "ReClaM/ValidationError.h"

#include <iostream>

namespace ml {
	unsigned int Trainer::getMaximum() {
		try {
			std::stringstream qss;
			qss << "SELECT \n MAX(m.value) \n FROM measurement m \n";
			for(size_t i = 0; i < features.size(); ++i ) {
				qss << " JOIN data d" << i << " ON m.id=d" << i << ".mid AND d" << i << ".fid=" << features[i] << std::endl;
			}

			return pStmt->GetSqlResultInt(qss.str());
		} catch (Kompex::SQLiteException &exception)
		{
			const std::string err = "\nUnable to read maximum value of column xxx" ;
			std::cerr << err << std::endl;
			exception.Show();
			throw ml::MachineLearningException(err);
		}
		return 0;
	}

	double Trainer::sharkEarlyStopping(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, Array<double>& target, size_t validatonSize) {
		ValidationError ve(&errFct, &optimizer, 0, double(validatonSize)/100);

		return ve.error(model, in, target);
	}

	double Trainer::earlyStopping(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, Array<double>& target, size_t validatonSize) {
		size_t inDim = model.getInputDimension();
		size_t outDim = model.getOutputDimension();
		size_t n = in.dim(0) / inDim; // the number of trainin patterns
		size_t nVal = double(n) / 100 * validatonSize;
		size_t nTrain = n - nVal;

		// generate a vector containing the indices for all training patterns
		std::vector<size_t> trainIndices(n);
		for(size_t i = 0; i < n; ++i)
			trainIndices[i] = i;

		// copy validation patterns to a new array since they can be used always in the same order
		Array<double> valData, valTarget;
		for(size_t i = 0; i < nVal; ++i) {
			valData.append_elems(in.subarr(trainIndices[i]*inDim, (trainIndices[i]+1)*inDim-1));
			valTarget.append_elems(target.subarr(trainIndices[i]*outDim, (trainIndices[i]+1)*outDim-1));
		}
		trainIndices.resize(nTrain);

		double err = DBL_MAX;
		size_t cnt, striplen = 5;
//		Model* bestModel;
		EarlyStopping estop(striplen);//, worsen(1);
		size_t trainErr = 0, valErr = 0;

		for(int epoch = 0; epoch < 10000; ++epoch) {
			// permute training data
			std::random_shuffle(trainIndices.begin(), trainIndices.end());

			//perform online training
			for(std::vector<size_t>::const_iterator I = trainIndices.begin(); I != trainIndices.end(); ++I) {
				trainErr += optimizer.optimize(model, errFct, in.subarr(*I*inDim, (*I+1)*inDim-1), target.subarr(*I*outDim, (*I+1)*outDim-1));
			}

			trainErr /= nTrain;
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
			trainErr = 0;
		}

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
		Array<double> in, target;

		std::stringstream qss;
		qss << "SELECT \n m.id AS id, m.ts AS ts, \n";
		size_t n = features.size();
		for(size_t i = 0; i < n; ++i) {
			qss << " d" << i << ".value AS Feature" << i << ",\n";
		}
		qss << " m.value AS method FROM measurement m \n";
		for(size_t i = 0; i < n; ++i) {
			qss << " JOIN data d" << i << " ON m.id=d" << i << ".mid AND d" << i << ".fid=" << features[i] << std::endl;
		}

		std::string query = qss.str();

		try
		{
			unsigned int nClasses = model.getOutputDimension();

			pStmt->Sql(query);
			std::cout << "Queried Rows: " << pStmt->GetNumberOfRows() << std::endl;

			Array<double> oneOfN(nClasses);
			for(Array<double>::iterator I = oneOfN.begin(); I != oneOfN.end(); ++I) {
				*I = NEG;
			}

			//Train machine
			// fetch all results
			while(pStmt->FetchRow()){
				std::cout << "Result: " << pStmt->GetColumnName(2) << " " << pStmt->GetColumnName(3) << " " << pStmt->GetColumnName(4) << std::endl;
				std::cout << "Data:   " << pStmt->GetColumnInt(2) << " " << pStmt->GetColumnInt(3) << " " << pStmt->GetColumnInt(4) << std::endl;

				// construct training vectors
				for(size_t j = 2; j < 2+features.size(); ++j) {
					in.append_elem(pStmt->GetColumnDouble(j));
				}

				// translate index to one-of-n coding
				int theOne = pStmt->GetColumnInt(2+features.size());

				if(theOne >= nClasses){
					std::stringstream err;
					err << "Measurement value (" << theOne << ") is bigger than the number of the model's output dimension (" << nClasses << ")";
					std::cerr << err.str() << std::endl;
					throw ml::MachineLearningException(err.str());
				}

				oneOfN[theOne] = POS;
				target.append_elems(oneOfN);
				oneOfN[theOne] = NEG;
			}

			// reset the prepared statement
			pStmt->Reset();

			// do not forget to clean-up
			pStmt->FreeQuery();
		} catch (Kompex::SQLiteException &exception)
		{
			const std::string err = "\nQuery for data failed" ;
			std::cerr << err << std::endl;
			exception.Show();
			throw ml::MachineLearningException(err);
		}

		// do the actual training
		optimizer.init(model);

		double error = 0;
		if(iterations != 0)
			for(size_t i = 0; i < iterations; ++i)
				error = optimizer.optimize(model, errFct, in, target);
		else
			error = this->sharkEarlyStopping(optimizer, errFct, in, target, 10);

		return error;
	}
}

int main(int argc, char* argv[]) {
	CommandLineOptions::Parse(argc, argv);
	const std::string dbPath(CommandLineOptions::DataBase != std::string() ? CommandLineOptions::DataBase : std::string("data.db"));


	// Create a connection matrix with 2 inputs, 1 output
	// and a single, fully connected hidden layer with
	// 8 neurons:
	Array<int> con;
	createConnectionMatrix(con, 2, 8, 2);
	// declare Machine
	FFNet net = FFNet(2, 2, con);
	MeanSquaredError err;
	Array<double> in, target;
	Quickprop qprop;

	// create trainer
	ml::Trainer qpnn(dbPath, net);

	if(CommandLineOptions::FeatureNames.size() > 0)
		qpnn.setFeaturesByName(CommandLineOptions::FeatureNames);

	qpnn.setFeaturesByIndex(CommandLineOptions::Features);
	std::cout << "Error: " << qpnn.train(qprop, err, 0) << std::endl;

	return 0;
}
