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
#include <fstream>

#include "ReClaM/ValidationError.h"
#include "ReClaM/EarlyStopping.h"

#include "ReClaM/EarlyStopping.h"

#include "ReClaM/BFGS.h"
#include "ReClaM/CG.h"
#include "ReClaM/Rprop.h"
#include "ReClaM/Quickprop.h"
#include "ReClaM/MeanSquaredError.h"
#include "ReClaM/Svm.h"
#include "ReClaM/ClassificationError.h"

#include "insieme/machine_learning/trainer.h"
#include "insieme/machine_learning/feature_preconditioner.h"
#include "insieme/machine_learning/evaluator.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"

namespace insieme {
namespace ml {


// early stopping parameters
#define GL 120.0
#define TP 0.05
#define PQ 150.0
#define UP 50


namespace {
/*
 * compares the first element in a pair of <double, size_t>
 */
bool pairCompare(std::pair<double, size_t> a, std::pair<double, size_t> b) {
	if(a.first < b.first)
		return true;
	return false;
}


const std::string getName(const Optimizer* optimizer) {
	if(dynamic_cast<const BFGS*>(optimizer) != 0)
		return "BFGS";

	if(dynamic_cast<const CG*>(optimizer) != 0)
		return "CG";

	if(dynamic_cast<const RpropMinus*>(optimizer) != 0)
		return "Rprop-";

	if(dynamic_cast<const RpropPlus*>(optimizer) != 0)
		return "Rprop+";

	if(dynamic_cast<const Quickprop*>(optimizer) != 0)
		return "Quickprop";

	if(dynamic_cast<const SVM_Optimizer*>(optimizer) != 0)
		return "SVM Optimizer";

	assert(false && "getName not implemented for this Optimizer");

	return "eieieieiei";
}

const std::string getName(const ErrorFunction* errFct) {
	if(dynamic_cast<const MeanSquaredError*>(errFct) )
		return "MeanSquaredError";

	if(dynamic_cast<const ClassificationError*>(errFct) )
		return "ClassificationError";

	assert(false && "getName not implemented for this Error Function");

	return "eieieieiei";
}

} // end anonymous namespace


double Trainer::getMaximum(const std::string& param) {
	try {
		std::stringstream qss;
		qss << "SELECT \n MAX(m." << param << ") \n FROM measurement m \n";
		for(size_t i = 0; i < staticFeatures.size(); ++i ) {
			qss << " JOIN code c" << i << " ON m.cid=c" << i << ".cid AND c" << i << ".fid=" << staticFeatures[i] << std::endl;
		}
		for(size_t i = 0; i < dynamicFeatures.size(); ++i ) {
			qss << " JOIN setup s" << i << " ON m.sid=s" << i << ".sid AND s" << i << ".fid=" << dynamicFeatures[i] << std::endl;
		}
		for(size_t i = 0; i < pcaFeatures.size(); ++i ) {
			qss << " JOIN principal_component p" << i << " ON m.pid=p" << i << ".pid AND p" << i << ".fid=" << pcaFeatures[i] << std::endl;
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
		for(size_t i = 0; i < staticFeatures.size(); ++i ) {
			qss << " JOIN code c" << i << " ON m.cid=c" << i << ".cid AND c" << i << ".fid=" << staticFeatures[i] << std::endl;
		}
		for(size_t i = 0; i < dynamicFeatures.size(); ++i ) {
			qss << " JOIN setup s" << i << " ON m.sid=s" << i << ".sid AND s" << i << ".fid=" << dynamicFeatures[i] << std::endl;
		}
		for(size_t i = 0; i < pcaFeatures.size(); ++i ) {
			qss << " JOIN principal_component p" << i << " ON m.pid=p" << i << ".pid AND p" << i << ".fid=" << pcaFeatures[i] << std::endl;
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
	// handle case of one single output
	if(coded.nelem() == 1)
		return coded(0) > static_cast<double>(POS + NEG)/2.0 ? 1 : 0;

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


/*
 * puts the first numPatterns/n patterns in the first class, the second in the second etc
 */
void Trainer::mapToNClasses(std::list<std::pair<double, size_t> >& measurements, size_t n, double neg, double pos, Array<double>& target) {
	measurements.sort(pairCompare);

	size_t elemPerClass = measurements.size()/n;
	size_t bulge = measurements.size()%n;

	size_t curr = 0;
	size_t theOne = 0;

	// allocate memor for target
	target = Array<double>(measurements.size(), n);

	for(std::list<std::pair<double, size_t> >::iterator I = measurements.begin(); I != measurements.end(); ++I) {
		if(curr == (elemPerClass + ((theOne < bulge) ? 1 : 0))) {
			++theOne;
			curr = 0;
		}
		for(size_t i = 0; i < n; ++i){
			target(I->second, i) = (i == theOne ? pos : neg);		}

		++curr;
	}
}

/**
 * writes informations about the current training run to a stream
 */
void Trainer::writeHeader(const std::string trainer, const Optimizer& optimizer, const ErrorFunction& errFct, const size_t iterations) const {
	out << trainer << ", Targets: " << NEG << " - " << POS << std::endl;
	out << model.getType()    << model.getStructure() << std::endl;
	if(model.iterativeTraining()) {
		if(iterations > 0)
			out << iterations << " training iterations\n";
		else
			out << "Early stopping params (GL, TP, PQ, UP): " << GL << TP << 150.0 << 50 << std::endl;
	}
	out << "Init Interval:  (" << model.getInitInterval().first << ", " << model.getInitInterval().second << ")" << std::endl;
	out << "Optimizer:      " << getName(&optimizer) << std::endl;
	out << "Error Function: " << getName(&errFct) << std::endl;
	out << "Database:       " << dbPath << std::endl;
	if(staticFeatures.size() > 0) {
		out << "Static Features:\n";
		for(std::vector<std::string>::const_iterator I = staticFeatures.begin(); I != staticFeatures.end(); ++I) {
			// query for the name of that used features
			std::stringstream qss;
			qss << "SELECT name FROM static_features f WHERE f.id = \"" << *I << "\"";
			out << "\t" << *I << " " << pStmt->GetSqlResultString(qss.str()) << std::endl;
		}
	}
	if(dynamicFeatures.size() > 0) {
		out << "Dynamic Features:\n";
		for(std::vector<std::string>::const_iterator I = dynamicFeatures.begin(); I != dynamicFeatures.end(); ++I) {
			// query for the name of that used features
			std::stringstream qss;
			qss << "SELECT name FROM dynamic_features f WHERE f.id = \"" << *I << "\"";
			out << "\t" << *I << " " << pStmt->GetSqlResultString(qss.str()) << std::endl;
		}
	}
	if(pcaFeatures.size() > 0) {
		out << "PCA Features:\n";
		for(std::vector<std::string>::const_iterator I = pcaFeatures.begin(); I != pcaFeatures.end(); ++I) {
			// query for the name of that used features
			std::stringstream qss;
			qss << "SELECT name FROM pca_features f WHERE f.id = \"" << *I << "\"";
			out << "\t" << *I << " " << pStmt->GetSqlResultString(qss.str()) << std::endl;
		}
	}
	out << std::endl;

// get table name	out << pStmt->GetSqlResultString("PRAGMA table_info('features')") << std::endl;
}


/**
 * writes the current iteration and error on the dataset to a stream
 */
void Trainer::writeStatistics(size_t iteration, Array<double>& in, Array<double>& target, ErrorFunction& errFct) {
	out << iteration << " " << errFct.error(model.getModel(), in, target) << std::endl;;
}

double Trainer::earlyStopping(Optimizer& Optimizer, ErrorFunction& errFct, Array<double>& in, Array<double>& target, size_t validatonSize) {
	ValidationError ve(&errFct, &Optimizer, 1000, double(validatonSize)/100);

	return ve.error(model.getModel(), in, target);
}

double Trainer::myEarlyStopping(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, Array<double>& target, size_t validationSize,
		size_t nBatches) {
	size_t n = in.dim(0); // the number of training patterns
	size_t nVal = double(n) / 100 * validationSize;
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
//		MyModel* bestModel;
	EarlyStopping estop(striplen);//, worsen(1);
	double trainErr = 0, valErr = 0;

	for(int epoch = 0; epoch < 1000; ++epoch) {
		// permute training data
		std::random_shuffle(trainIndices.begin(), trainIndices.end());
		trainErr = 0;

		//perform online training
		for(size_t i = 0; i < nBatches; ++i) {
			optimizer.optimize(model.getModel(), errFct, trainBatchesData[trainIndices[i]], trainBatchesTarget[trainIndices[i]]);
			trainErr += errFct.error(model.getModel(), trainBatchesData[trainIndices[i]], trainBatchesTarget[trainIndices[i]]);
		}

		trainErr /= nBatches;
//			trainErr = errFct.error(model, in, target);
		valErr = errFct.error(model.getModel(), valData, valTarget);
//			std::cout << epoch << ": " << trainErr << " - " << valErr << std::endl;
/*
		 implement rollback only if needed
		worsen.update(trainErr, valErr);
		if(!worsen.one_of_all( 1.0, 1.0, 1.0, 3)) {
			Mode;
		}
*/
		if(TRAINING_OUTPUT)
			writeStatistics(epoch, in, target, errFct);

		estop.update(trainErr, valErr);
//			std::cout << "GL " << estop.GL(12.0) << "\nTP " << estop.TP(0.5) << "\nPQ " <<  estop.PQ(15.0) << "\nUP " << estop.UP(5) << std::endl;
		if(estop.one_of_all(GL, TP, PQ, UP)) {
			LOG(INFO) << "Early stopping after " << epoch << " iterations\n";
			break;
		}
	}

	LOG(INFO) << "Train error " << trainErr << std::endl;
	return valErr;
}


void Trainer::setStaticFeaturesByIndex(const std::vector<std::string>& featureIndices) {
	assert(pcaFeatures.size() == 0 && "Cannot use PCA and 'normal' features at the same time");
	for(std::vector<std::string>::const_iterator I = featureIndices.begin(); I != featureIndices.end(); ++I)
		staticFeatures.push_back(*I);
}
void Trainer::setStaticFeatureByIndex(const std::string featureIndex) {
	assert(pcaFeatures.size() == 0 && "Cannot use PCA and 'normal' features at the same time");
	staticFeatures.push_back(featureIndex);
}

void Trainer::setStaticFeaturesByName(const std::vector<std::string>& featureNames){
	for(std::vector<std::string>::const_iterator I = featureNames.begin(); I != featureNames.end(); ++I) {
		setStaticFeatureByName(*I);
	}
}

void Trainer::setStaticFeatureByName(const std::string featureName){
	assert(pcaFeatures.size() == 0 && "Cannot use PCA and 'normal' features at the same time");
	// build query for name
	std::string tmp;
	try {
		std::stringstream qss;
		qss << "SELECT id FROM static_features f WHERE f.name = \"" << featureName << "\"";

		// query for the index of that name
		tmp = pStmt->GetSqlResultString(qss.str());

		// store feature index in field
		staticFeatures.push_back(tmp);
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

void Trainer::setDynamicFeaturesByIndex(const std::vector<std::string>& featureIndices) {
	assert(pcaFeatures.size() == 0 && "Cannot use PCA and 'normal' features at the same time");
	for(std::vector<std::string>::const_iterator I = featureIndices.begin(); I != featureIndices.end(); ++I)
		dynamicFeatures.push_back(*I);
}
void Trainer::setDynamicFeatureByIndex(const std::string featureIndex) {
	assert(pcaFeatures.size() == 0 && "Cannot use PCA and 'normal' features at the same time");
	dynamicFeatures.push_back(featureIndex);
}

void Trainer::setDynamicFeaturesByName(const std::vector<std::string>& featureNames){
	for(std::vector<std::string>::const_iterator I = featureNames.begin(); I != featureNames.end(); ++I) {
		setDynamicFeatureByName(*I);
	}
}

void Trainer::setDynamicFeatureByName(const std::string featureName){
	assert(pcaFeatures.size() == 0 && "Cannot use PCA and 'normal' features at the same time");
	// build query for name
	std::string tmp;
	try {
		std::stringstream qss;
		qss << "SELECT id FROM dynamic_features f WHERE f.name = \"" << featureName << "\"";

		// query for the index of that name
		tmp = pStmt->GetSqlResultString(qss.str());

		// store feature index in field
		dynamicFeatures.push_back(tmp);
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


void Trainer::setPcaFeaturesByIndex(const std::vector<std::string>& featureIndices) {
	assert((staticFeatures.size() + dynamicFeatures.size()) == 0 && "Cannot use PCA and 'normal' features at the same time");
	for(std::vector<std::string>::const_iterator I = featureIndices.begin(); I != featureIndices.end(); ++I)
		pcaFeatures.push_back(*I);
}
void Trainer::setPcaFeatureByIndex(const std::string featureIndex) {
	assert((staticFeatures.size() + dynamicFeatures.size()) == 0 && "Cannot use PCA and 'normal' features at the same time");
	pcaFeatures.push_back(featureIndex);
}

void Trainer::setPcaFeaturesByName(const std::vector<std::string>& featureNames){
	for(std::vector<std::string>::const_iterator I = featureNames.begin(); I != featureNames.end(); ++I) {
		setPcaFeatureByName(*I);
	}
}

void Trainer::setPcaFeatureByName(const std::string featureName){
	assert((staticFeatures.size() + dynamicFeatures.size()) == 0 && "Cannot use PCA and 'normal' features at the same time");
	// build query for name
	std::string tmp;
	try {
		std::stringstream qss;
		qss << "SELECT id FROM pca_features f WHERE f.name = \"" << featureName << "\"";

		// query for the index of that name
		tmp = pStmt->GetSqlResultString(qss.str());

		// store feature index in field
		pcaFeatures.push_back(tmp);
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

/*
 * Reads an entry for the training values form the database and appends it to the Array target in one-of-n coding
*/
void Trainer::appendToTrainArray(Array<double>& target, Kompex::SQLiteStatement* stmt, size_t queryIdx, double max, double min, Array<double>& oneOfN) {
	size_t theOne = valToOneOfN(stmt, queryIdx, max, min);
	size_t nClasses = oneOfN.dim(0);

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

/*
 * generates the default query, querying for all patterns which have all features set and using the column
 * targetName as target, using the current values for the features and targetName
 */
void Trainer::genDefaultQuery() {
	std::stringstream qss;

	qss << "SELECT \n ";
	size_t s = staticFeatures.size();
	size_t d = dynamicFeatures.size();
	size_t p = pcaFeatures.size();
	for(size_t i = 0; i < s; ++i) {
		qss << " c" << i << ".value AS Feature" << i << ",\n";
	}
	for(size_t i = 0; i < d; ++i) {
		qss << " s" << i << ".value AS Feature" << i + s << ",\n";
	}
	for(size_t i = 0; i < p; ++i) {
		qss << " p" << i << ".value AS Feature" << i << ",\n";
	}
	qss << " m." << trainForName << " AS target FROM measurement m \n";
	for(size_t i = 0; i < s; ++i) {
		qss << " JOIN code c" << i << " ON m.cid=c" << i << ".cid AND c" << i << ".fid=" << staticFeatures[i] << std::endl;
	}
	for(size_t i = 0; i < d; ++i) {
		qss << " JOIN setup s" << i << " ON m.sid=s" << i << ".sid AND s" << i << ".fid=" << dynamicFeatures[i] << std::endl;
	}
	for(size_t i = 0; i < p; ++i) {
		qss << " JOIN principal_component p" << i << " ON m.pid=p" << i << ".pid AND p" << i << ".fid=" << pcaFeatures[i] << std::endl;
	}
//std::cout << "Query: \n" << qss.str() << std::endl;
	query = qss.str();
}

/*
 * Reads values form the database and stores the features in in, the targets (mapped according to the set policy) in targets as one-of-n coding
*/
size_t Trainer::readDatabase(Array<double>& in, Array<double>& target) throw(Kompex::SQLiteException) {
	// if no query has been set, use default query
	if(query.size() == 0)
		genDefaultQuery();

	// read the maximum of the column in measurement for which to train
	double max, min;
	if(genOut != GenNNoutput::ML_KEEP_INT)
		max = getMaximum(trainForName), min = getMinimum(trainForName);

	Kompex::SQLiteStatement *localStmt = new Kompex::SQLiteStatement(pDatabase);
	unsigned int nClasses = model.getParameterDimension() <= 2 ? 1 : model.getOutputDimension();

	localStmt->Sql(query);

	size_t nRows = localStmt->GetNumberOfRows();
	in = Array<double>(nRows, nFeatures());
	LOG(INFO) << "Queried Rows: " << nRows << ", Number of features: " << staticFeatures.size() << " + " << dynamicFeatures.size()  <<
			" + " << pcaFeatures.size() << std::endl;
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
		for(size_t j = 0; j < nFeatures(); ++j) {
			in(i, j) = localStmt->GetColumnDouble(j);
		}


		// translate index to one-of-n coding
		if(genOut == ML_MAP_TO_N_CLASSES)
			measurements.push_back(std::make_pair(localStmt->GetColumnDouble(nFeatures()), i));
		else
			appendToTrainArray(target, localStmt, nFeatures(), max, min, oneOfN);

		++i;
	}


	if(genOut == ML_MAP_TO_N_CLASSES)
		mapToNClasses(measurements, model.getOutputDimension(), NEG, POS, target);

	// reset the prepared statement
	localStmt->Reset();

	// do not forget to clean-up
	localStmt->FreeQuery();
	delete localStmt;

	FeaturePreconditioner fp;
	featureNormalization = fp.normalize(in, -1, 1);
	return nRows;
}

double Trainer::train(Optimizer& optimizer, ErrorFunction& errFct, size_t iterations) throw(MachineLearningException) {
	double error = 0;

	if(TRAINING_OUTPUT)
		writeHeader("Normal trainer", optimizer, errFct, iterations);


//		std::cout << "Query: \n" << query << std::endl;
	try {
		if(nFeatures() != model.getInputDimension() && model.getParameterDimension() > 2) {
			std::cerr << "Number of features: " << nFeatures() << "\nModel input size: " << model.getInputDimension() << std::endl;
//			throw MachineLearningException("Number of selected features is not equal to the model's input size");
		}

		Array<double> in, target;
		Array<double> out(model.getOutputDimension());

		size_t nRows = readDatabase(in, target);

		// do the actual training
		optimizer.init(model.getModel());

//std::cout << target << std::endl;
		if(iterations != 0) {
			for(size_t i = 0; i < iterations; ++i) {
				optimizer.optimize(model.getModel(), errFct, in, target);
				if(TRAINING_OUTPUT)
					writeStatistics(i, in, target, errFct);
			}

/*			if(errFct == SVM_Optimizer::dummyError) { // called with SVM
				model.model(in, out);
				error
			}*/
			error = errFct.error(model.getModel(), in, target);
		}
		else
			error = this->myEarlyStopping(optimizer, errFct, in, target, 10, std::max(static_cast<size_t>(1),nRows/1000));

		size_t misClass = 0;
		for(size_t i = 0; i < in.dim(0); ++i ) {
			model.model(in.subarr(i,i), out);
//				std::cout << target.subarr(i,i) << out << std::endl;
//				std::cout << oneOfNtoIdx(target.subarr(i,i)) << " - " <<  oneOfNtoIdx(out) << std::endl;
			if(oneOfNtoIdx(target.subarr(i,i)) != oneOfNtoIdx(out))
				++misClass;
		}
		LOG(INFO) << "Misclassification rate: " << (double(misClass)/in.dim(0)) * 100.0 << "%\n";

	} catch(Kompex::SQLiteException& sqle) {
		const std::string err = "\nSQL query for training data failed\n" ;
		LOG(ERROR) << err << std::endl;
		sqle.Show();
		throw ml::MachineLearningException(err);
	}catch (std::exception& exception) {
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
double Trainer::evaluateDatabase(ErrorFunction& errFct) throw(MachineLearningException) {
	try {
		if(nFeatures() != model.getInputDimension()) {
			std::cerr << "Number of features: " << nFeatures() << "\nModel input size: " << model.getInputDimension() << std::endl;
			throw MachineLearningException("Number of selected features is not equal to the model's input size");
		}

		Array<double> in, target;

		readDatabase(in, target);

		return errFct.error(model.getModel(), in, target);
	} catch(Kompex::SQLiteException& sqle) {
		const std::string err = "\nSQL query for evaluation data failed\n" ;
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
 * Evaluates a pattern using the internal model
 */
size_t Trainer::evaluate(Array<double>& pattern) {
	Evaluator eval(model, featureNormalization);

	return eval.evaluate(pattern);
}

size_t Trainer::evaluate(const double* pattern) {
	Evaluator eval(model, featureNormalization);

	return eval.evaluate(pattern);
}

/*
 * saves the fields model and featureNormalization to a file
 */
void Trainer::saveModel(const std::string filename, const std::string path){
	std::string filePath = path + "/" + filename;
	// store model
	model.save((filePath + ".mod").c_str());

	// store the feature normalization parameters
	std::fstream fnp;
	fnp.open(filePath + ".fnp", std::ios::out);

	size_t nProperties = featureNormalization.dim(0);

	if(!fnp.is_open())
		throw MachineLearningException("Cannot open " + filePath + ".fnp for writing");
	// dump parameters as ssv, one column for each feature in form: avgerage, min, max
	for(size_t i = 0; i < nProperties; ++i) {
		for(size_t j = 0; j < featureNormalization.dim(1); ++j) {
			fnp << featureNormalization(i,j) << " ";
		}
		fnp << "\n";
	}

	// write min and max to file
//	fnp << featureNormalization(nProperties,0) << " " << featureNormalization(nProperties,1) << "\n";

	fnp.close();
}

size_t Trainer::loadModel(const std::string& filename, const std::string& path){
	std::string filePath = path + "/" + filename;
	// load model
	model.load((filePath + ".mod").c_str());

	// load the feature normalization parameters
	std::ifstream fnp;
	fnp.open(filePath + ".fnp", std::ios::in);

	if(!fnp.is_open())
		throw MachineLearningException("Cannot open " + filePath + ".fnp for reading");

	featureNormalization.resize(0,0,false);
	std::string line, buf;
	size_t numF;

	// read parameters from ssv, one line for each feature in forom: avgerage, min, max
	while(getline(fnp, line)) {
		numF = 0;
		std::stringstream linestream(line);
		// read the four features
		while(linestream >> buf) {
			++numF;
			featureNormalization.append_elem(insieme::utils::numeric_cast<double>(buf));
		}

	}
	featureNormalization.resize(4, numF, false);

	return numF;
}


} // end namespace ml
} // end namespace insieme

