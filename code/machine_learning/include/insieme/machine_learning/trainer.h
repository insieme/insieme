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

#pragma once
#include <list>

#include "KompexSQLitePrerequisites.h"
#include "KompexSQLiteDatabase.h"
#include "KompexSQLiteStatement.h"
#include "KompexSQLiteException.h"
//#include "KompexSQLiteStreamRedirection.h"
//#include "KompexSQLiteBlob.h"

#include "Array/Array.h"

#include "insieme/machine_learning/myModel.h"
#include "insieme/machine_learning/machine_learning_exception.h"

namespace insieme {
namespace ml {

#define TRAINING_OUTPUT false

#define POS  1
#define NEG  0

// enums defining how the measurement values should be mapped to the ml-algorithms output
enum GenNNoutput {
	ML_KEEP_INT,
	ML_MAP_FLOAT_LIN,
	ML_MAP_FLOAT_LOG,
	ML_MAP_FLOAT_HYBRID,
	ML_MAP_TO_N_CLASSES,
//	ML_COMPARE_BINARY,
	ML_FUZZY_VECTOR,
	size
};

namespace {
/**
 * calculates the index of the biggest element in an Array
 * @param  arr The array
 * @return  the index of the biggest element in the array arr
 */
template<typename T>
size_t arrayMaxIdx(const Array<T>& arr) {
	size_t idx = 0, cnt = 0;
	T currMax = 0;

	for(typename Array<T>::const_iterator I = arr.begin(); I != arr.end(); ++I) {
		if(currMax < *I)
			idx = cnt;
		++cnt;
	}
	return idx;
}
}// end anonymous namespace


class Trainer {
	enum GenNNoutput genOut;
protected:
	Kompex::SQLiteDatabase *pDatabase;
	std::string dbPath;
	Kompex::SQLiteStatement *pStmt;

	std::vector<std::string> staticFeatures, dynamicFeatures, pcaFeatures;
	std::vector<std::string> excludeCodes, filterCodes;
	std::string trainForName, query;

	MyModel& model;
	Array<double> featureNormalization;
	std::ostream& out;

private:
	/**
	 * Queries the maximum value for the given parameter in table measurements for the used features
	 * @param param the name of the column to query for
	 * @return the maximum of the queried column with the current features set
	 */
	double getMaximum(const std::string& param);

	/**
	 * Queries the minimum value for the given parameter in table measurements for the used features
	 * @param param the name of the column to query for
	 * @return the minimum of the queried column with the current features set
	 */
	double getMinimum(const std::string& param);

	/**
	 * Converts the value read from the database to an index to a class, according to the policy defined in the variable genOut.
	 * The returned should be set to POS, the rest to NEG
	 * @param stmt the SQLiteStatement with a prepared query to read the value from position index
	 * @param index the index of the value to be trained in the database joint table
	 * @param max the maximum of the values in the columns. Will be ignored if genOut is set to ML_KEEP_INT
	 * @return the index for the one of n coding of the current query
	 */
	size_t valToClass(Kompex::SQLiteStatement* stmt, size_t index, double max, double min);

	/**
	 * Generates an array where the element at index is set to POS, the rest to NEG
	 * @param theOne the index of actual class
     * @param oneOfN an array of nClasses size, prefilled with NEG which will be set to POS at position theOne
	 */
	void valToOneOfN(size_t theOne, Array<double>& oneOfN);

	/**
	 * Generates an array where the elements form a fuzzy train vector with values between POS and NEG, depending on their measurements
	 * @param stmt the SQLiteStatement with a prepared query to read the value from position index
	 * @param index the index of the first measured value in the database
     * @param oneOfN an array of nClasses size to store the fuzzy training signal
	 */
	void valsToFuzzyTrainVector(Kompex::SQLiteStatement* stmt, size_t index, Array<double>& fuzzy);


    /**
     * puts the first numPatterns/n patterns in the first class, the second in the second etc
     * @param measurements a vector containing a pair of one double, representing the measured value, and a size_t, holding the index of the measurement
     * @param n the number of classes to map to
     * @param neg the lower target value
     * @param pos the upper target value
     * @param target an empty array which will be filled with the one-of-n coding for all patterns
     */
    void mapToNClasses(std::list<std::pair<double, size_t> >& measurements, size_t n, double neg, double pos, Array<double>& target);

protected:
	/**
	 * writes informations about the current training run to out (protected field)
	 * @param trainer a string describing the used trainer
	 * @param optimizer the used optimizer
	 * @param errFct the used error function
	 * @param iterations the number of training iterations
	 */
	void writeHeader(const std::string trainer, const Optimizer& optimizer, const ErrorFunction& errFct, const size_t iterations) const;

	/**
	 * writes the current iteration and error on the dataset to out (protected field)
	 * @param iteration the current iteration
	 * @param in the array of inputs to the network
	 * @param target the array of desired outputs of the network
	 * @param errFct the used error function
	 * @param valErr the error on the validation set, pass a negative value to not use it
	 */
	void writeStatistics(size_t iteration, Array<double>& in, Array<double>& target, ErrorFunction& errFct, double valErr) throw(SharkException);

	/**
	 * Returns the index of the maximum of all elements in coded
	 * @param coded An array with a one-of-n coding
	 * @return the 'the one'
	 */
	size_t oneOfNtoIdx(Array<double> coded);

	double earlyStopping(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, Array<double>& target, size_t validatonSize);

	/**
	 * Splits the training dataset in two pieces of validationSieze% for validation and 100-validatonSize% for validation
	 * @param optimizer the Optimizer to be used
	 * @param errFct the Error function to be use
	 * @param in the input to the model
	 * @param target the desired outputs for the given inputs
	 * @param validationSize the size of the validation size in percent
	 * @param nBatches the number of batches to train at once to be generated out of the entire training dataset
	 * @return the current error on the validation
	 */
	double myEarlyStopping(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, Array<double>& target, size_t validationSize, size_t nBatches = 5);

	/**
	 * Reads an entry for the training values form the database and appends it to the Array target in one-of-n coding
	 * @param target an Array where the target values should be written to
	 * @param stmt An sql statement holdin the line with de desired target value
	 * @param queryIdx the index of the desired target value in the sql statement
	 * @param max the maximum value of all targets (needed for one-to-n coding)
	 * @param min the minimum value of all targets (needed for one-to-n coding)
	 * @param oneOfN an Array of size number-of-classes containing only NEG. The Array will be reset during the call, only needed for performance reasons
	 */
	virtual void appendToTrainArray(Array<double>& target, Kompex::SQLiteStatement* stmt, size_t queryIdx, double max, double min, Array<double>& oneOfN);

	/**
	 * Reads values form the database and stores the features in in, the targets (mapped according to the set policy) in targets as one-of-n coding
	 * @param in An empty Array which will hold the features read form the database
	 * @param target An empty Array which will hold the target values as one-of-n codung
	 * @return the number of training patterns
	 */
	size_t readDatabase(Array<double>& in, Array<double>& target) throw(Kompex::SQLiteException);
public:
	Trainer(const std::string& myDbPath, MyModel& myModel, enum GenNNoutput genOutput = ML_MAP_TO_N_CLASSES, std::ostream& outstream = std::cout) :
		genOut(genOutput), pDatabase(new Kompex::SQLiteDatabase(myDbPath, SQLITE_OPEN_READONLY, 0)), dbPath(myDbPath),
		pStmt(new Kompex::SQLiteStatement(pDatabase)), trainForName("time"), model(myModel), out(outstream) {
/*			query = std::string("SELECT \
				m.id AS id, \
				m.ts AS ts, \
				c1.value AS FeatureA, \
				c2.value AS FeatureB, \
				c3.value AS FeatureC, \
				m.time AS target \
					FROM measurement m \
					JOIN code c1 ON m.cid=c1.cid AND c1.fid=1 \
					JOIN code c2 ON m.cid=c2.cid AND c2.fid=2 \
					JOIN code c3 ON m.cid=c3.cid AND c3.fid=3 ");
		*/
	}

	~Trainer() {
		delete pStmt;
		pDatabase->Close();
// FIXME find a way to avoid stack corruption in certain cases (-f2 -f1)
		delete pDatabase;
	}

	/**
	 * trains the model using the patterns returned by the given query or the default query if none is given
	 * @param optimizer the Shark Optimizer to be used, eg. Quickprop, Bfgs etc.
	 * @param errFct the Shark error function to be used, eg. MeanSquaredError,
	 * @param iterations the number of training operations to perform. If a number >0 is given, the trainer performs this
	 *        number of training iterations on the whole dataset and returns the error on it. If 0 is passed, the trainer
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
	 * Evaluates a pattern using the internal model.
	 * @param pattern An Array holding the features of the pattern to be evaluated
	 * @return the index of the winning class
	 */
	virtual size_t evaluate(Array<double>& pattern);

	/**
	 * Evaluates a pattern using the internal model
	 * WARNING size of pointer is not checked
	 * @param pattern A C pointer holding the static features of the pattern to be evaluated
	 * @return the index of the winning class
	 */
	virtual size_t evaluate(const double* pattern);

	/**
	 * adds a vector of static features indices to the internal feature vector
	 * @param featureIndices a vector holding the column (in the database) indices of some static features
	 */
	void setStaticFeaturesByIndex(const std::vector<std::string>& featureIndices);
	/**
	 * adds one feature index to the internal static feature vector
	 * @param featureIndex the index of the column (in the database) holding a feature
	 */
	void setStaticFeatureByIndex(const std::string featureIndex);

	/**
	 * adds a vector of static features to the internal feature vector by name
	 * @param featureNames a vector holding the name (in the database) of some static features
	 */
	void setStaticFeaturesByName(const std::vector<std::string>& featureNames);
	/**
	 * adds one feature to the internal static feature vector by name
	 * @param featureName the name of a feature (in the database)
	 */
	void setStaticFeatureByName(const std::string featureName);

	/**
	 * adds a vector of dynamic features indices to the internal feature vector
	 * @param featureIndices a vector holding the column (in the database) indices of some dynamic features
	 */
	void setDynamicFeaturesByIndex(const std::vector<std::string>& featureIndices);
	/**
	 * adds one feature index to the internal dynamic feature vector
	 * @param featureIndex the index of the column (in the database) holding a feature
	 */
	void setDynamicFeatureByIndex(const std::string featureIndex);

	/**
	 * adds a vector of dynamic features to the internal feature vector by name
	 * @param featureNames a vector holding the name (in the database) of some dynamic features
	 */
	void setDynamicFeaturesByName(const std::vector<std::string>& featureNames);
	/**
	 * adds one feature to the internal dynamic feature vector by name
	 * @param featureName the name of a feature (in the database)
	 */
	void setDynamicFeatureByName(const std::string featureName);

	/**
	 * adds a vector of pca features indices to the internal feature vector
	 * @param featureIndices a vector holding the column (in the database) indices of some pca features
	 */
	void setPcaFeaturesByIndex(const std::vector<std::string>& featureIndices);
	/**
	 * adds one feature index to the internal pca feature vector
	 * @param featureIndex the index of the column (in the database) holding a feature
	 */
	void setPcaFeatureByIndex(const std::string featureIndex);

	/**
	 * adds a vector of pca features to the internal feature vector by name
	 * @param featureNames a vector holding the name (in the database) of some pca features
	 */
	void setPcaFeaturesByName(const std::vector<std::string>& featureNames);
	/**
	 * adds one feature to the internal pca feature vector by name
	 * @param featureName the name of a feature (in the database)
	 */
	void setPcaFeatureByName(const std::string featureName);

	/**
	 * adds a vector of codes not to be considered to the internal excludeCode vector by cid
	 * @param excludeCids a vector holding the cids (in the database) of some codes not to be considered
	 */
	void setExcludeCodes(const std::vector<std::string>& excludeCids);
	/**
	 * adds one code not to be considered to the internal excludeCode vector by cid
	 * @param excludeCid the name of a code not to be considered (in the database)
	 */
	void setExcludeCode(const std::string excludeCid);

	/**
	 * adds a vector of codes to the internal filter vector by cid. If none are specified, all codes are used
	 * @param filterCids a vector holding the cids (in the database) of some codes not to be considered
	 */
	void setFilterCodes(const std::vector<std::string>& filterCids);
	/**
	 * one code to the internal filter vector by cid. If none are specified, all codes are used
	 * @param filterCid the name of a code not to be considered (in the database)
	 */
	void setFilterCode(const std::string filterCid);

	/**
	 * returns the number of all (static + dynamic + pca) features
	 * @return the number of features
	 */
	size_t nFeatures() { return staticFeatures.size() + dynamicFeatures.size() + pcaFeatures.size(); }

	/**
	 * sets the name of the column from which to read the target values form the database
	 * @param targetName the name of the column in the database which holds the training target
	 */
	void setTargetByName(const std::string& targetName){ trainForName = targetName; }

	/**
	 * sets the default splitting values for a CPU+2GPU machine as targets. Only to be used in conjunciton
	 * with GenNNoutput::ML_FUZZY_VECTOR
	 */
	void setDefaultSplittingAsTarget(){
		assert(genOut == GenNNoutput::ML_FUZZY_VECTOR && "Fuzzy ouptut can only be used in conjunciton with GenNNoutput::ML_FUZZY_VECTOR");
		trainForName = "s100_0_0, m.s90_10_0, m.s90_5_5, m.s80_20_0, m.s80_10_10, m.s70_30_0, m.s70_15_15, m.s60_40_0, \
			m.s60_20_20, m.s50_50_0, m.s50_25_25, m.s40_60_0, m.s40_30_30, m.s30_70_0, m.s30_35_35, m.s20_80_0, m.s20_40_40, m.s10_90_0, m.s10_45_45, \
			m.s0_100_0, m.s0_50_50";
	}

	/**
	 * generates the default query, querying for all patterns which have all features set and using the column
	 * targetName as target, using the current values for the features and targetName. If no query is set before
	 * the training is started, this function will be used to generate a query
	 */
	void genDefaultQuery();

	/**
	 * sets the query to a custom string. The query must return one line for each pattern of the following form
	 * [measurmentId, featue1, ..., featureN, measurement
	 * @param customQuery a string to be used as database query
	 */
	void setCustomQuery(std::string& customQuery) { query = customQuery; }

	/**
	 * return the query to be used
	 * @return the current value of the field query
	 */
	std::string& getQuery() { return query; }

	/**
	 * return the internal stored model
	 * @return the value of the field model
	 */
	MyModel& getModel() { return model; }

	/**
	 * Gives back an array of size (3 x nFeatures) holding the average, the min and the max of all features
	 * @return The array holding the data needed for feature normalization
	 */
	Array<double> getFeatureNormalization() { return featureNormalization; }

	/**
	 * Generates two files:
	 * The model is stored in path/filename.mod
	 * The feature normalization data is stored in path/filename.fnd
	 * @param filename The name of the files to store the data. The appropriate file extension will be added automatically
	 * @param path The path were to store the two files, the current directory is the default
	 */
	void saveModel(const std::string filename, const std::string path = ".");
	/**
	 * Loads stored values from disk:
	 * The model is loaded from in path/filename.mod
	 * The feature normalization data is loaded from in path/filename.fnd
	 * The trainer must have been constructed with model of the same type/size as the stored one
	 * @param filename The name of the files to load the data from. The appropriate file extension will be added automatically
	 * @param path The path were to load the two files from, the current directory is the default
	 * @return The number of features for this model (not takeing into account doubling for binary compare trainers)
	 */
	size_t loadModel(const std::string& filename, const std::string& path = ".");
};

} // end namespace ml
} // end namespace insieme
