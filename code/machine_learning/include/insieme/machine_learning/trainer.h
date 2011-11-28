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

#include "KompexSQLitePrerequisites.h"
#include "KompexSQLiteDatabase.h"
#include "KompexSQLiteStatement.h"
#include "KompexSQLiteException.h"
//#include "KompexSQLiteStreamRedirection.h"
//#include "KompexSQLiteBlob.h"

#include "Array/Array.h"
#include "ReClaM/FFNet.h"

namespace insieme {
namespace ml {

#define POS  1
#define NEG 0

// enums defining how the measurement values should be mapped to the ml-algorithms output
enum GenNNoutput {
	ML_KEEP_INT,
	ML_MAP_FLOAT_LIN,
	ML_MAP_FLOAT_LOG,
	ML_MAP_FLOAT_HYBRID,
	ML_MAP_TO_N_CLASSES,
	ML_COMPARE_BINARY,
	size
};

class MachineLearningException : public std::exception {
    std::string err;
public :
	const char* what() const throw() {
		return ("Machine Learning Error! \n" + err).c_str();
	}

	MachineLearningException() : err("") {}

    MachineLearningException(std::string errMsg) : err(errMsg) {}

    ~MachineLearningException() throw() {}
};


class Trainer {
	enum GenNNoutput genOut;
protected:
	Kompex::SQLiteDatabase *pDatabase;
	Kompex::SQLiteStatement *pStmt;

	std::vector<std::string> features;
	std::string trainForName;

	Model& model;

private:
	/*
	 * Queries the maximum value for the given parameter in table measurements for the used features
	 * @param
	 * param the name of the column to query for
	 * @return
	 * the maximum of the queried column with the current features set
	 */
	double getMaximum(const std::string& param);

	/*
	 * Queries the minimum value for the given parameter in table measurements for the used features
	 * @param
	 * param the name of the column to query for
	 * @return
	 * the minimum of the queried column with the current features set
	 */
	double getMinimum(const std::string& param);

	/*
	 * Converts the value read from the database to an index in one of n coding, according to the policy defined in the variable genOut.
	 * The returned should be set to POS, the rest to NEG
	 * @param
	 * stmt the SQLiteStatement with a prepared query to read the value from position index
	 * index the index of the value to be trained in the database joint table
	 * max the maximum of the values in the columns. Will be ignored if genOut is set to ML_KEEP_INT
	 * @return
	 * the index for the one of n coding of the current query
	 */
	size_t valToOneOfN(Kompex::SQLiteStatement* stmt, size_t index, double max, double min);

protected:
	/*
	 * Returns the index of the maximum of all elements in coded
	 * @param
	 * coded An array with a one-of-n coding
	 * @return
	 * the 'the one'
	 */
	size_t oneOfNtoIdx(Array<double> coded);

	double earlyStopping(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, Array<double>& target, size_t validatonSize);

	/* Splits the training dataset in two pieces of validationSieze% for validaton and 100-validatonSize% for validation
	 * @param
	 * errFct the Error function to be use
	 * in the input to the model
	 * targed the desired outputs for the given inputs
	 * validationSize the size of the validation size in percent
	 * @return
	 * the current error on the validation set
	 */
	double myEarlyStopping(Optimizer& optimizer, ErrorFunction& errFct, Array<double>& in, Array<double>& target, size_t validatonSize);

public:
	Trainer(const std::string& myDbPath, Model& myModel, enum GenNNoutput genOutput = ML_KEEP_INT) :
		genOut(genOutput), pDatabase(new Kompex::SQLiteDatabase(myDbPath, SQLITE_OPEN_READONLY, 0)), pStmt(new Kompex::SQLiteStatement(pDatabase)),
		trainForName("time"), model(myModel) {
/*		query = std::string("SELECT \
			m.id AS id, \
			m.ts AS ts, \
			d1.value AS FeatureA, \
			d2.value AS FeatureB, \getMaximum
			m.copyMethod AS method \
				FROM measurement m \
				JOIN data d1 ON m.id=d1.mid AND d1.fid=1 \
				JOIN data d2 ON m.id=d2.mid AND d2.fid=2 ");
*/

	}

	~Trainer() {
		delete pStmt;
		pDatabase->Close();
// FIXME find a way to avoid stack corruption in certain cases (-f2 -f1)
		delete pDatabase;
	}

	virtual double train(Optimizer& optimizer, ErrorFunction& errFct, size_t iterations);

	void setFeaturesByIndex(const std::vector<std::string>& featureIndices);
	void setFeatureByIndex(const std::string featureIndex);

	void setFeaturesByName(const std::vector<std::string>& featureNames);
	void setFeatureByName(const std::string featureName);

	void setTargetByName(const std::string& featureNames){ trainForName = featureNames; }
};

} // end namespace ml
} // end namespace insieme
