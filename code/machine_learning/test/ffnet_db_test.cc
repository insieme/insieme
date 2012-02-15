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

#include <fstream>
#include <string>
#include <sstream>
#include <cstdlib>


#include <gtest/gtest.h>

#include "KompexSQLitePrerequisites.h"
#include "KompexSQLiteDatabase.h"
#include "KompexSQLiteStatement.h"
#include "KompexSQLiteException.h"

#include "ReClaM/FFNet.h"
#include "ReClaM/createConnectionMatrix.h"
#include "ReClaM/FFNetSource.h"
//#include "insieme/machine_learning/backprop.h"
#include "ReClaM/Svm.h"

#include "insieme/machine_learning/myOptimizer.h"
#include "insieme/machine_learning/myErrorFunctions.h"

#include "insieme/machine_learning/inputs.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/machine_learning/trainer.h"
#include "insieme/machine_learning/binary_compare_trainer.h"
#include "insieme/machine_learning/evaluator.h"

using namespace insieme::ml;

class MlTest : public ::testing::Test {
 protected:

	// create very simple dataset
	virtual void SetUp() {
//		return;
		try
		{
			// create and open database
			Kompex::SQLiteDatabase *pDatabase = new Kompex::SQLiteDatabase("linear.db", SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, 0);
			// create statement instance for sql queries/statements
			Kompex::SQLiteStatement *features = new Kompex::SQLiteStatement(pDatabase);
			Kompex::SQLiteStatement *measurement = new Kompex::SQLiteStatement(pDatabase);
			Kompex::SQLiteStatement *data = new Kompex::SQLiteStatement(pDatabase);

			// delete tables if already existing
			if(features->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='features'") >= 0)
				features->SqlStatement("DROP TABLE features");
			if(measurement->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='measurement'") >= 0)
				measurement->SqlStatement("DROP TABLE measurement");
			if(data->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='data'") >= 0)
				data->SqlStatement("DROP TABLE data");

			// create tables
			features->SqlStatement("CREATE TABLE features (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL,	static BOOL NOT NULL)");
			measurement->SqlStatement("CREATE TABLE measurement (id INTEGER PRIMARY KEY, ts TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
					time DOUBLE, power DOUBLE, cost DOUBLE)");
			data->SqlStatement("CREATE TABLE data (fid INTEGER REFERENCES features ON DELETE RESTRICT ON UPDATE RESTRICT, \
				mid INTEGER REFERENCES measurement ON DELETE RESTRICT ON UPDATE RESTRICT, \
				value INTEGER NOT NULL, \
				PRIMARY KEY(fid, mid))");

			features->BeginTransaction();
			// sql statements to write into the tables
			features->Sql("INSERT INTO features (name, static) VALUES(?, ?);");

			// setup feature table for three features
			features->BindString(1, "FeatureA");
			features->BindBool(2, false);
			features->Execute();
			features->Reset();

			features->BindString(1, "FeatureB");
			features->BindBool(2, true);
			features->Execute();
			features->Reset();

			features->BindString(1, "FeatureC");
			features->BindBool(2, false);
			features->Execute();
			features->Reset();

			features->BindString(1, "FeatureD");
			features->BindBool(2, false);
			features->ExecuteAndFree();

			features->CommitTransaction();
			delete features;

			// read values form file into database
//			data->BeginTransaction();
			measurement->BeginTransaction();
			measurement->Sql("INSERT INTO measurement (time) VALUES (?)");
			data->Sql("INSERT INTO data (fid, mid, value) VALUES(?, ?, ?);");

			int mid = 0;

			// loop over each line in the file
			for(int i = 0; i < 10; ++i) {
				// target class is round robin
				measurement->BindDouble(1, i%5);

				// write measurement result to database
				measurement->Execute();
				measurement->Reset();

				// get the id of the recently added measurement
				mid++; // assume database was empty and index increases one-by-one

				// write features and their corresponding fid and mid to database
				for(int fid = 0; fid < 4; ++fid) {
					data->BindInt(1, fid+1);
					data->BindInt(2, mid);
					data->BindInt(3, (int)((i%5*10) + (rand() % 4)));
					data->Execute();
					data->Reset();
				}
			}

			measurement->FreeQuery();
			data->FreeQuery();

			measurement->CommitTransaction();
//			data->CommitTransaction();

			pDatabase->Close();

			delete pDatabase;
			delete measurement;
			delete data;
		} catch (Kompex::SQLiteException &exception)
		{
			std::cerr << "\nException occured" << std::endl;
			exception.Show();
			throw MachineLearningException("HATE");
		}
	}

};

TEST_F(MlTest, CreateDb) {
	// open file
	std::ifstream file;
	file.open(std::string(IN_DIR) + "mean_value.8", std::ios::in);

	EXPECT_TRUE(file.is_open());

	size_t nFeatures = 3, nMeasurements = 3;

	try
	{
		// create and open database
		Kompex::SQLiteDatabase *pDatabase = new Kompex::SQLiteDatabase("mean8.db", SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, 0);
		// create statement instance for sql queries/statements
		Kompex::SQLiteStatement *features = new Kompex::SQLiteStatement(pDatabase);
		Kompex::SQLiteStatement *measurement = new Kompex::SQLiteStatement(pDatabase);
		Kompex::SQLiteStatement *data = new Kompex::SQLiteStatement(pDatabase);

		// delete tables if already existing
		if(features->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='features'") >= 0)
			features->SqlStatement("DROP TABLE features");
		if(measurement->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='measurement'") >= 0)
			measurement->SqlStatement("DROP TABLE measurement");
		if(data->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='data'") >= 0)
			data->SqlStatement("DROP TABLE data");

		// create tables
		features->SqlStatement("CREATE TABLE features (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL,	static BOOL NOT NULL)");
		measurement->SqlStatement("CREATE TABLE measurement (id INTEGER PRIMARY KEY, ts TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
				time DOUBLE, power DOUBLE, cost DOUBLE)");
		data->SqlStatement("CREATE TABLE data (fid INTEGER REFERENCES features ON DELETE RESTRICT ON UPDATE RESTRICT, \
			mid INTEGER REFERENCES measurement ON DELETE RESTRICT ON UPDATE RESTRICT, \
			value INTEGER NOT NULL, \
			PRIMARY KEY(fid, mid))");

		features->BeginTransaction();
		// sql statements to write into the tables
		features->Sql("INSERT INTO features (name, static) VALUES(?, ?);");

		// setup feature table for three features
		for(size_t i = 0; i < nFeatures; ++i) {
			std::stringstream tableName;
			tableName << "Feature" << char('A' + i);
			features->BindString(1, tableName.str());
			features->BindBool(2, false);
			features->Execute();
			features->Reset();
		}
		features->FreeQuery();

		features->CommitTransaction();

		// read values form file into database
//		data->BeginTransaction();
		measurement->BeginTransaction();
		measurement->Sql("INSERT INTO measurement (time, power, cost) VALUES (?, ?, ?)");
		data->Sql("INSERT INTO data (fid, mid, value) VALUES(?, ?, ?);");

		std::string line, buf;
		int f[nFeatures];
		double m[nMeasurements];
		int mid = 0;

		// loop over each line in the file
		while(getline(file, line)) {
			std::stringstream linestream(line);
			// read the four features
			for(size_t fid = 0; fid < nFeatures; ++fid) {
			    EXPECT_TRUE(linestream >> buf);
			    f[fid] = insieme::utils::numeric_cast<int>(buf);
			}

			// read the three measurements and prepare to write into database
			for(size_t i = 0; i < nMeasurements; ++i) {
			    EXPECT_TRUE(linestream >> buf);
			    m[i] = insieme::utils::numeric_cast<double>(buf);
			    measurement->BindDouble(i+1, m[i]);
			}

			// write measurement result to database
			measurement->Execute();
			measurement->Reset();

			// get the id of the recently added measurement
			mid++; // assume database was empty and index increases one-by-one
//			mid = features->GetSqlResultInt("SELECT MAX(m.id) FROM measurement m");
//			std::cout << mid << std::endl;

			// write features and their corresponding fid and mid to database
			for(size_t fid = 0; fid < nFeatures; ++fid) {
				data->BindInt(1, fid+1);
				data->BindInt(2, mid);
				data->BindInt(3, f[fid]);
				data->Execute();
				data->Reset();
			}
		}

		EXPECT_EQ(features->GetSqlResultInt("SELECT MAX(m.id) FROM measurement m"), 1024);

		measurement->FreeQuery();
		data->FreeQuery();

		measurement->CommitTransaction();
//			data->CommitTransaction();

		pDatabase->Close();

		delete features;
		delete pDatabase;
		delete measurement;
		delete data;
	} catch (Kompex::SQLiteException &exception)
	{
		std::cerr << "\nException occurred" << std::endl;
		exception.Show();
	}

	file.close();
}

TEST_F(MlTest, SvmTrain) {
	Logger::get(std::cerr, DEBUG);
	const std::string dbPath("linear.db");

	RBFKernel kernel(1.0);
	SVM svm(&kernel);
	C_SVM csvm(&svm, 100.0, 100.0);
	MySVM_Optimizer opt;

	opt.init(csvm);

	BinaryCompareTrainer svmTrainer(dbPath, csvm);

	std::vector<std::string> features;

	for(size_t i = 0u; i < 3u; ++i)
		features.push_back(toString(i+1));

	svmTrainer.setFeaturesByIndex(features);

	//SVM_Optimizer::dummyError
	MyClassificationError err;

	double error = svmTrainer.train(opt, err, 1);
	LOG(INFO) << "Error: " << error << std::endl;
	EXPECT_LT(error, 1.0);

//	svm.SetTrainingData(input);
//	svmTrainer.train(opt, err, 1);
//	svm.SaveSVMModel(std::cout); //works only if double SVM_Optimizer::optimize(SVM& model, const Array<double>& input, const Array<double>& target, bool copy = true); is set
}
/*
TEST_F(MlTest, MultiSvmTrain) {
	Logger::get(std::cerr, DEBUG);
	const std::string dbPath("linear.db");

	RBFKernel kernel(1.0);
	MultiClassSVM msvm(&kernel, 2);
//	C_SVM csvm(&svm, 100.0, 100.0);
	SVM_Optimizer opt;

//	opt.init(msvm);

	BinaryCompareTrainer svmTrainer(dbPath, msvm);

	std::vector<std::string> features;
	for(size_t i = 0u; i < 3u; ++i)
		features.push_back(toString(i+1));

	svmTrainer.setFeaturesByIndex(features);

	//SVM_Optimizer::dummyError
	ClassificationError err;

	double error = svmTrainer.train(opt, err, 1);
	LOG(INFO) << "Error: " << error << std::endl;
	EXPECT_LT(error, 1.0);

}
*/
TEST_F(MlTest, FfNetTrain) {
	Logger::get(std::cerr, DEBUG);
	const std::string dbPath("linear.db");

	// Create a connection matrix with 2 inputs, 1 output
	// and a single, fully connected hidden layer with
	// 8 neurons:
	Array<int> con;
	size_t nIn = 3, nOut = 5;
	createConnectionMatrix(con, nIn, 8, nOut, true, false, false);

	// declare Machine
	FFNet net = FFNet(nIn, nOut, con);
	net.initWeights(-0.4, 0.4);
	MyMeanSquaredError err;
	Array<double> in, target;
	MyQuickprop qprop;
	qprop.initUserDefined(net, 1.5, 1.75);
	MyBFGS bfgs;
	bfgs.initBfgs(net);
	MyCG cg;
	MyRpropPlus rpp;
	rpp.init(net);
	MyRpropMinus rpm;
	rpm.init(net);

	// create trainer
	Trainer qpnn(dbPath, net);//, GenNNoutput::ML_MAP_FLOAT_HYBRID);

	std::vector<std::string> features;

	for(size_t i = 0u; i < 3u; ++i)
		features.push_back(toString(i+1));

	qpnn.setFeaturesByIndex(features);

	double error = qpnn.train(bfgs, err, 3);
	LOG(INFO) << "Error: " << error << std::endl;
	EXPECT_LT(error, 1.0);

	qpnn.saveModel("dummy");

//	FFNetSource(std::cout, nIn, nOut, con, net.getWeights(), "tanh(#)", "#", 10);

	// reevaluate the data on the model
	error = qpnn.evaluateDatabase(err);

	LOG(INFO) << "Error: " << error << std::endl;
	EXPECT_LT(error, 1.0);
}

TEST_F(MlTest, FfNetBinaryCompareTrain) {
	Logger::get(std::cerr, DEBUG);
	const std::string dbPath("linear.db");

	// Create a connection matrix with 2 inputs, 1 output
	// and a single, fully connected hidden layer with
	// 8 neurons:
	Array<int> con;
	size_t nIn = 6, nOut = 2;
	createConnectionMatrix(con, nIn, 8, nOut, true, false, false);

	// declare Machine
	FFNet net = FFNet(nIn, nOut, con);
	net.initWeights(-0.4, 0.4);
	MyMeanSquaredError err;
	Array<double> in, target;
	MyQuickprop qprop;
	qprop.initUserDefined(net, 1.5, 1.75);
	MyBFGS bfgs;
	bfgs.initBfgs(net);
	MyCG cg;
	MyRpropPlus rpp;
	rpp.init(net);
	MyRpropMinus rpm;
	rpm.init(net);

	// create trainer
	BinaryCompareTrainer bct(dbPath, net);//, GenNNoutput::ML_MAP_FLOAT_HYBRID);

	std::vector<std::string> features;

	for(size_t i = 0u; i < 3u; ++i)
		features.push_back(toString(i+1));

	bct.setFeaturesByIndex(features);

	double error = bct.train(bfgs, err, 1);
	LOG(INFO) << "Error: " << error << std::endl;

	EXPECT_LT(error, 1.0);

	size_t f = nIn/2;
	Array<double> a(1,f), b(1,f);

	for(size_t i = 0; i < f; ++i) {
		a(0,i) = ((double)(rand()%100)/50)-1;
		b(0,i) = ((double)(rand()%100)/50)-1;
	}

	size_t firstTry = bct.evaluate(a, b);

	a.append_rows(b);

	size_t secondTry = bct.evaluate(a);
	EXPECT_EQ(firstTry, secondTry);
}

TEST_F(MlTest, LoadModel) {
	Logger::get(std::cerr, DEBUG);
	const std::string dbPath("linear.db");

	// declare Machine
	FFNet net;

	Trainer loaded(dbPath, net, GenNNoutput::ML_MAP_TO_N_CLASSES);

	EXPECT_EQ(3u, loaded.loadModel("dummy"));

	Array<double> fnp = loaded.getFeatureNormalization();
	Evaluator eval1(net, fnp);

	Evaluator eval2 = Evaluator::loadEvaluator(net, "dummy");

	size_t f = net.getInputDimension();
	EXPECT_EQ(f, 3u);

	MyMeanSquaredError errFct;
	std::vector<string> features;
	for(size_t i = 0u; i < 3u; ++i)
		features.push_back(toString(i+1));

	loaded.setFeaturesByIndex(features);

	double err = loaded.evaluateDatabase(errFct);
	EXPECT_LT(err, 1.0);

	Array<double> a(f), b(f);

	srand(time(NULL));
	for(size_t i = 0; i < f; ++i) {
		a(i) = ((double)(rand()%100)/50)-1;
		b(i) = ((double)(rand()%100)/50)-1;
	}

	size_t trainerSais = loaded.evaluate(a);

	EXPECT_EQ(eval1.evaluate(a), trainerSais);
	EXPECT_EQ(eval2.evaluate(a), trainerSais);

	trainerSais = loaded.evaluate(b);

	EXPECT_EQ(eval1.evaluate(b), trainerSais);
	EXPECT_EQ(eval2.evaluate(b), trainerSais);
}

