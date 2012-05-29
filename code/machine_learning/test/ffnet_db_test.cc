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

#include "ReClaM/createConnectionMatrix.h"
#include "ReClaM/FFNetSource.h"
//#include "insieme/machine_learning/backprop.h"
#include "ReClaM/BFGS.h"
#include "ReClaM/CG.h"
#include "ReClaM/Rprop.h"
#include "ReClaM/Quickprop.h"
#include "ReClaM/MeanSquaredError.h"
#include "ReClaM/ClassificationError.h"
#include "ReClaM/Svm.h"
#include "ReClaM/PCA.h"

#include "insieme/machine_learning/myModel.h"

#include "insieme/machine_learning/inputs.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/machine_learning/trainer.h"
#include "insieme/machine_learning/binary_compare_trainer.h"
#include "insieme/machine_learning/evaluator.h"
#include "insieme/machine_learning/database_utils.h"

#include "insieme/machine_learning/pca_separate_ext.h"
#include "insieme/machine_learning/pca_combined_ext.h"

using namespace insieme::ml;

class MlTest : public ::testing::Test {
 protected:

	// create very simple dataset
	virtual void SetUp() {
//		return;
		srand(42);
		try
		{
			// create and open database
			Kompex::SQLiteDatabase *pDatabase = createDatabase("linear.db");//new Kompex::SQLiteDatabase("linear.db", SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, 0);
			// create statement instance for sql queries/statements
			Kompex::SQLiteStatement *staticFeatures = new Kompex::SQLiteStatement(pDatabase);
			Kompex::SQLiteStatement *code = new Kompex::SQLiteStatement(pDatabase);
			Kompex::SQLiteStatement *dynamicFeatures = new Kompex::SQLiteStatement(pDatabase);
			Kompex::SQLiteStatement *setup = new Kompex::SQLiteStatement(pDatabase);
			Kompex::SQLiteStatement *measurement = new Kompex::SQLiteStatement(pDatabase);
			// delete tables if already existing

			staticFeatures->BeginTransaction();
			// sql statements to write into the tables
			staticFeatures->Sql("INSERT INTO static_features (name) VALUES(?);");

			// setup feature table for three staticFeatures
			staticFeatures->BindString(1, "FeatureA");
			staticFeatures->Execute();
			staticFeatures->Reset();

			staticFeatures->BindString(1, "FeatureB");
			staticFeatures->Execute();
			staticFeatures->Reset();

			staticFeatures->BindString(1, "FeatureC");
			staticFeatures->Execute();
			staticFeatures->Reset();

			staticFeatures->BindString(1, "FeatureD");
			staticFeatures->ExecuteAndFree();

			staticFeatures->CommitTransaction();
			delete staticFeatures;

			// generate one dynamic feature
			dynamicFeatures->BeginTransaction();
			// sql statements to write into the tables
			dynamicFeatures->Sql("INSERT INTO dynamic_features (name) VALUES(?);");

			dynamicFeatures->BindString(1, "Feature0");
			dynamicFeatures->ExecuteAndFree();

			dynamicFeatures->CommitTransaction();
			delete dynamicFeatures;

			setup->BeginTransaction();
			setup->Sql("INSERT INTO setup (sid, fid, value) VALUES(?, ?, ?);");

			// generate two dynamic features
			for(int i = 0; i < 2; ++i) {
				setup->BindInt(1, i+1);
				setup->BindInt(2, 1);
				setup->BindInt(3, 2-i);
				setup->Execute();
				setup->Reset();
			}
			setup->FreeQuery();
			setup->CommitTransaction();
			delete(setup);

			measurement->BeginTransaction();
			measurement->Sql("INSERT INTO measurement (cid, sid, time) VALUES (?, ?, ?)");
			code->Sql("INSERT INTO code (cid, fid, value) VALUES(?, ?, ?);");
			int mid = 0; // assume database was empty and index increases one-by-one

			// loop over each pattern
			for(int i = 0; i < 10; ++i) {
				mid++;

				// write the cid (= mid, since there are no dynamic staticFeatures) to the measurement
				measurement->BindInt(1, mid);
				// we only have two dynamic features
				measurement->BindInt(2, (mid+4)/5);
				// target class is round robin
				measurement->BindDouble(3, i%5);

				// write measurement result to database
				measurement->Execute();
				measurement->Reset();

				// get the id of the recently added measurement

				// write staticFeatures and their corresponding fid and mid to database
				for(int fid = 0; fid < 4; ++fid) {
					code->BindInt(1, mid);
					code->BindInt(2, fid+1);
					code->BindInt(3, (int)((i%5*10) + (rand() % 4)));
					code->Execute();
					code->Reset();
				}
			}

			measurement->FreeQuery();
			code->FreeQuery();

			measurement->CommitTransaction();
//			data->CommitTransaction();

			pDatabase->Close();

			delete pDatabase;
			delete measurement;
			delete code;
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

	size_t nFeatures = 3, nMeasurements = 2;

	try
	{
		// create and open database
		Kompex::SQLiteDatabase *pDatabase = new Kompex::SQLiteDatabase("mean8.db", SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, 0);
		// create statement instance for sql queries/statements
		Kompex::SQLiteStatement *staticFeatures = new Kompex::SQLiteStatement(pDatabase);
		Kompex::SQLiteStatement *code = new Kompex::SQLiteStatement(pDatabase);
		Kompex::SQLiteStatement *dynamicFeatures = new Kompex::SQLiteStatement(pDatabase);
		Kompex::SQLiteStatement *setup = new Kompex::SQLiteStatement(pDatabase);
		Kompex::SQLiteStatement *measurement = new Kompex::SQLiteStatement(pDatabase);

		// delete tables if already existing
		if(staticFeatures->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='static_features'") >= 0)
			staticFeatures->SqlStatement("DROP TABLE static_features");
		if(dynamicFeatures->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='dynamic_features'") >= 0)
			dynamicFeatures->SqlStatement("DROP TABLE dynamic_features");
		if(measurement->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='measurement'") >= 0)
			measurement->SqlStatement("DROP TABLE measurement");
		if(code->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='code'") >= 0)
			code->SqlStatement("DROP TABLE code");
		if(code->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='setup'") >= 0)
			code->SqlStatement("DROP TABLE setup");

		// create tables
		staticFeatures->SqlStatement("CREATE TABLE static_features (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL)");
		code->SqlStatement("CREATE TABLE code (cid INTEGER, fid INTEGER REFERENCES static_features ON DELETE RESTRICT ON UPDATE RESTRICT, \
			value INTEGER NOT NULL, PRIMARY KEY(cid, fid))");
		dynamicFeatures->SqlStatement("CREATE TABLE dynamic_features (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL)");
		setup->SqlStatement("CREATE TABLE setup (sid INTEGER, fid INTEGER REFERENCES static_features ON DELETE RESTRICT ON UPDATE RESTRICT, \
			value INTEGER NOT NULL, PRIMARY KEY(sid, fid))");
		measurement->SqlStatement("CREATE TABLE measurement (id INTEGER PRIMARY KEY, ts TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
				cid INTEGER REFERENCES code ON DELETE RESTRICT ON UPDATE RESTRICT, sid INTEGER REFERENCES setup ON DELETE RESTRICT ON UPDATE RESTRICT, \
				time DOUBLE, power DOUBLE)");

		staticFeatures->BeginTransaction();
		// sql statements to write into the tables
		staticFeatures->Sql("INSERT INTO static_features (name) VALUES(?);");

		// setup feature table for three features
		for(size_t i = 0; i < nFeatures; ++i) {
			std::stringstream tableName;
			tableName << "Feature" << char('A' + i);
			staticFeatures->BindString(1, tableName.str());
			staticFeatures->Execute();
			staticFeatures->Reset();
		}
		staticFeatures->FreeQuery();

		staticFeatures->CommitTransaction();


		dynamicFeatures->BeginTransaction();
		// sql statements to write into the tables
		dynamicFeatures->Sql("INSERT INTO dynamic_features (name) VALUES(?);");

		// setup feature table for one dynamic feature
		std::stringstream tableName;
		tableName << "Feature1";
		dynamicFeatures->BindString(1, tableName.str());
		dynamicFeatures->Execute();
		dynamicFeatures->Reset();

		dynamicFeatures->FreeQuery();

		dynamicFeatures->CommitTransaction();

		// read values form file into database
//		data->BeginTransaction();
		measurement->BeginTransaction();
		measurement->Sql("INSERT INTO measurement (cid, sid, time, power) VALUES (?, ?, ?, ?)");
		code->Sql("INSERT INTO code (cid, fid, value) VALUES(?, ?, ?);");
		setup->Sql("INSERT INTO setup (sid, fid, value) VALUES(?, ?, ?);");

		std::string line, buf;
		int f[nFeatures];
		double m[nMeasurements];
		int mid = 0; // assume database was empty and index increases one-by-one

		// loop over each line in the file
		while(getline(file, line)) {
			mid++;
			std::stringstream linestream(line);
			// read the four features
			for(size_t fid = 0; fid < nFeatures; ++fid) {
			    EXPECT_TRUE(linestream >> buf);
			    f[fid] = insieme::utils::numeric_cast<int>(buf);
			}

			// write the cid (= mid, since all measurements have different static features) to the measurement
			measurement->BindInt(1, mid);
			measurement->BindInt(2, mid);

			// use first measurement column as dynamic feature
		    EXPECT_TRUE(linestream >> buf);
		    setup->BindInt(1, mid);
		    setup->BindInt(2, 1);
		    setup->BindInt(3, insieme::utils::numeric_cast<double>(buf));
		    setup->Execute();
		    setup->Reset();

			// read the three measurements and prepare to write into database
			for(size_t i = 0; i < nMeasurements; ++i) {
			    EXPECT_TRUE(linestream >> buf);
			    m[i] = insieme::utils::numeric_cast<double>(buf);
			    measurement->BindDouble(i+3, m[i]);
			}

			// write measurement result to database
			measurement->Execute();
			measurement->Reset();

			// get the id of the recently added measurement
//			mid = features->GetSqlResultInt("SELECT MAX(m.id) FROM measurement m");
//			std::cout << mid << std::endl;

			// write features and their corresponding fid and mid to database
			for(size_t fid = 0; fid < nFeatures; ++fid) {
				code->BindInt(1, mid);
				code->BindInt(2, fid+1);
				code->BindInt(3, f[fid]);
				code->Execute();
				code->Reset();
			}
		}

		EXPECT_EQ(staticFeatures->GetSqlResultInt("SELECT MAX(m.id) FROM measurement m"), 1024);

		measurement->FreeQuery();
		code->FreeQuery();
		setup->FreeQuery();

		measurement->CommitTransaction();
//			code->CommitTransaction();

		pDatabase->Close();

		delete staticFeatures;
		delete dynamicFeatures;
		delete pDatabase;
		delete measurement;
		delete code;
		delete setup;
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

	MyC_SVM csvm(&kernel, 100.0, 100.0);
	SVM_Optimizer opt;
	opt.init(csvm.getModel());

	BinaryCompareTrainer svmTrainer(dbPath, csvm);

	std::vector<std::string> features;

	for(size_t i = 0u; i < 3u; ++i)
		features.push_back(toString(i+1));

	svmTrainer.setStaticFeaturesByIndex(features);

	//SVM_Optimizer::dummyError
	ClassificationError err;

	double error = svmTrainer.train(opt, err, 2);
	LOG(INFO) << "Error: " << error << std::endl;
	EXPECT_LT(error, 1.0);

	svmTrainer.saveModel("svm");

	Array<double> fnp = svmTrainer.getFeatureNormalization();
	Evaluator eval1(csvm, fnp);
	return;

	RBFKernel k2(1.0);
	MyC_SVM load(&k2);
	Evaluator eval2 = Evaluator::loadEvaluator(load, "svm");

	Array<double> testPattern(6);
	for(size_t i = 0; i < 6; ++i) {
		testPattern(i) = ((double)(rand()%100)/50)-1;
	}

	size_t trainerSais = svmTrainer.evaluate(testPattern);

	EXPECT_EQ(eval1.binaryCompare(testPattern), trainerSais);
	EXPECT_EQ(eval2.binaryCompare(testPattern), trainerSais);

}

TEST_F(MlTest, MultiSvmTrain) {
	Logger::get(std::cerr, DEBUG);
	const std::string dbPath("linear.db");

	RBFKernel kernel(1.0);

//	LinearKernel lk;
	MyMultiClassSVM msvm(&kernel, 5, 1);
	SVM_Optimizer opt;

	Trainer svmTrainer(dbPath, msvm, GenNNoutput::ML_KEEP_INT);

	std::vector<std::string> features;
	for(size_t i = 0u; i < 3u; ++i)
		features.push_back(toString(i+1));

	svmTrainer.setStaticFeaturesByIndex(features);

	//SVM_Optimizer::dummyError
	ClassificationError err;

	double error = svmTrainer.train(opt, err, 1);
	LOG(INFO) << "Error: " << error << std::endl;
	EXPECT_LE(error, 1.0);

	svmTrainer.saveModel("mcsvm");

	Array<double> fnp = svmTrainer.getFeatureNormalization();
	Evaluator eval1(msvm, fnp);
	RBFKernel k2(1.0);
	MyMultiClassSVM load(&k2, 5, 1);

	Evaluator eval2 = Evaluator::loadEvaluator(load, "mcsvm");

	Array<double> testPattern(3);
	for(size_t i = 0; i < 3; ++i) {
		testPattern(i) = ((double)(rand()%100)/50)-1;
	}

	size_t trainerSais = svmTrainer.evaluate(testPattern);

	EXPECT_EQ(eval1.evaluate(testPattern), trainerSais);
//	EXPECT_EQ(eval2.evaluate(testPattern), trainerSais);

}

TEST_F(MlTest, FfNetTrain) {
	Logger::get(std::cerr, DEBUG);
	const std::string dbPath("linear.db");

	// Create a connection matrix with 3 inputs, 5 output
	// and a single, fully connected hidden layer with
	// 8 neurons:
	Array<int> con;
	size_t nIn = 3, nOut = 5;
	createConnectionMatrix(con, nIn, 8, nOut, true, false, false);

	// declare Machine
	MyFFNet net = MyFFNet(nIn, nOut, con);
	net.initWeights(-0.4, 0.4);

//	std::cout << net.getInputDimension() << std::endl;

	MeanSquaredError err;
	Array<double> in, target;
	Quickprop qprop;
	qprop.initUserDefined(net.getModel(), 1.5, 1.75);
	BFGS bfgs;
	bfgs.initBfgs(net.getModel());
	CG cg;
	RpropPlus rpp;
	rpp.init(net.getModel());
	RpropMinus rpm;
	rpm.init(net.getModel());

	// create trainer
	Trainer qpnn(dbPath, net);//, GenNNoutput::ML_MAP_FLOAT_HYBRID);

	std::vector<std::string> features;

	for(size_t i = 0u; i < 3u; ++i)
		features.push_back(toString(i+1));

	qpnn.setStaticFeaturesByIndex(features);

	double error = qpnn.train(bfgs, err, 4);
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
	size_t nIn = 8, nOut = 2;
	createConnectionMatrix(con, nIn, 8, nOut, true, false, false);

	// declare Machine
	MyFFNet net = MyFFNet(nIn, nOut, con);
	net.initWeights(-0.4, 0.4);
	MeanSquaredError err;
	Array<double> in, target;
	Quickprop qprop;
	qprop.initUserDefined(net.getModel(), 1.5, 1.75);
	BFGS bfgs;
	bfgs.initBfgs(net.getModel());
	CG cg;
	RpropPlus rpp;
	rpp.init(net.getModel());
	RpropMinus rpm;
	rpm.init(net.getModel());

	// create trainer
	BinaryCompareTrainer bct(dbPath, net);//, GenNNoutput::ML_MAP_FLOAT_HYBRID);

	std::vector<std::string> features, filter;

	for(size_t i = 0u; i < 3u; ++i) {
		features.push_back(std::string("Feature") + char('A' + i));
	}
	bct.setStaticFeaturesByName(features);

	bct.setDynamicFeatureByIndex("1");


	for(size_t i = 1u; i < 10u; ++i)
		filter.push_back(std::string() + char('0' + i));
	bct.setFilterCodes(filter);
	bct.setExcludeCode("5");

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
	MyFFNet net;

	Trainer loaded(dbPath, net, GenNNoutput::ML_MAP_TO_N_CLASSES);

	EXPECT_EQ(3u, loaded.loadModel("dummy"));

	Array<double> fnp = loaded.getFeatureNormalization();

	Evaluator eval1(net, fnp);

	Evaluator eval2 = Evaluator::loadEvaluator(net, "dummy");

	size_t f = net.getInputDimension();
	EXPECT_EQ(f, 3u);

	MeanSquaredError errFct;
	std::vector<string> features;
	for(size_t i = 0u; i < 3u; ++i)
		features.push_back(toString(i+1));

	loaded.setStaticFeaturesByIndex(features);

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

TEST_F(MlTest, PCAseparate) {
	Logger::get(std::cerr, DEBUG);
	const std::string dbPath("linear.db");

	size_t nIn = 3;

	// declare Machine
	PcaSeparateExt pse(dbPath);

	std::vector<string> features;
	for(size_t i = 0u; i < nIn; ++i)
		features.push_back(toString(i+1));

	pse.setStaticFeaturesByIndex(features);
	pse.setDynamicFeatureByIndex("1");

	pse.calcPca(1,2);
}

TEST_F(MlTest, PCAcombined) {
	Logger::get(std::cerr, DEBUG);
	const std::string dbPath("linear.db");

	// extract two pcs
	PcaCombinedExt pce(dbPath);

	std::vector<string> features;
	for(size_t i = 0u; i < 3u; ++i)
		features.push_back(toString(i+1));

	pce.setStaticFeaturesByIndex(features);
	pce.setDynamicFeatureByIndex("1");

	pce.calcPca(99.8);

	// Create a connection matrix with 2 inputs, 1 output
	// and a single, fully connected hidden layer with
	// 8 neurons:
	Array<int> con;
	size_t nIn = 2, nOut = 5;
	createConnectionMatrix(con, nIn, 8, nOut, true, false, false);

	// declare Machine
	MyFFNet net = MyFFNet(nIn, nOut, con);
	net.initWeights(-0.4, 0.4);

	MeanSquaredError err;
	CG cg;

	// create trainer
	Trainer qpnn(dbPath, net);//, GenNNoutput::ML_MAP_FLOAT_HYBRID);

	std::vector<std::string> pcaFeatures;

	for(size_t i = 0u; i < 2u; ++i)
		pcaFeatures.push_back(std::string("pca_") + toString(i+1));

	qpnn.setPcaFeaturesByName(pcaFeatures);

	double error = qpnn.train(cg, err, 4);
	LOG(INFO) << "Error: " << error << std::endl;
	EXPECT_LT(error, 1.0);

}

