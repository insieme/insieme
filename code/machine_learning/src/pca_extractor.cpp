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

#include <boost/functional/hash.hpp>

#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"

#include "insieme/machine_learning/machine_learning_exception.h"
#include "insieme/machine_learning/pca_extractor.h"

namespace insieme {
namespace ml {

/*
 * checks if an entry with id already exists in table tableName
 */
bool PcaExtractor::alreadyThere(const int64_t id, const std::string& featureName, const std::string& tableName) {
	Kompex::SQLiteStatement localQuery(pDatabase);
	char query[128];
	sprintf(query, "SELECT name from %s WHERE id = %ld", tableName.c_str(), id);
	std::string collision = localQuery.GetSqlResultString(query);

	if(collision.size() > 0) {
		LOG(WARNING) << "\nFeature " << collision << " with id " << id << " already exists in " << tableName << "!\n\tSkipping feature "
				<< featureName << std::endl;
		return true;
	}
	return false;
}


/*
 * writes the principal components in pcs to the database
 */
void PcaExtractor::writeToDatabase(Array<double>& pcs,  Array<int64>& ids, const std::string& nameTbl, const std::string& dataTbl, bool checkBeforeInsert)
		throw(Kompex::SQLiteException) {
	// sql statements to write into the tables
	std::stringstream insertIntoStaticFeatures;
	insertIntoStaticFeatures << "INSERT INTO " << nameTbl << " (id, name) VALUES(?, ?)";

	Kompex::SQLiteStatement localStmt(pDatabase);
	localStmt.BeginTransaction();
	localStmt.Sql(insertIntoStaticFeatures.str());

	std::stringstream insertIntoCode;

	insertIntoCode << "INSERT INTO " << dataTbl << " (" << dataTbl.substr(0,1) << "id" /* code -> cid, setup -> sid, pca -> pid */
			<< ", fid, value) VALUES(?, ?, ?);";

	pStmt->Sql(insertIntoCode.str());

	boost::hash<std::string> string_hash;
	std::stringstream name;
	name << "pca_";

	if(mangling.size() > 0)
		name << mangling << "_";

	// write names into the name table
	for(size_t i = 0; i < pcs.cols(); ++i) {
		// write into name table
		std::stringstream pcaName;
		pcaName << name.str() << (i+1);
		int64 fid = string_hash(pcaName.str());

		if(checkBeforeInsert && alreadyThere(fid, pcaName.str(), nameTbl))
			throw Kompex::SQLiteException("pca_extractor.cpp", 56, "Name of principal component already exists");

		localStmt.BindInt64(1, fid);
		localStmt.BindString(2, pcaName.str());
		localStmt.Execute();
		localStmt.Reset();

		// write into data table
		for(size_t j = 0; j < pcs.rows(); ++j) {
			pStmt->BindInt64(1, ids(j));
			pStmt->BindInt64(2, fid);
			pStmt->BindDouble(3, pcs(i,j));
			pStmt->Execute();
			pStmt->Reset();
		}
	}

	pStmt->FreeQuery();

	localStmt.FreeQuery();
	localStmt.CommitTransaction();
}

/*
 * writes the principal components in pcs to the code/static_features table in the database
 */
void PcaExtractor::writeToCode(Array<double>& pcs, Array<int64>& ids, bool checkBeforeInsert) throw(MachineLearningException) {
	std::string staticFeatures("static_features");
	std::string code("code");

	try {
		writeToDatabase(pcs, ids, staticFeatures, code);
	} catch(Kompex::SQLiteException& sqle) {
		const std::string err = "\nwriting to static features or code failed\n" ;
		LOG(ERROR) << err << std::endl;
		sqle.Show();
		throw ml::MachineLearningException(err);
	}
}

/*
 * writes the principal components in pcs to the setup/dynnamic_features table in the database
 */
void PcaExtractor::writeToSetup(Array<double>& pcs, Array<int64>& ids, bool checkBeforeInsert) throw(MachineLearningException) {
	std::string dynamicFeatures("dynamic_features");
	std::string setup("setup");

	try {
		writeToDatabase(pcs, ids, dynamicFeatures, setup);
	}
	catch(Kompex::SQLiteException& sqle) {
		const std::string err = "\nwriting to dynamic features or setup failed\n" ;
		LOG(ERROR) << err << std::endl;
		sqle.Show();
		throw ml::MachineLearningException(err);
	}
}

/*
 * writes the principal components in pcs to the pca/pc_features table in the database
 */
void PcaExtractor::writeToPca(Array<double>& pcs, Array<int64>& ids, bool checkBeforeInsert) throw(MachineLearningException) {
	try {
		// check if pca tables do aleady exist. If not create them
		if(pStmt->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='pca_features'") < 0)
			pStmt->SqlStatement("CREATE TABLE pca_features (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL)");
		if(pStmt->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='principal_component'") < 0)
			pStmt->SqlStatement("CREATE TABLE principal_component (pid INTEGER, fid INTEGER REFERENCES pca_features ON DELETE RESTRICT ON UPDATE RESTRICT, \
				value DOUBLE NOT NULL, PRIMARY KEY(pid, fid))");

		std::string pcaFeatures("pca_features");
		std::string pca("principal_component");

		writeToDatabase(pcs, ids, pcaFeatures, pca);
	}
	catch(Kompex::SQLiteException& sqle) {
		const std::string err = "\nwriting to pca features or principal_component failed\n" ;
		LOG(ERROR) << err << std::endl;
		sqle.Show();
		throw ml::MachineLearningException(err);
	}
}

/*
 * applies query on the given database and stores the read data in in
 */
size_t PcaExtractor::readDatabase(Array<double>& in, Array<int64>& ids, size_t nFeatures, std::string query, size_t nIds) throw(ml::MachineLearningException) {
	if(query.size() == 0)
		genDefaultQuery();

	try {
		Kompex::SQLiteStatement *localStmt = new Kompex::SQLiteStatement(pDatabase);

		localStmt->Sql(query);

		size_t nRows = localStmt->GetNumberOfRows();
		in = Array<double>(nRows, nFeatures);
		ids = Array<int64>(nRows, nIds);
		LOG(INFO) << "Queried Rows: " << nRows << ", Number of features: " << nFeatures << std::endl;

		if(nRows == 0)
			throw MachineLearningException("No dataset for the requested features could be found");

		// load data
		size_t i = 0;
		// fetch all results
		while(localStmt->FetchRow()){
			// construct feature vectors
			for(size_t j = 0; j < nFeatures; ++j) {
				in(i, j) = localStmt->GetColumnDouble(j);
			}

			// read the ids from the query
			for(size_t j = 0; j < nIds; ++j) {
				ids(i, j) = localStmt->GetColumnInt64(nFeatures + j);
			}

			++i;
		}

		// reset the prepared statement
		localStmt->Reset();

		// do not forget to clean-up
		localStmt->FreeQuery();
		delete localStmt;

	//	FeaturePreconditioner fp;
	//	featureNormalization = fp.normalize(in, -1, 1);
		return nRows;

	} catch(Kompex::SQLiteException& sqle) {
		const std::string err = "\nSQL query for features failed\n" ;
		LOG(ERROR) << err << std::endl;
		sqle.Show();
		throw ml::MachineLearningException(err);
	}

	return 0u;
}

/*
 * generates a model of type AffineLinearMap that is initialized with the feature's eigenvectors and
 * can be used to generate the PCs
 */
void PcaExtractor::genPCAmodel(AffineLinearMap& model, Array<double>& data, Array<double>& eigenvalues, Array<double>& eigenvectors){
	PCA pca;
	pca.init(model);
	pca.optimize(model, data, eigenvalues, eigenvectors);
}

/*
 * calculates the principal components of the 2D shaped (patterns x features) data using a model
 * which has been previously initialized with PcaExtractor::genPCAmodel
 */
Array<double> PcaExtractor::genPCs(AffineLinearMap& model, Array<double>& data) {
	Array<double> pcs(model.getOutputDimension(), data.rows());

	model.model(data, pcs);
	return pcs;
}


PcaExtractor::PcaExtractor(const std::string& myDbPath, const std::string& manglingPostfix)
	: pDatabase(new Kompex::SQLiteDatabase(myDbPath, SQLITE_OPEN_READWRITE, 0)),
	  pStmt(new Kompex::SQLiteStatement(pDatabase)), mangling(manglingPostfix) { }

PcaExtractor::~PcaExtractor() {
	delete pStmt;
	delete pDatabase;
}

void PcaExtractor::setStaticFeaturesByIndex(const std::vector<std::string>& featureIndices) {
	for(std::vector<std::string>::const_iterator I = featureIndices.begin(); I != featureIndices.end(); ++I)
		staticFeatures.push_back(*I);
}
void PcaExtractor::setStaticFeatureByIndex(const std::string featureIndex) {
	staticFeatures.push_back(featureIndex);
}

void PcaExtractor::setStaticFeaturesByName(const std::vector<std::string>& featureNames){
	for(std::vector<std::string>::const_iterator I = featureNames.begin(); I != featureNames.end(); ++I) {
		setStaticFeatureByName(*I);
	}
}

void PcaExtractor::setStaticFeatureByName(const std::string featureName){
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

void PcaExtractor::setDynamicFeaturesByIndex(const std::vector<std::string>& featureIndices) {
	for(std::vector<std::string>::const_iterator I = featureIndices.begin(); I != featureIndices.end(); ++I)
		dynamicFeatures.push_back(*I);
}
void PcaExtractor::setDynamicFeatureByIndex(const std::string featureIndex) {
	dynamicFeatures.push_back(featureIndex);
}

void PcaExtractor::setDynamicFeaturesByName(const std::vector<std::string>& featureNames){
	for(std::vector<std::string>::const_iterator I = featureNames.begin(); I != featureNames.end(); ++I) {
		setDynamicFeatureByName(*I);
	}
}

void PcaExtractor::setDynamicFeatureByName(const std::string featureName){
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

/**
 * resets the feature arrays as well as the stored query
 */
void PcaExtractor::restetFeatures() {
	staticFeatures.clear();
	dynamicFeatures.clear();
	query = "";
}


} // end namespace ml
} // end namespace insieme
