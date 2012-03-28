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

#include <iostream>
#include <vector>

#include "KompexSQLitePrerequisites.h"
#include "KompexSQLiteDatabase.h"
#include "KompexSQLiteStatement.h"
#include "KompexSQLiteException.h"

#include "insieme/machine_learning/database_utils.h"

using namespace Kompex;

namespace insieme {
namespace ml {

void Database::createDatabase(const std::string& path, bool clear) {
	try {
		dBase = new SQLiteDatabase(path, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, 0);
		staticFeaturesStmt = new SQLiteStatement(dBase);
		codeStmt = new SQLiteStatement(dBase);
		dynamicFeaturesStmt = new SQLiteStatement(dBase);
		setupStmt = new SQLiteStatement(dBase);
		measurementsStmt = new SQLiteStatement(dBase);

		if(clear) {// delete tables if already existing
			if(codeStmt->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='static_features'") >= 0)
				codeStmt->SqlStatement("DROP TABLE static_features");
			if(codeStmt->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='dynamic_features'") >= 0)
				codeStmt->SqlStatement("DROP TABLE dynamic_features");
			if(codeStmt->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='code'") >= 0)
				codeStmt->SqlStatement("DROP TABLE code");
			if(codeStmt->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='setup'") >= 0)
				codeStmt->SqlStatement("DROP TABLE setup");
			if(codeStmt->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='measurement'") >= 0)
				codeStmt->SqlStatement("DROP TABLE measurement");
		}
		// create tables
		if(codeStmt->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='static_features'") < 0)
			codeStmt->SqlStatement("CREATE TABLE static_features (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL)");

		if(codeStmt->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='dynamic_features'") < 0)
			codeStmt->SqlStatement("CREATE TABLE dynamic_features (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL)");

		if(codeStmt->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='code'") < 0)
			codeStmt->SqlStatement("CREATE TABLE code (cid INTEGER, fid INTEGER REFERENCES static_features ON DELETE RESTRICT ON UPDATE RESTRICT, \
				value DOUBLE NOT NULL, PRIMARY KEY(cid, fid))");

		if(codeStmt->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='setup'") < 0)
			codeStmt->SqlStatement("CREATE TABLE setup (sid INTEGER, fid INTEGER REFERENCES dynamic_features ON DELETE RESTRICT ON UPDATE RESTRICT, \
				value DOUBLE NOT NULL, PRIMARY KEY(sid, fid))");

		if(codeStmt->GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='measurement'") < 0) {
			std::stringstream qss;
			qss << "CREATE TABLE measurement (id INTEGER PRIMARY KEY, ts TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
					cid INTEGER REFERENCES code ON DELETE RESTRICT ON UPDATE RESTRICT, sid INTEGER REFERENCES setup ON DELETE RESTRICT ON UPDATE RESTRICT";

			for(auto I = measurements.begin(); I != measurements.end(); ++I) {
				qss << ", " << *I << " DOUBLE";
			}
			qss << ")";

			codeStmt->SqlStatement(qss.str());
		}
	} catch(SQLiteException& e) {
		std::cerr << "Error in createDatabase:\n\t" << e.GetString() << std::endl;
	}
}

/*
 * creating a database, at path with a column for each measurement in the measurement table
 */
Database::Database(const std::string& path, std::vector<std::string>& allMeasurements, bool clear) {
	measurements = allMeasurements;
	createDatabase(path, clear);
}

/*
 * creating a database, using time as the only measurement
 */
Database::Database(const std::string& path, bool clear) {
	std::vector<std::string> timeMeasurement;
	timeMeasurement.push_back("time");
	measurements = timeMeasurement;
	createDatabase(path, clear);
}

/*
 * closes the database
 */
Database::~Database() {
	dBase->Close();
	delete staticFeaturesStmt;
	delete codeStmt;
	delete dynamicFeaturesStmt;
	delete setupStmt;
	delete measurementsStmt;
	delete dBase;
}

/*
 * setups and starts an insertion sql statement for the static features table
 */
void Database::beginStaticFeaturesTransaction() {
	try {
		staticFeaturesStmt->BeginTransaction();
		// sql statements to write into the tables
		staticFeaturesStmt->Sql("INSERT INTO static_features (id, name) VALUES(?, ?);");
	} catch(SQLiteException& e) {
		std::cerr << "Error in beginStaticFeaturesTransaction:\n\t" << e.GetString() << std::endl;
	}
}

/*
 * checks if an entry with id already exists in table tableName
 */
bool Database::alreadyThere(const int64_t id, const std::string& featureName, const std::string& tableName) {
	Kompex::SQLiteStatement localQuery(dBase);
	char query[128];
	sprintf(query, "SELECT name from %s WHERE id = %ld", tableName.c_str(), id);
	std::string collision = localQuery.GetSqlResultString(query);

	if(collision.size() > 0) {
		std::cout << "\nWARNING! Feature " << collision << " with id " << id << " already exists in " << tableName << "!\n\tSkipping feature "
				<< featureName << std::endl;
		return true;
	}
	return false;
}

bool Database::alreadyThere(const int64_t id1, const int64_t id2, const std::string& tableName) {
	Kompex::SQLiteStatement localQuery(dBase);
	const char* idNames[2];
	if(tableName == "code") {
		idNames[0] = "cid", idNames[1] = "fid";
	} else if(tableName == "setup") {
		idNames[0] = "sid", idNames[1] = "fid";
	} else {
		idNames[0] = "cid", idNames[1] = "sid";
	}

	char query[128];
	sprintf(query, "SELECT value from %s WHERE %s = %ld AND %s = %ld", tableName.c_str(), idNames[0], id1, idNames[1], id2);
	std::string collision = localQuery.GetSqlResultString(query);

	if(collision.size() > 0) {
		std::cout << "\nWARNING! Feature with " << idNames[0] << " and " << id1 << " and " << idNames[1] << " " << id2 <<
				" already exists in " << tableName << "!\n\tSkipping this feature " << std::endl;
		return true;
	}
	return false;
}

/*
 * inserts one element into the static feature table
 */
void Database::insertIntoStaticFeatures(int64_t id, std::string featureName, bool checkBeforeInsert) {
	try {
		if(checkBeforeInsert && alreadyThere(id, featureName, "static_features"))
			return;

		staticFeaturesStmt->BindInt64(1, id);
		staticFeaturesStmt->BindString(2, featureName);
		staticFeaturesStmt->Execute();
		staticFeaturesStmt->Reset();
	} catch(SQLiteException& e) {
		std::cerr << "Error in insertIntoStaticFeatures:\n\t" << e.GetString() << std::endl;
	}
}

/*
 * commits an sql statement to the static features table
 */
void Database::commitStaticFeaturesTransaction() {
	try {
		staticFeaturesStmt->FreeQuery();
		staticFeaturesStmt->CommitTransaction();
	} catch(SQLiteException& e) {
		std::cerr << "Error in commitStaticFeaturesTransaction:\n\t" << e.GetString() << std::endl;
	}
}

/*
 * setups and starts an insertion sql statement for the dynamic features table
 */
void Database::beginDynamicFeaturesTransaction() {
	try {
		dynamicFeaturesStmt->BeginTransaction();
		// sql statements to write into the tables
		dynamicFeaturesStmt->Sql("INSERT INTO dynamic_features (id, name) VALUES(?, ?);");
	} catch(SQLiteException& e) {
		std::cerr << "Error in beginDynamicFeaturesTransaction:\n\t" << e.GetString() << std::endl;
	}
}

/*
 * inserts one element into the dynamic feature table
 */
void Database::insertIntoDynamicFeatures(int64_t id, std::string featureName, bool checkBeforeInsert) {
	try {
		if(checkBeforeInsert && alreadyThere(id, featureName, "dynamic_features"))
			return;

		dynamicFeaturesStmt->BindInt64(1, id);
		dynamicFeaturesStmt->BindString(2, featureName);
		dynamicFeaturesStmt->Execute();
		dynamicFeaturesStmt->Reset();
	} catch(SQLiteException& e) {
		std::cerr << "Error in insertIntoDynamicFeatures:\n\t" << e.GetString() << std::endl;
	}
}

/*
 * commits an sql statement to the dynamic features table
 */
void Database::commitDynamicFeaturesTransaction() {
	try {
		dynamicFeaturesStmt->FreeQuery();
		dynamicFeaturesStmt->CommitTransaction();
	} catch(SQLiteException& e) {
		std::cerr << "Error in commitDynamicFeaturesTransaction:\n\t" << e.GetString() << std::endl;
	}
}

/*
 * setups and starts sql statements for the code, setup and measurement tables
 */
void Database::beginDataTransaction() {
	try {
		measurementsStmt->BeginTransaction();
		std::stringstream mStmt;
		mStmt << "INSERT INTO measurement (cid, sid";
		for(auto I = measurements.begin(); I != measurements.end(); ++I) {
			mStmt << ", " << *I;
		}
		mStmt << ") VALUES (?, ";
		for(size_t i = measurements.size(); i > 0; --i)
			mStmt << "?, ";
		mStmt << "?)";

		measurementsStmt->Sql(mStmt.str());
		codeStmt->Sql("INSERT INTO code (cid, fid, value) VALUES(?, ?, ?);");
		setupStmt->Sql("INSERT INTO setup (sid, fid, value) VALUES(?, ?, ?);");
	} catch(SQLiteException& e) {
		std::cerr << "Error in beginDataTransaction:\n\t" << e.GetString() << std::endl;
	}
}

/*
 * inserts one element into the measurement table
 */
void Database::insertIntoMeasurements(int64_t cid, int64_t sid, std::vector<double>& values, bool checkBeforeInsert) {
	try {
		if(checkBeforeInsert && alreadyThere(cid, sid, "measurement"))
			return;

		measurementsStmt->BindInt64(1, cid);
		measurementsStmt->BindInt64(2, sid);

		for(size_t i = 0; i < values.size(); ++i) {
		    measurementsStmt->BindDouble(i+3, values.at(i));
		}
		// write measurement result to database
		measurementsStmt->Execute();
		measurementsStmt->Reset();
	} catch(SQLiteException& e) {
		std::cerr << "Error in insertIntoMeasurements:\n\t" << e.GetString() << std::endl;
	}
}

/*
 * inserts one element into the code table
 */
void Database::insertIntoCode(int64_t cid, int64_t fid, double value, bool checkBeforeInsert) {
	try {
		if(checkBeforeInsert && alreadyThere(cid, fid, "code"))
			return;

		codeStmt->BindInt64(1, cid);
		codeStmt->BindInt64(2, fid);
		codeStmt->BindDouble(3, value);
		codeStmt->Execute();
		codeStmt->Reset();
	} catch(SQLiteException& e) {
		std::cerr << "Error in insertIntoCode:\n\t" << e.GetString() << std::endl;
	}
}

/*
 * inserts one element into the setup table
 */
void Database::insertIntoSetup(int64_t sid, int64_t fid, double value, bool checkBeforeInsert) {
	try {
		if(checkBeforeInsert && alreadyThere(sid, fid, "setup"))
			return;

		setupStmt->BindInt64(1, sid);
	    setupStmt->BindInt64(2, fid);
	    setupStmt->BindDouble(3, value);
	    setupStmt->Execute();
	    setupStmt->Reset();
	} catch(SQLiteException& e) {
		std::cerr << "Error in insertIntoSetup:\n\t" << e.GetString() << std::endl;
	}
}

/*
 * commits an sql statement to the code, setup and measurement tables
 */
void Database::commitDataTransaction() {
	try {
		measurementsStmt->FreeQuery();
		codeStmt->FreeQuery();
		setupStmt->FreeQuery();

		measurementsStmt->CommitTransaction();
	} catch(SQLiteException& e) {
		std::cerr << "Error in commitDataTransaction:\n\t" << e.GetString() << std::endl;
	}
}


SQLiteDatabase* createDatabase(const std::string path) {
	SQLiteDatabase* dBase = new SQLiteDatabase(path, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, 0);

	SQLiteStatement sqlStatement = SQLiteStatement(dBase);
	// delete tables if already existing
	if(sqlStatement.GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='static_features'") >= 0)
		sqlStatement.SqlStatement("DROP TABLE static_features");
	if(sqlStatement.GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='dynamic_features'") >= 0)
		sqlStatement.SqlStatement("DROP TABLE dynamic_features");
	if(sqlStatement.GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='measurement'") >= 0)
		sqlStatement.SqlStatement("DROP TABLE measurement");
	if(sqlStatement.GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='code'") >= 0)
		sqlStatement.SqlStatement("DROP TABLE code");
	if(sqlStatement.GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='setup'") >= 0)
		sqlStatement.SqlStatement("DROP TABLE setup");

	// create tables
	sqlStatement.SqlStatement("CREATE TABLE static_features (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL)");
	sqlStatement.SqlStatement("CREATE TABLE code (cid INTEGER, fid INTEGER REFERENCES static_features ON DELETE RESTRICT ON UPDATE RESTRICT, \
		value INTEGER NOT NULL, PRIMARY KEY(cid, fid))");
	sqlStatement.SqlStatement("CREATE TABLE dynamic_features (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL)");
	sqlStatement.SqlStatement("CREATE TABLE setup (sid INTEGER, fid INTEGER REFERENCES dynamic_features ON DELETE RESTRICT ON UPDATE RESTRICT, \
		value INTEGER NOT NULL, PRIMARY KEY(sid, fid))");
	sqlStatement.SqlStatement("CREATE TABLE measurement (id INTEGER PRIMARY KEY, ts TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
			cid INTEGER REFERENCES code ON DELETE RESTRICT ON UPDATE RESTRICT, sid INTEGER REFERENCES setup ON DELETE RESTRICT ON UPDATE RESTRICT, \
			time DOUBLE)");

	return dBase;
}

/**
 * closes and frees a database
 */
void closeDatabase(SQLiteDatabase* database) {
	database->Close();
	delete database;
}


} // end namespace ml
} // end namespace insieme
