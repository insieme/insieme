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

#include <inttypes.h>

//forward declarations
namespace Kompex {
class SQLiteDatabase;
class SQLiteStatement;
}

namespace insieme {
namespace ml {

class Database {
private:
	Kompex::SQLiteDatabase* dBase;
	Kompex::SQLiteStatement* staticFeaturesStmt;
	Kompex::SQLiteStatement* codeStmt;
	Kompex::SQLiteStatement* dynamicFeaturesStmt;
	Kompex::SQLiteStatement* setupStmt;
	Kompex::SQLiteStatement* measurementsStmt;

	std::vector<std::string> measurements;

	/*
	 * creates a database with four tables: static_features, code, dynamic_featueres, setup, measurements
	 * @param path the path to the file where the database should be created
	 * @param clear indicates if a database existing on the given path should be cleared before inserting the new data
	 */
	void createDatabase(const std::string& path, bool clear = false);

public:
	/*
	 * creating a database, at path with a column for each measurement in the measurement table
	 * @param path the file path where to create the database
	 * @param measurments a vector containing all the names of the measured parameters
	 * @param clear indicates if a database existing on the given path should be cleared before inserting the new data
	 */
	Database(const std::string& path, std::vector<std::string>& measurements, bool clear = false);

	/*
	 * creating a database, using time as the only measurement
	 * @param path the file path where to create the database
	 * @param clear indicates if a database existing on the given path should be cleared before inserting the new data
	 */
	Database(const std::string& path, bool clear = false);

	/*
	 * closes the database
	 */
	~Database();

	/*
	 * setups and starts an insertion sql statement for the static features table
	 */
	void beginStaticFeaturesTransaction();

	/*
	 * inserts one element into the feature table
	 * @param id the unique id of the feature
	 * @param featureName the name of the feature
	 */
	void insertIntoStaticFeatures(int64_t id, std::string featureName);

	/*
	 * commits an sql statement to the static features table
	 */
	void commitStaticFeaturesTransaction();

	/*
	 * setups and starts an insertion sql statement for the dynamic features table
	 */
	void beginDynamicFeaturesTransaction();

	/*
	 * inserts one element into the feature table
	 * @param id the unique id of the feature
	 * @param featureName the name of the feature
	 */
	void insertIntoDynamicFeatures(int64_t id, std::string featureName);

	/*
	 * commits an sql statement to the dynamic features table
	 */
	void commitDynamicFeaturesTransaction();

	/*
	 * inserts one element into the measurements table
	 * @param cid the code id of the given measurements
	 * @param sid the setup id of the given measurements
	 * @param values a vector containing all measured values. They must be in the same order as the columns in the measrurements table
	 */
	void insertIntoMeasurements(int64_t cid, int64_t sid, std::vector<double>& values);
	/*
	 * inserts one element into the code table
	 * @param cid the code id
	 * @param fid the feature id
	 * @param value the value of the given feature in a certain code
	 */
	void insertIntoCode(int64_t cid, int64_t fid, double value);

	/*
	 * inserts one element into the setup table
	 * @param sid the setup id
	 * @param fid the feature id
	 * @param value the value of the given feature at the specific setup
	 */
	void insertIntoSetup(int64_t sid, int64_t fid, double value);

	/*
	 * setups and starts sql statements for the code, setup and measurement tables
	 */
	void beginDataTransaction();

	/*
	 * commits an sql statement to the code, setup and measurement tables
	 */
	void commitDataTransaction();
};

/**
 * creates a opens a database on the given path, deletes the tables in it (if any) and creates new ones
 * @param path the path of the SQLite database file
 * @return A pointer to a KompexSQLiteDatabase object
 */
Kompex::SQLiteDatabase* createDatabase(const std::string path);

/**
 * closes and frees a database
 * @param database a pointer to the database to be closed
 */
void closeDatabase(Kompex::SQLiteDatabase* database);

} // end namespace ml
} // end namespace insieme
