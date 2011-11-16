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

#include <gtest/gtest.h>

#include "KompexSQLitePrerequisites.h"
#include "KompexSQLiteDatabase.h"
#include "KompexSQLiteStatement.h"
#include "KompexSQLiteException.h"

#include "insieme/machine_learning/inputs.h"
#include "insieme/utils/numeric_cast.h"

TEST(CreateDbTest, ReadFromFile) {
	// open file
	std::ifstream file;
	file.open(std::string(IN_DIR) + "training.small.random", std::ios::in);

	EXPECT_TRUE(file.is_open());

	try
	{
		// create and open database
		Kompex::SQLiteDatabase *pDatabase = new Kompex::SQLiteDatabase("small.db", SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, 0);
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

		// sql statements to write into the tables
		features->Sql("INSERT INTO features (name, static) VALUES(?, ?);");
		measurement->Sql("INSERT INTO measurement (time, power, cost) VALUES (?, ?, ?)");
		data->Sql("INSERT INTO data (fid, mid, value) VALUES(?, ?, ?);");

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

		// read values form file into database

		std::string line, buf;
		int f[4] ={1, 2, 3, 4};
		double m[3] = {0.0, 0.1, 0.2};
		int mid = 0;

		// loop over each line in the file
		while(getline(file, line)) {
			std::stringstream linestream(line);
			// read the three features
			for(int fid = 0; fid < 4; ++fid) {
			    EXPECT_TRUE(linestream >> buf);
			    f[fid] = insieme::utils::numeric_cast<int>(buf);
			}

			// read the three measurements and prepare to write into database
			for(int i = 0; i < 3; ++i) {
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
			for(int fid = 0; fid < 4; ++fid) {
				data->BindInt(1, fid+1);
				data->BindInt(2, mid);
				data->BindInt(3, f[fid]);
				data->Execute();
				data->Reset();
			}
		}

		EXPECT_EQ(features->GetSqlResultInt("SELECT MAX(m.id) FROM measurement m"), 100);

		measurement->FreeQuery();
		data->FreeQuery();

		pDatabase->Close();
	} catch (Kompex::SQLiteException &exception)
	{
		std::cerr << "\nException Occured" << std::endl;
		exception.Show();
	}

	file.close();
}
