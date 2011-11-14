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

#include "KompexSQLitePrerequisites.h"
#include "KompexSQLiteDatabase.h"
#include "KompexSQLiteStatement.h"
#include "KompexSQLiteException.h"


int mains() {
	try
	{
		// create and open database
		Kompex::SQLiteDatabase *pDatabase = new Kompex::SQLiteDatabase("data.db", SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, 0);
		// create statement instance for sql queries/statements
		Kompex::SQLiteStatement *pStmt = new Kompex::SQLiteStatement(pDatabase);

		// create tables
		pStmt->SqlStatement("CREATE TABLE features (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL,	static BOOL NOT NULL)");
		pStmt->SqlStatement("CREATE TABLE measurement (id INTEGER PRIMARY KEY, ts TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, value INTEGER)");
		pStmt->SqlStatement("CREATE TABLE data (fid INTEGER REFERENCES features ON DELETE RESTRICT ON UPDATE RESTRICT, \
			mid INTEGER REFERENCES measurement ON DELETE RESTRICT ON UPDATE RESTRICT, \
			value INTEGER NOT NULL, \
			PRIMARY KEY(fid, mid))");


		// add some data to features
		pStmt->Sql("INSERT INTO features (name, static) VALUES(?, ?);");
		pStmt->BindString(1, "FeatureA");
		pStmt->BindBool(2, false);
		pStmt->Execute();
		pStmt->Reset();

		pStmt->BindString(1, "FeatureB");
		pStmt->BindBool(2, true);
		pStmt->Execute();
		pStmt->Reset();

		pStmt->BindString(1, "FeatureC");
		pStmt->BindBool(2, false);
		pStmt->ExecuteAndFree();

		// add some data to measurement
		pStmt->SqlStatement("INSERT INTO measurement (ts, value) VALUES (CURRENT_TIMESTAMP, 0)");
		pStmt->SqlStatement("INSERT INTO measurement (ts, value) VALUES (CURRENT_TIMESTAMP, 1)");

		// add some data to data
		pStmt->Sql("INSERT INTO data (fid, mid, value) VALUES(?, ?, ?);");
		pStmt->BindInt(1, 1);
		pStmt->BindInt(2, 1);
		pStmt->BindInt(3, 15);
		pStmt->Execute();
		pStmt->Reset();

		pStmt->BindInt(1, 1);
		pStmt->BindInt(2, 2);
		pStmt->BindInt(3, 20);
		pStmt->Execute();
		pStmt->Reset();

		pStmt->BindInt(1, 2);
		pStmt->BindInt(2, 2);
		pStmt->BindInt(3, 4);
		pStmt->Execute();
		pStmt->Reset();

		pStmt->BindInt(1, 2);
		pStmt->BindInt(2, 1);
		pStmt->BindInt(3, 10);
		pStmt->Execute();
		pStmt->FreeQuery();

	} catch (Kompex::SQLiteException &exception)
	{
		std::cerr << "\nException Occured" << std::endl;
		exception.Show();
	}


	return 0;
}
