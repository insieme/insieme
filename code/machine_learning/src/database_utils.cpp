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

#include "insieme/machine_learning/database_utils.h"

namespace insieme {
namespace ml {

Kompex::SQLiteDatabase* createDatabase(const std::string path) {
	Kompex::SQLiteDatabase* dBase = new Kompex::SQLiteDatabase(path, SQLITE_OPEN_READWRITE | SQLITE_OPEN_CREATE, 0);

	Kompex::SQLiteStatement creator = Kompex::SQLiteStatement(dBase);
	// delete tables if already existing
	if(creator.GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='static_features'") >= 0)
		creator.SqlStatement("DROP TABLE static_features");
	if(creator.GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='dynamic_features'") >= 0)
		creator.SqlStatement("DROP TABLE dynamic_features");
	if(creator.GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='measurement'") >= 0)
		creator.SqlStatement("DROP TABLE measurement");
	if(creator.GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='code'") >= 0)
		creator.SqlStatement("DROP TABLE code");
	if(creator.GetSqlResultInt("SELECT name FROM sqlite_master WHERE name='setup'") >= 0)
		creator.SqlStatement("DROP TABLE setup");

	// create tables
	creator.SqlStatement("CREATE TABLE static_features (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL)");
	creator.SqlStatement("CREATE TABLE code (cid INTEGER, fid INTEGER REFERENCES static_features ON DELETE RESTRICT ON UPDATE RESTRICT, \
		value INTEGER NOT NULL, PRIMARY KEY(cid, fid))");
	creator.SqlStatement("CREATE TABLE dynamic_features (id INTEGER NOT NULL PRIMARY KEY, name VARCHAR(50) NOT NULL)");
	creator.SqlStatement("CREATE TABLE setup (sid INTEGER, fid INTEGER REFERENCES static_features ON DELETE RESTRICT ON UPDATE RESTRICT, \
		value INTEGER NOT NULL, PRIMARY KEY(sid, fid))");
	creator.SqlStatement("CREATE TABLE measurement (id INTEGER PRIMARY KEY, ts TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP, \
			cid INTEGER REFERENCES code ON DELETE RESTRICT ON UPDATE RESTRICT, sid INTEGER REFERENCES setup ON DELETE RESTRICT ON UPDATE RESTRICT, \
			time DOUBLE)");

	return dBase;
}

} // end namespace ml
} // end namespace insieme
