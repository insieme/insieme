/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include <sstream>
#include <vector>
#include <fstream>
#include <regex>
#include <cmath>

#include <boost/lexical_cast.hpp>

#include "insieme/utils/logging.h"

namespace insieme {
namespace driver {
namespace perf_reg {
namespace tools {

	using namespace std;

	string vec2string(vector<string> &vec);
	time_t str2time(const string &in);
	double str2double(const string &in);
	int str2int(const string &in);

	bool isDouble(const string &in);
	bool isInt(const string &in);

	double roundDouble(double in, double precision = 3);

	bool fileExists(const string &filename);
	bool allFilesExist(const vector<string> &filenames);

	bool extractSettingsValue(const string &in, const string &key, string &out);
	bool extractTimestamp(const string &in, time_t &out);

	bool quickWriteFile(const string filename, const string input);
	bool quickRemoveFile(const string filename);

} // namespace tools
} // namespace perf_reg
} // namespace driver
} // namespace insieme
