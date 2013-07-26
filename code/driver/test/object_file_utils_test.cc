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

#include <gtest/gtest.h>

#include "insieme/driver/object_file_utils.h"

#include <boost/filesystem/operations.hpp>

#include "insieme/driver/driver_config.h"
#include "insieme/frontend/frontend.h"

namespace fe = insieme::frontend;
namespace fs = boost::filesystem;

namespace insieme {
namespace driver {

	TEST(ObjectFile, HelloWorldTest) {
		core::NodeManager mgr;

		fe::ConversionJob job(SRC_DIR "/hello_world.c");
		auto unit = job.toTranslationUnit(mgr);

		// save tu to temporary file
		auto file = fs::unique_path(fs::temp_directory_path() / "tmp%%%%%%%%.o");

		// save translation unit
		saveLib(unit, file);

		// check validity
		EXPECT_TRUE(fs::exists(file));
		EXPECT_TRUE(isInsiemeLib(file));

		// this one should fail ..
		EXPECT_FALSE(isInsiemeLib(SRC_DIR "/hello_world.c"));

		// reload translation unit
		core::NodeManager mgr2;
		auto tu = loadLib(mgr2, file);

		EXPECT_EQ(toString(tu), toString(unit));

		// cleanup
		if (fs::exists(file)) {
			fs::remove(file);
		}
	}

} // end namespace driver
} // end namespace insieme

