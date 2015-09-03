/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include <algorithm>
#include <string>

#include <fstream>
#include <sstream>

#include <boost/filesystem.hpp>

#include "insieme/core/parser3/ir_parser.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/channel.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/io.h"

#include "insieme/utils/config.h"

namespace insieme {
namespace core {
namespace parser3 {

	// the directory to load input files from
	const auto ROOT_DIR = SRC_ROOT_DIR "core/test/parser3/inputs/";


	using std::string;
	namespace fs = boost::filesystem;

	vector<string> getInputFiles();

	// the type definition (specifying the parameter type)
	class IRParserTest : public ::testing::TestWithParam<string> {};

	// define the test case pattern
	TEST_P(IRParserTest, ReadFileTest) {
		string file = ROOT_DIR + string(GetParam());
		std::cout << "Testing: " << GetParam() << std::endl;

		SCOPED_TRACE(file);

		// check whether file is present
		EXPECT_TRUE(fs::exists(file)) << "File " << file << " should exist!";
		ASSERT_TRUE(fs::exists(file));

		// load file
		std::stringstream ss;
		ss << std::fstream(file).rdbuf();

		// parse file
		NodeManager manager;

		// create default definitions and alias maps
		definition_map definitions;
		type_alias_map aliases;

		// add up definitions
		for(const auto& cur : manager.getLangExtension<lang::ArrayExtension>().getDefinedSymbols()) definitions.insert(cur);
		for(const auto& cur : manager.getLangExtension<lang::ReferenceExtension>().getDefinedSymbols()) definitions.insert(cur);
		for(const auto& cur : manager.getLangExtension<lang::ParallelExtension>().getDefinedSymbols()) definitions.insert(cur);
		for(const auto& cur : manager.getLangExtension<lang::ChannelExtension>().getDefinedSymbols()) definitions.insert(cur);
		for(const auto& cur : manager.getLangExtension<lang::InputOutputExtension>().getDefinedSymbols()) definitions.insert(cur);

		for(const auto& cur : manager.getLangExtension<lang::ArrayExtension>().getTypeAliases()) aliases.insert(cur);
		for(const auto& cur : manager.getLangExtension<lang::ReferenceExtension>().getTypeAliases()) aliases.insert(cur);
		for(const auto& cur : manager.getLangExtension<lang::ChannelExtension>().getTypeAliases()) aliases.insert(cur);
		for(const auto& cur : manager.getLangExtension<lang::ParallelExtension>().getTypeAliases()) aliases.insert(cur);
		for(const auto& cur : manager.getLangExtension<lang::InputOutputExtension>().getTypeAliases()) aliases.insert(cur);


		NodePtr res = parse_program(manager, ss.str(), true, definitions, aliases);

		// if(res) dumpColor(res);

		// it should have produced a result
		ASSERT_TRUE(res);
		// dumpColor(res);

		// run semantic checks on files
		auto msg = checks::check(res);
		EXPECT_TRUE(msg.empty()) << msg;
	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(InputFileChecks, IRParserTest, ::testing::ValuesIn(getInputFiles()));


	vector<string> getInputFiles() {
		vector<string> res;

		fs::path root(ROOT_DIR);
		assert_true(fs::is_directory(root));

		for(auto it = fs::directory_iterator(root); it != fs::directory_iterator(); ++it) {
			fs::path file = it->path();
			if(file.extension().string() == ".ir") { res.push_back(file.filename().string()); }
		}
		std::sort(res.begin(), res.end());

		return res;
	}

} // end namespace parser2
} // end namespace core
} // end namespace insieme
