/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */

#include <gtest/gtest.h>

#include <algorithm>
#include <string>

#include <fstream>
#include <sstream>

#include <boost/filesystem.hpp>

#include "insieme/core/parser/ir_parser.h"
#include "insieme/core/checks/full_check.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/channel.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/io.h"

#include "insieme/utils/config.h"
#include "insieme/utils/gtest_utils.h"

namespace insieme {
namespace core {
namespace parser {

	// the directory to load input files from
	const auto ROOT_DIR = utils::getInsiemeSourceRootDir() + "core/test/parser/inputs/";


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
		ss << std::fstream(file, std::ios_base::in).rdbuf();

		// parse file
		NodeManager manager;

		// create default definitions and alias maps
		DefinitionMap definitions;
		TypeAliasMap aliases;

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


		NodePtr res = parseProgram(manager, ss.str(), true, definitions, aliases);

		// if(res) dumpColor(res);

		// it should have produced a result
		ASSERT_TRUE(res);
		// dumpColor(res);

		// run semantic checks on files
		auto msg = checks::check(res);
		EXPECT_TRUE(msg.empty()) << msg;
	}

	// instantiate the test case
	INSTANTIATE_TEST_CASE_P(InputFileChecks,
	                        IRParserTest,
	                        ::testing::ValuesIn(utils::collectInputFiles(ROOT_DIR, {".ir"})));

} // end namespace parser2
} // end namespace core
} // end namespace insieme
