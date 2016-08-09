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

#include <string>
#include <fstream>
#include <functional>
#include <regex>

#include <gtest/gtest.h>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/replace.hpp>

#include "insieme/annotations/expected_ir_annotation.h"
#include "insieme/core/analysis/compare.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/frontend/extensions/test_pragma_extension.h"
#include "insieme/frontend/frontend.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/frontend/utils/frontend_inspire_module.h"
#include "insieme/utils/config.h"
#include "insieme/utils/set_utils.h"

#define FRONTEND_TEST_DIR insieme::utils::getInsiemeSourceRootDir() + "frontend/test/"

namespace insieme {
namespace frontend {

	using namespace extensions;
	using namespace core;
	using insieme::annotations::ExpectedIRAnnotation;

	namespace {
		static inline std::string typeParsingError(core::IRBuilder& builder, const std::string& str, const core::lang::symbol_map& symbols) {
			try {
				builder.parseType(str, symbols);
			} catch(const core::parser::IRParserException& ex) { return ex.getMessage(); }
			return "";
		}
		static inline std::string exprParsingError(core::IRBuilder& builder, const std::string& str, const core::lang::symbol_map& symbols) {
			try {
				builder.parseExpr(str, symbols);
			} catch(const core::parser::IRParserException& ex) { return ex.getMessage(); }
			return "";
		}
		static inline std::string stmtParsingError(core::IRBuilder& builder, const std::string& str, const core::lang::symbol_map& symbols) {
			try {
				builder.parseStmt(str, symbols);
			} catch(const core::parser::IRParserException& ex) { return ex.getMessage(); }
			return "";
		}

		/*
		 * Returns a new tree derived from the given actual tree.
		 * Both given trees have to have an identical structure.
		 * The StringValue(prefixes) "__any_string__" in expected can be anything in actual, and will be replaced with the matching StringValue(prefixes)
		 * from the expected tree. the Suffixes (delimited by "::") will keep the same.
		 *
		 * As an example, calling the function with the following trees:
		 * expected:                                          actual:
		 * - someRoot                                         - someRoot
		 *   `- "__any_string__1"                               `- "SomeStringInActual"
		 *   `- 4                                               `- 20
		 *   `- someNode                                        `- someNode
		 *      `- someChild                                       `- someChild
		 *      |  `- 8                                            |  `- 1000
		 *      |  `- "__any_string__2::ExpectedSuffix"            |  `- "AnotherString::ActualSuffix"
		 *      `- "__any_string__1::FooSuffix"                    `- "SomeStringInActual::BarSuffix"
		 *      `- "__any_string__2"                               `- "AnotherString"
		 *      `- someLeaf                                        `- someLeaf
		 *
		 * Will return the following tree:
		 * - someRoot
		 *   `- "__any_string__1"
		 *   `- 20
		 *   `- someNode
		 *      `- someChild
		 *      |  `- 1000
		 *      |  `- "__any_string__2::ActualSuffix"
		 *      `- "__any_string__1::BarSuffix"
		 *      `- "__any_string__2"
		 *      `- someLeaf
		 */
		static inline NodePtr replaceAnyStringsInActual(const NodePtr& expected, const NodePtr& actual) {
			NodeManager& mgr = expected.getNodeManager();
			IRBuilder builder(mgr);

			static const std::string markerPrefix = "__any_string__";

			std::map<NodeAddress, NodePtr> replacements;
			std::map<std::string, std::string> nameMappings;

			visitDepthFirst(NodeAddress(expected), [&](const StringValueAddress& addr) {
				auto& expectedName = addr->getValue();
				if(boost::starts_with(expectedName, markerPrefix)) {
					auto targetAddr = addr.switchRoot(actual);
					if (!targetAddr || !targetAddr.isa<StringValueAddress>()) {
						assert_fail() << "Invalid tree structure";
					}
					//if we found an instance without any appended member name
					if(!boost::contains(expectedName, "::")) {
						//add it to our mapping
						nameMappings[targetAddr->getValue()] = expectedName;
						replacements[targetAddr] = addr.getAddressedNode();

						//otherwise we have to replace only a part of the string
					} else {
						auto& targetName = targetAddr->getValue();
						auto seperatorIndex = targetName.find("::");
						auto prefix = targetName.substr(0, seperatorIndex);
						replacements[targetAddr] = builder.stringValue(nameMappings[prefix] + targetName.substr(seperatorIndex));
					}
				}
			}, true, true);

			if(replacements.empty()) {
				return actual;
			}
			return core::transform::replaceAll(mgr, replacements);
		}

		static inline void checkExpected(NodePtr expected, NodePtr actual, const NodeAddress& addr) {
			ASSERT_TRUE(actual) << "Actual IR pointer null!";
			ASSERT_TRUE(expected) << "Expected IR pointer null (likely parser error)!\n"
			                      << "  Location: " << *core::annotations::getLocation(addr) << "\n";

			IRBuilder builder(expected->getNodeManager());
			bool eIsExp = expected.isa<ExpressionPtr>();
			bool aIsExp = actual.isa<ExpressionPtr>();
			auto expNN = expected;
			auto actNN = actual;
			expected = builder.normalize(expected);
			actual = builder.normalize(actual);
			auto print = [](const core::NodePtr& node) {
				return core::printer::PrettyPrinter(node, core::printer::PrettyPrinter::OPTIONS_DEFAULT | core::printer::PrettyPrinter::USE_COLOR
				                                              | core::printer::PrettyPrinter::PRINT_DEREFS | core::printer::PrettyPrinter::FULL_LITERAL_SYNTAX);
			};
			const auto emptyStringValue = builder.stringValue("");
			auto removeStringValues = [&emptyStringValue](const NodePtr& root) {
				return core::transform::transformBottomUpGen(root, [&emptyStringValue](const StringValuePtr& stringValue){
					return emptyStringValue;
				}, core::transform::globalReplacement);
			};

			// first we check whether the given trees are identical
			bool result = (expected == actual);

			// if this isn't the case we compare them with a special handling for StringValues, which might be different and are tagged with a special marker
			if(!result) {
				// first we create copies of both trees which have all their StringValues set to "", so we actually only compare their structure
				auto expectedCopy = removeStringValues(expected);
				auto actualCopy = removeStringValues(actual);

				// if the copies are not identical now, the trees differ structurally
				result = (expectedCopy == actualCopy);

				// if they are identical, we compare them with a special treatment for our string markers
				if(result) {
					actual = replaceAnyStringsInActual(expected, actual);
					result = (expected == actual);
				}
			}

			EXPECT_TRUE(result)         << "\tLocation     : " << core::annotations::getLocationString(addr) << "\n"
			                            << "\tActual Pretty: " << print(actual) << "\n"
			                            << "\tExpect Pretty: " << print(expected) << "\n"
			                            //<< "\tActual Text: " << dumpText(actual) << "\n"
			                            //<< "\tExpect Text: " << dumpText(expected) << "\n"
			                            << "\tActual type  : " << (aIsExp ? toString(dumpColor(actual.as<ExpressionPtr>()->getType())) : toString("-")) << "\n"
			                            << "\tExpected type: " << (eIsExp ? toString(dumpColor(expected.as<ExpressionPtr>()->getType())) : toString("-"))
			                            << "\n";

			if(getenv("INSIEME_IRDIFF") != nullptr && expected != actual) {
				core::analysis::irDiff(actual, expected, "CodeIR", "PragmaIR");
				exit(-1);
			}
		}
	}

	static inline void runIndependentTestOn(const string& fn, std::function<void(ConversionJob&)> jobModifier = [](ConversionJob& job) {}) {
		core::NodeManager mgr;
		core::IRBuilder builder(mgr);

		// build conversion job and add required extensions
		ConversionJob job(fn);
		if(job.isCxx()) job.setStandard(ConversionSetup::Cxx11);
		job.registerFrontendExtension<TestPragmaExtension>([](conversion::Converter& converter, int num) {
			EXPECT_EQ(num, converter.getVarMan()->numVisibleDeclarations()) << "Location: " << converter.getLastTrackableLocation() << "\n";
		});
		job.registerFrontendExtension<FrontendCleanupExtension>();
		jobModifier(job);
		auto res = builder.normalize(job.execute(mgr));

		// add FE module symbols for use in test cases
		auto symbols = mgr.getLangExtension<frontend::utils::FrontendInspireModule>().getSymbols();

		// iterate over res and check pragma expectations
		size_t visited = 0;
		visitDepthFirstOnce(NodeAddress(res), [&](const NodeAddress& addr) {

			auto node = addr.getAddressedNode();
			ASSERT_TRUE(node) << " no node in address";

			if(node->hasAnnotation(ExpectedIRAnnotation::KEY)) {
				const auto& ann = node->getAnnotation(ExpectedIRAnnotation::KEY);
				const auto& ex = ann->getExpected();
				ASSERT_FALSE(ex.empty()) << "!Empty string in pragma @ " << core::annotations::getLocationString(node);

				const string regexKey = "REGEX";
				const string regexKeyS = "REGEX_S";
				const string stringKey = "STRING";
				const string exprTypeKey = "EXPR_TYPE";

				VLOG(2) << "Expected annotation string: " << ex << "\n";

				// -------------------------------------------------------------------------------------------------------------------------- Regex ===========|
				if(boost::starts_with(ex, regexKey)) {
					string regexString;
					if(boost::starts_with(ex, regexKeyS)) {
						regexString = ex.substr(regexKeyS.size());
						boost::replace_all(regexString, " ", "[\\s\\r\\n]*");
						boost::replace_all(regexString, "\t", "[\\s\\r\\n]*");
						boost::replace_all(regexString, "\n", "[\\s\\r\\n]*");
					} else {
						regexString = ex.substr(regexKey.size());
					}
					// make . behave as in multi-line mode
					boost::replace_all(regexString, "\\.", "__INSIEME_ESCAPE_BACKSLASH_DOT");
					boost::replace_all(regexString, ".", "(?:.|\\r|\\n)");
					boost::replace_all(regexString, "__INSIEME_ESCAPE_BACKSLASH_DOT", "\\.");
					std::regex re(regexString);
					auto irString = ::toString(printer::PrettyPrinter(builder.normalize(node), printer::PrettyPrinter::OPTIONS_DEFAULT
					                                                                               | printer::PrettyPrinter::PRINT_DERIVED_IMPL
					                                                                               | printer::PrettyPrinter::PRINT_DEREFS));
					EXPECT_TRUE(std::regex_match(irString, re)) << "Location : " << core::annotations::getLocationString(node) << "\n"
					                                              << "IR String: " << irString << "\n";
				}
				// --------------------------------------------------------------------------------------------------------------------- String Compare =======|
				else if(boost::starts_with(ex, stringKey)) {
					string exs(ex.substr(stringKey.size()));
					auto irString = ::toString(printer::PrettyPrinter(
					    builder.normalize(node), printer::PrettyPrinter::OPTIONS_DEFAULT | printer::PrettyPrinter::NO_LET_BINDINGS
					                                 | printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS | printer::PrettyPrinter::PRINT_DEREFS));
					EXPECT_EQ(exs, irString) << "Location       : " << core::annotations::getLocationString(node) << "\n";
				}
				// ----------------------------------------------------------------------------------------------------------------- type of expression =======|
				else if(boost::starts_with(ex, exprTypeKey)) {
					string irs(ex.substr(exprTypeKey.size()));
					NodePtr expected;
					ASSERT_NO_THROW(expected = builder.parseType(irs, symbols)) << "Type in pragma could not be parsed:\n\t" << ex
					                                                            << "error: " << typeParsingError(builder, irs, symbols) << "\n@"
					                                                            << core::annotations::getLocationString(node);
					checkExpected(expected, node.as<ExpressionPtr>()->getType(), addr);
				}
				// --------------------------------------------------------------------------------------------------------------------- any other case =======|
				else {
					NodePtr expected;
					if(node.isa<ExpressionPtr>()) {
						ASSERT_NO_THROW(expected = builder.parseExpr(ex, symbols)) << "Expression in pragma could not be parsed:\n\t" << ex
						                                                           << "error: " << exprParsingError(builder, ex, symbols) << "\n@"
						                                                           << core::annotations::getLocationString(node);
					} else {
						ASSERT_NO_THROW(expected = builder.parseStmt(ex, symbols)) << "Statement in pragma could not be parsed:\n\t" << ex
						                                                           << "error: " << stmtParsingError(builder, ex, symbols) << "\n@"
						                                                           << core::annotations::getLocationString(node);
					}
					ASSERT_TRUE(expected) << "nothing to compare with";
					checkExpected(expected, node, addr);
				}
				visited++;
			}
		});

        // count number of occurrences of the pragma string
		std::ifstream tf(fn);
		std::string fileText((std::istreambuf_iterator<char>(tf)), std::istreambuf_iterator<char>());
		fileText = insieme::utils::removeCppStyleComments(fileText);

		// search pragmas
		std::string searchString{"#pragma test expect_ir"};
		string::size_type start = 0;
		unsigned occurrences = 0;
		while((start = fileText.find(searchString, start)) != string::npos) {
			++occurrences;
			start += searchString.length();
		}
		EXPECT_EQ(visited, occurrences);

		// dumpColor(res);
		// std::cout << res;

		auto checkResult = core::checks::check(res);
		EXPECT_EQ(checkResult.size(), 0) << printer::dumpErrors(checkResult)
		                                 //<< "\n" << dumpColor(checkResult.getErrors()[0].getOrigin().getAddressedNode())
		                                 //<< "\n@(" << core::annotations::getLocationString(checkResult.getErrors()[0].getOrigin()) << ")"
		                                 << "\n" << checkResult;
		// std::cout << "Semantic Error dump:\n";
		// dumpText(checkResult.getErrors()[0].getOrigin().getParentNode(2));
	}
} // end namespace frontend
} // end namespace insieme
