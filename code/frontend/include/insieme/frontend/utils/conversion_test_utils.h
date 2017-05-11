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
 *
 */
#include <string>
#include <fstream>
#include <functional>
#include <regex>

#include <gtest/gtest.h>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/replace.hpp>

#include "insieme/core/analysis/compare.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/frontend/extensions/test_pragma_extension.h"
#include "insieme/frontend/frontend.h"
#include "insieme/frontend/state/variable_manager.h"
#include "insieme/frontend/utils/frontend_inspire_module.h"

#include "insieme/annotations/expected_ir_annotation.h"
#include "insieme/utils/config.h"
#include "insieme/utils/set_utils.h"
#include "insieme/common/env_vars.h"

#define FRONTEND_TEST_DIR insieme::utils::getInsiemeSourceRootDir() + "frontend/test/"

namespace insieme {
namespace frontend {
namespace utils {

	using namespace extensions;
	using namespace core;
	using insieme::annotations::ExpectedIRAnnotation;

	namespace detail {
		/*
		 * Returns a new tree derived from the given actual tree with special expression and type replacements applied.
		 * Both given trees have to have an identical structure to some degree.
		 * The address of GenericTypes in expected with the name "__any_type__" will be replaced in the actual tree with the same node.
		 * The address of Literals in expected with the StringValue "__any_expr__" will be replaced in the actual tree with the same node.
		 *
		 * As an example, calling the function with the following trees:
		 * expected:                                          actual:
		 * - someRoot                                         - someActualRoot
		 *   `- Literal                                         `- Literal
		 *      `- "__any_expr__"                                  `- "true"
		 *      `- bool                                            `- bool
		 *   `- 4                                               `- 20
		 *   `- someNode                                        `- someActualNode
		 *      `- Literal                                         `- LambdaExpr
		 *      |  `- "__any_expr__"                               |  `- [...]
		 *      |  `- GenericType<"__any_type__">                  |  `- (int<4>) -> bool
		 *      `- someChild                                       `- someActualChild
		 *      `- GenericType<"__any_type__">                     `- TagType
		 *      |                                                  |  `- TagTypeReference
		 *      |                                                  |  `- [...]
		 *      `- someLeaf                                        `- someActualLeaf
		 *
		 * Will return the following tree:
		 * - someActualRoot
		 *   `- Literal
		 *      `- "__any_expr__"
		 *      `- bool
		 *   `- 20
		 *   `- someActualNode
		 *      `- Literal
		 *      |  `- "__any_expr__"
		 *      |  `- GenericType<"__any_type__">
		 *      `- someActualChild
		 *      `- GenericType<"__any_type__">
		 *      `- someActualLeaf
		 */
		static inline NodePtr replaceAnyTypeAndAnyExprNodes(const NodePtr& expected, const NodePtr& actual) {
			NodeManager& mgr = expected.getNodeManager();
			IRBuilder builder(mgr);

			static const std::string anyExprString = "__any_expr__";
			static const auto anyType = builder.genericType("__any_type__");

			std::map<NodeAddress, NodePtr> replacements;

			visitDepthFirst(NodeAddress(expected), [&](const NodeAddress& addr) {
				// this lambda will switch the current address into the actual tree. If that address exists it will add the replacement if the types are correct
				auto addReplacement = [&](const NodePtr& replacement) {
					auto targetAddr = addr.switchRoot(actual);
					// if the address exists in the target tree
					if(targetAddr) {
						// and the target of a Literal replacement is an expression or the target of a GenericType replacement is a type
						if((replacement.isa<LiteralPtr>() && targetAddr.isa<ExpressionAddress>())
								|| (replacement.isa<GenericTypePtr>() && targetAddr.isa<TypeAddress>())) {
							replacements[targetAddr] = replacement;
						}
					}
				};

				// Literals which's StringValue is the special expr marker get replaced
				if(auto lit = addr.isa<LiteralAddress>()) {
					if(lit->getValue()->getValue() == anyExprString) {
						addReplacement(lit.getAddressedNode());
					}
				}
				// as well as types which match the special type marker
				if(auto type = addr.isa<GenericTypeAddress>()) {
					if(type.getAddressedNode() == anyType) {
						addReplacement(anyType);
					}
				}
			}, true, true);

			if(replacements.empty()) {
				return actual;
			}
			return core::transform::replaceAll(mgr, replacements);
		}

		/*
		 * Returns a new tree derived from the given actual tree with special StringValue replacements applied.
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
	}

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

			// if this isn't the case we compare them with a special handling for our __any_type__ and __any_expr__ nodes
			if(!result) {
				// we modify actual and replace the addresses of the __any_type__ and __any_expr__ nodes in expected with the same nodes
				actual = detail::replaceAnyTypeAndAnyExprNodes(expected, actual);
				result = (expected == actual);
			}

			// if the trees are still not identical we compare them with a special handling for StringValues, which might be different and are tagged with a special marker
			if(!result) {
				// first we create copies of both trees which have all their StringValues set to "", so we actually only compare their structure
				auto expectedCopy = removeStringValues(expected);
				auto actualCopy = removeStringValues(actual);

				// if the copies are not identical now, the trees differ structurally
				result = (expectedCopy == actualCopy);

				// if they are identical, we compare them with a special treatment for our string markers
				if(result) {
					actual = detail::replaceAnyStringsInActual(expected, actual);
					result = (expected == actual);
				}
			}

			EXPECT_TRUE(result) << "\tLocation     : " << core::annotations::getLocationString(addr) << "\n"
			                    << "\tActual Pretty: " << print(actual) << "\n"
			                    << "\tExpect Pretty: " << print(expected) << "\n"
			                    //<< "\tActual Text: " << dumpText(actual) << "\n"
			                    //<< "\tExpect Text: " << dumpText(expected) << "\n"
			                    << "\tActual type  : " << (aIsExp ? toString(dumpColor(actual.as<ExpressionPtr>()->getType())) : toString("-")) << "\n"
			                    << "\tExpected type: " << (eIsExp ? toString(dumpColor(expected.as<ExpressionPtr>()->getType())) : toString("-")) << "\n";

			if(getenv(INSIEME_IRDIFF) != nullptr && expected != actual) {
				size_t contextSize = 0;
				if(auto contextSizeString = getenv(INSIEME_IRDIFF_CONTEXT_SIZE)) contextSize = atoi(contextSizeString);
				core::analysis::irDiff(actual, expected, "CodeIR", "PragmaIR", contextSize);
				exit(-1);
			}
		}
	}

	static inline void runConversionTestOn(const string& fn, std::function<void(ConversionJob&)> jobModifier = [](auto& job) {},
	                                       std::function<void(core::NodeManager&, core::lang::symbol_map&)> symbolModifier = [](auto& mgr, auto& symbols) {},
	                                       std::function<core::checks::MessageList(const core::NodePtr&)> semanticCheckFunction = [](const auto& node) { return core::checks::check(node); }) {
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
		symbolModifier(mgr, symbols);

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

		auto checkResult = semanticCheckFunction(res);
		EXPECT_EQ(checkResult.size(), 0) << printer::dumpErrors(checkResult)
		                                 //<< "\n" << dumpColor(checkResult.getErrors()[0].getOrigin().getAddressedNode())
		                                 //<< "\n@(" << core::annotations::getLocationString(checkResult.getErrors()[0].getOrigin()) << ")"
		                                 << "\n" << checkResult;
		// std::cout << "Semantic Error dump:\n";
		// dumpText(checkResult.getErrors()[0].getOrigin().getParentNode(2));
	}

	/**
	 * An enumeration of supported source language types.
	 */
	enum SrcType { C, CPP };

	/**
	 * A class managing the life-cycle of temporary source files which will only be
	 * created for the sake of unit tests.
	 */
	class Source {
		/**
		 * The path to the temporary file.
		 */
		fs::path file;

		public:
		/**
		 * The constructor creates a temporary file containing the given example code. The
		 * file will exist as long as the object is alive.
		 */
		Source(const string& code, SrcType type = C) {
			// create a temporary file containing the code
			switch(type) {
			case C: file = fs::unique_path(fs::temp_directory_path() / "src%%%%%%%%.c"); break;
			case CPP: file = fs::unique_path(fs::temp_directory_path() / "src%%%%%%%%.cpp"); break;
			default: assert(false && "Invalid type selected!");
			}

			// write source to file
			std::fstream srcFile(file.string(), std::fstream::out);
			srcFile << code << "\n";
			srcFile.close();
		}

		~Source() {
			// remove temporary file
			if(fs::exists(file)) { fs::remove(file); }
		}

		const fs::path& getPath() const {
			return file;
		}

		operator fs::path() const {
			return getPath();
		}
	};

	// a utility to fix variable names
	NodePtr fixVariableIDs(const NodePtr& code) {
		NodeManager& mgr = code.getNodeManager();
		IRBuilder builder(mgr);

		// first, run simple normalizer
		NodePtr res = insieme::core::analysis::normalize(code);

		// now fix free variables
		std::map<VariablePtr, VariablePtr> vars;
		std::map<NodeAddress, NodePtr> replacements;
		auto freeVars = insieme::core::analysis::getFreeVariableAddresses(res);

		// if there are no free variables => done
		if(freeVars.empty()) return res;

		std::set<VariableAddress> freeVarSet(freeVars.begin(), freeVars.end());
		for(auto cur : freeVarSet) {
			VariablePtr var = cur.as<VariablePtr>();
			VariablePtr replacement;

			auto pos = vars.find(var);
			if(pos != vars.end()) {
				replacement = pos->second;
			} else {
				replacement = builder.variable(cur->getType().as<TypePtr>(), 100 + vars.size());
				vars[var] = replacement;
			}
			replacements[cur] = replacement;
		}

		return transform::replaceAll(mgr, replacements);
	};

} // end namespace utils
} // end namespace frontend
} // end namespace insieme
