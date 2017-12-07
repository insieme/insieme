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

#include <string>
#include <fstream>
#include <functional>
#include <regex>
#include <unordered_set>

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
#include "insieme/utils/hash_utils.h"
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
		 * This function compares both trees recursively and returns whether they match considering some special markers.
		 *
		 * - If the expected node is a GenericType with the family name "__any_type__", any type is accepted as actual node.
		 * - If the expected node is a Literal with the string value "__any_expr__", any expression is accepted as actual node.
		 * - If the expected node is a StringLiteral which starts with the prefix "__any_string__", then the string value of the actual node
		 *   will be mapped to the string value of the expected node. If a mapping for the expected node already exists, the actual value has to match.
		 *   For StringLiteral objects which contain the string "::" (i.e. class members), this procedure will be executed individually for the class name
		 *   and member name part.
		 */
		static inline bool compareWithMarkerNodeHandlingInternal(const NodePtr& expected, const NodePtr& actual,
		                                                         std::map<std::string, std::string>& nameMappings,
		                                                         std::unordered_set<std::pair<core::NodePtr, core::NodePtr>>& equalNodes) {
			static const std::string markerPrefix = "__any_string__";
			static const std::string anyExprString = "__any_expr__";
			static const std::string anyTypeString = "__any_type__";

			// lookup in cache
			if(equalNodes.find({expected, actual}) != equalNodes.end()) return true;

			// if the expected node is the special __any_type__ generic type
			if(expected->getNodeType() == core::NT_GenericType && expected.as<core::GenericTypePtr>()->getFamilyName() == anyTypeString) {
				// we accept any type for the actual node
				return actual->getNodeCategory() == core::NC_Type;

				// if the expected node is a literal with the special __any_expr__ StringValue
			} else if(expected->getNodeType() == core::NT_Literal && expected.as<core::LiteralPtr>()->getStringValue() == anyExprString) {
				// we accept any expression for the actual node
				return actual->getNodeCategory() == core::NC_Expression;
			}

			// now we ensure that both nodes have the same node type and the same number of children
			if(expected->getNodeType() != actual->getNodeType()) return false;
			if(expected->getChildList().size() != actual->getChildList().size()) return false;

			// this lambda will lookup the name mapping and check for equality. If the name mapping isn't already existing, it will be added
			auto lookupAndInsertNameMapping = [&nameMappings](const std::string& expectedName, const std::string& actualName) {
				// only do the mapping if the string starts with the markerPrefix
				if(boost::starts_with(expectedName, markerPrefix)) {
					// we check whether we already have a mapping for this prefix
					auto mappingResult = nameMappings.find(expectedName);
					if(mappingResult != nameMappings.end()) {
						// then the mapping needs to be the same
						return mappingResult->second == actualName;
					}
					// if we don't have a mapping already, we add it and the comparison is correct
					nameMappings[expectedName] = actualName;
					return true;
				}
				// otherwise, we compare both strings
				return expectedName == actualName;
			};

			// StringValues need special consideration
			if(expected->getNodeType() == core::NT_StringValue) {
				auto& expectedName = expected.as<core::StringValuePtr>()->getValue();
				auto& actualName = actual.as<core::StringValuePtr>()->getValue();

				// if this StringValue does not represent a record-member
				if(!boost::contains(expectedName, "::")) {
					return lookupAndInsertNameMapping(expectedName, actualName);
				}

				// otherwise we have to split both names into className::memberName parts and compare them both
				if(!boost::contains(actualName, "::")) return false;
				auto expectedNameClass = expectedName.substr(0, expectedName.find("::"));
				auto actualNameClass = actualName.substr(0, actualName.find("::"));
				auto expectedNameMember = expectedName.substr(expectedName.find("::") + 2);
				auto actualNameMember = actualName.substr(actualName.find("::") + 2);
				return lookupAndInsertNameMapping(expectedNameClass, actualNameClass) && lookupAndInsertNameMapping(expectedNameMember, actualNameMember);

				// all other nodes are compared by comparing all their children
			} else {
				for(size_t index = 0; index < expected->getChildList().size(); ++index) {
					if(!compareWithMarkerNodeHandlingInternal(expected->getChild(index), actual->getChild(index), nameMappings, equalNodes)) return false;
				}
			}

			// insert a new entry into our cache
			equalNodes.insert({expected, actual});
			return true;
		}
		static inline bool compareWithMarkerNodeHandling(const NodePtr& expected, const NodePtr& actual) {
			std::map<std::string, std::string> nameMappings;
			std::unordered_set<std::pair<core::NodePtr, core::NodePtr>> equalNodes;
			return compareWithMarkerNodeHandlingInternal(expected, actual, nameMappings, equalNodes);
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

			// first we check whether the given trees are identical
			bool result = (expected == actual);

			// if this isn't the case we compare them with a special handling for our __any_type__, __any_expr__ and __any_string__ nodes
			if(!result) {
				result = detail::compareWithMarkerNodeHandling(expected, actual);
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
