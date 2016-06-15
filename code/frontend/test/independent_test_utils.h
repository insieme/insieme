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

#include <gtest/gtest.h>
#include <boost/regex.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/replace.hpp>

#include "insieme/annotations/expected_ir_annotation.h"
#include "insieme/core/annotations/source_location.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/error_printer.h"
#include "insieme/core/ir_address.h"
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

		template <typename T>
		static inline std::string locationOf(const T& n) {
			auto loc = core::annotations::getLocation(n);
			return loc ? toString(*loc) : std::string("Location could not be found!");
		}

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

		void irDiff(NodePtr codeIr, NodePtr pragmaIr, insieme::utils::set::PointerSet<NodePtr> visited = insieme::utils::set::PointerSet<NodePtr>()) {
			const auto& locString = locationOf(codeIr);

			if(visited.find(codeIr) != visited.end()) {
				return;
			} else {
				visited.insert(codeIr);
			}

			if(codeIr->getNodeType() != pragmaIr->getNodeType()) {
				std::cout << "IRDIFF-----\nNode types differ:\n"
				          << "\tNT codeIr:   " << codeIr->getNodeType() << "\n"
				          << "\tNT pragmaIr: " << pragmaIr->getNodeType() << "\n"
				          << "\tcodeIr:   " << dumpColor(codeIr) << "\tpragmaIr: " << dumpColor(pragmaIr) << "\tLOC(" << locString << ")\n";
			}

			auto aExp = codeIr.isa<ExpressionPtr>();
			auto bExpr = pragmaIr.isa<ExpressionPtr>();
			if(aExp) {
				if(aExp->getType() != bExpr->getType()) {
					std::cout << "IRDIFF-----\nExpressions differ in type:\n"
					          << "\ttype codeIr:   " << *aExp->getType() << "\n"
					          << "\ttype pragmaIr: " << *bExpr->getType() << "\n"
					          << "\tcodeIr:   " << dumpColor(codeIr) << "\tpragmaIr: " << dumpColor(pragmaIr) << "\tLOC(" << locString << ")\n";
				}
			}

			auto aVar = codeIr.isa<VariablePtr>();
			auto bVar = pragmaIr.isa<VariablePtr>();
			if(aVar) {
				if(aVar->getId() != bVar->getId()) {
					std::cout << "IRDIFF-----\nVariables differ in id:\n"
					          << "\tid codeIr:   " << aVar->getId() << "\n"
					          << "\tid pragmaIr: " << bVar->getId() << "\n"
					          << "\tcodeIr:   " << dumpColor(codeIr) << "\tpragmaIr: " << dumpColor(pragmaIr) << "\tLOC(" << locString << ")\n";
				}
			}

			auto aString = codeIr.isa<StringValuePtr>();
			auto bString = pragmaIr.isa<StringValuePtr>();
			if(aString) {
				if(aString->getValue() != bString->getValue()) {
					std::cout << "IRDIFF-----\nStringValues differ:\n"
					          << "\tid codeIr:   " << aString->getValue() << "\n"
					          << "\tid pragmaIr: " << bString->getValue() << "\n"
					          << "\tcodeIr:   " << dumpColor(codeIr) << "\tpragmaIr: " << dumpColor(pragmaIr) << "\tLOC(" << locString << ")\n";
				}
			}

			auto aChildren = codeIr->getChildList();
			auto bChildren = pragmaIr->getChildList();
			if(aChildren.size() != bChildren.size()) {
				std::cout << "IRDIFF-----\nNodes differ in number of children (ABORTING):\n"
				          << "\tchild count codeIr:   " << aChildren.size() << "\n"
				          << "\tchild count pragmaIr: " << bChildren.size() << "\n"
				          << "\tcodeIr:   " << dumpColor(codeIr) << "\tpragmaIr: " << dumpColor(pragmaIr) << "\tLOC(" << locString << ")\n";
				return;
			}

			for(size_t i = 0; i < aChildren.size(); ++i) {
				irDiff(aChildren[i], bChildren[i]);
			}
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
			EXPECT_EQ(expected, actual) << "\tLocation     : " << locationOf(addr) << "\n"
			                            << "\tActual Pretty: " << print(actual) << "\n"
			                            << "\tExpect Pretty: " << print(expected) << "\n"
			                            //<< "\tActual Text: " << dumpText(actual) << "\n"
			                            //<< "\tExpect Text: " << dumpText(expected) << "\n"
			                            << "\tActual type  : " << (aIsExp ? toString(dumpColor(actual.as<ExpressionPtr>()->getType())) : toString("-")) << "\n"
			                            << "\tExpected type: " << (eIsExp ? toString(dumpColor(expected.as<ExpressionPtr>()->getType())) : toString("-"))
			                            << "\n";

			if(getenv("INSIEME_IRDIFF") != nullptr && expected != actual) {
				irDiff(actual, expected);
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
				ASSERT_FALSE(ex.empty()) << "!Empty string in pragma @ " << locationOf(node);

				const string regexKey = "REGEX";
				const string regexKeyS = "REGEX_S";
				const string stringKey = "STRING";
				const string exprTypeKey = "EXPR_TYPE";

				VLOG(2) << "Expected annotation string: " << ex << "\n";

				// -------------------------------------------------------------------------------------------------------------------------- Regex ===========|
				if(boost::starts_with(ex, regexKey)) {
					boost::regex re;
					if(boost::starts_with(ex, regexKeyS)) {
						auto res = ex.substr(regexKeyS.size());
						boost::replace_all(res, " ", "\\s*");
						boost::replace_all(res, "\t", "\\s*");
						boost::replace_all(res, "\n", "\\s*");
						re = boost::regex(res);
					} else {
						re = boost::regex(ex.substr(regexKey.size()));
					}
					auto irString = ::toString(printer::PrettyPrinter(builder.normalize(node), printer::PrettyPrinter::OPTIONS_DEFAULT
					                                                                               | printer::PrettyPrinter::PRINT_DERIVED_IMPL
					                                                                               | printer::PrettyPrinter::PRINT_DEREFS));
					EXPECT_TRUE(boost::regex_match(irString.begin(), irString.end(), re)) << "Location : " << locationOf(node) << "\n"
					                                                                      << "IR String: " << irString << "\n"
					                                                                      << "Regex    : " << re << "\n";
				}
				// --------------------------------------------------------------------------------------------------------------------- String Compare =======|
				else if(boost::starts_with(ex, stringKey)) {
					string exs(ex.substr(stringKey.size()));
					auto irString = ::toString(printer::PrettyPrinter(
					    builder.normalize(node), printer::PrettyPrinter::OPTIONS_DEFAULT | printer::PrettyPrinter::NO_LET_BINDINGS
					                                 | printer::PrettyPrinter::NO_LET_BOUND_FUNCTIONS | printer::PrettyPrinter::PRINT_DEREFS));
					EXPECT_EQ(exs, irString) << "Location       : " << locationOf(node) << "\n";
				}
				// ----------------------------------------------------------------------------------------------------------------- type of expression =======|
				else if(boost::starts_with(ex, exprTypeKey)) {
					string irs(ex.substr(exprTypeKey.size()));
					NodePtr expected;
					ASSERT_NO_THROW(expected = builder.parseType(irs, symbols))
					    << "Type in pragma could not be parsed:\n\t" << ex << "error: " << typeParsingError(builder, irs, symbols) << "\n@" << locationOf(node);
					checkExpected(expected, node.as<ExpressionPtr>()->getType(), addr);
				}
				// --------------------------------------------------------------------------------------------------------------------- any other case =======|
				else {
					NodePtr expected;
					if(node.isa<ExpressionPtr>()) {
						ASSERT_NO_THROW(expected = builder.parseExpr(ex, symbols)) << "Expression in pragma could not be parsed:\n\t" << ex
						                                                           << "error: " << exprParsingError(builder, ex, symbols) << "\n@"
						                                                           << locationOf(node);
					} else {
						ASSERT_NO_THROW(expected = builder.parseStmt(ex, symbols)) << "Statement in pragma could not be parsed:\n\t" << ex
						                                                           << "error: " << stmtParsingError(builder, ex, symbols) << "\n@"
						                                                           << locationOf(node);
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
		                                 //<< "\n@(" << locationOf(checkResult.getErrors()[0].getOrigin()) << ")"
		                                 << "\n" << checkResult;
		// std::cout << "Semantic Error dump:\n";
		// dumpText(checkResult.getErrors()[0].getOrigin().getParentNode(2));
	}
} // end namespace frontend
} // end namespace insieme
