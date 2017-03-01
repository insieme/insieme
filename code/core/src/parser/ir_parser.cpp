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
#include "insieme/core/parser/ir_parser.h"

#include <sstream>

#include "insieme/core/ir_builder.h"
#include "insieme/core/parser/detail/driver.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"


#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/encoder/lists.h"


namespace insieme {
namespace core {
namespace parser {

	using namespace detail;

	namespace {

		void checkErrors(InspireDriver& driver, bool onFailThrow) {
			if(driver.wereErrors() || !driver.result) {
				if(onFailThrow) {
					std::stringstream ss;
					driver.printErrors(ss);
					throw IRParserException(ss.str());
				} else {
					driver.printErrors();
				}
			}
		}

		void saveSymbolTable(InspireDriver& driver, const DefinitionMap& definitions) {
			for(const auto& def : definitions) {
				// the use of the line: symbols["name"] = builder.parseX("....", symbols);
				// will append a symbols with a null ptr inside. this is not good
				driver.declareSymbol(location(), def.first, def.second);
			}
		}

		void appendTypeAliases(InspireDriver& driver, const TypeAliasMap& aliases) {
			for(const auto& cur : aliases) {
				driver.addTypeAlias(cur.first, cur.second);
			}
		}
	}

	TypePtr parseType(NodeManager& manager, const string& code, bool onFailThrow, const DefinitionMap& definitions, const TypeAliasMap& aliases) {
		InspireDriver driver(code, manager);
		saveSymbolTable(driver, definitions);
		appendTypeAliases(driver, aliases);
		auto x = driver.parseType();
		if(!x) { checkErrors(driver, onFailThrow); }
		return x;
	}

	ExpressionPtr parseExpr(NodeManager& manager, const string& code, bool onFailThrow, const DefinitionMap& definitions, const TypeAliasMap& aliases) {
		InspireDriver driver(code, manager);
		saveSymbolTable(driver, definitions);
		appendTypeAliases(driver, aliases);
		auto x = driver.parseExpression();
		if(!x) { checkErrors(driver, onFailThrow); }
		return x;
	}

	StatementPtr parseStmt(NodeManager& manager, const string& code, bool onFailThrow, const DefinitionMap& definitions, const TypeAliasMap& aliases) {
		InspireDriver driver(code, manager);
		saveSymbolTable(driver, definitions);
		appendTypeAliases(driver, aliases);
		auto x = driver.parseStmt();
		if(!x) { checkErrors(driver, onFailThrow); }
		return x;
	}

	ProgramPtr parseProgram(NodeManager& manager, const string& code, bool onFailThrow, const DefinitionMap& definitions, const TypeAliasMap& aliases) {
		InspireDriver driver(code, manager);
		saveSymbolTable(driver, definitions);
		appendTypeAliases(driver, aliases);
		auto x = driver.parseProgram();
		if(!x) { checkErrors(driver, onFailThrow); }
		return x;
	}

	NodePtr parseAny(NodeManager& manager, const string& code, bool onFailThrow, const DefinitionMap& definitions, const TypeAliasMap& aliases) {
		NodePtr x;
		try {
			//first try to parse as an expression
			x = parseExpr(manager, code, onFailThrow, definitions, aliases);
			return x;
		} catch (const IRParserException& expr_ex) {

			//then try to parse as a type
			try {
				x = parseType(manager, code, onFailThrow, definitions, aliases);
				return x;
			} catch (const IRParserException& type_ex) {

				//then try to parse as a statement
				try {
					x = parseStmt(manager, code, onFailThrow, definitions, aliases);
					return x;
				} catch (const IRParserException& stmt_ex) {

					//finally try to parse as program
					try {
						x = parseProgram(manager, code, onFailThrow, definitions, aliases);
						return x;
					} catch (const IRParserException& prog_ex) {


						// aggregate all errors into a string:
						std::stringstream ss;
						ss << "Unable to parse input. The following errors occurred:\n";
						ss << " When attempting to interpret as a type:\n\t" << type_ex.what() << "\n";
						ss << " When attempting to interpret as an expression:\n\t" << expr_ex.what() << "\n";
						ss << " When attempting to interpret as a statement:\n\t" << stmt_ex.what() << "\n";
						ss << " When attempting to interpret as a program:\n\t" << prog_ex.what() << "\n";
						auto msg = ss.str();

						//if this failed and we should throw exceptions, then we do so
						if (onFailThrow) {
							throw IRParserException(msg);
						} else {
							std::cerr << msg << std::endl;
						}
					}
				}
			}
		}
		return x;
	}

	namespace {

		struct MarkEliminator : public core::transform::CachedNodeMapping {
			virtual const NodePtr resolveElement(const NodePtr& ptr) {
				// replace recursively
				NodePtr res = ptr->substitute(ptr->getNodeManager(), *this);

				// eliminate marked nodes
				if(ptr->hasAttachedValue<AddressMark>()) {
					// strip off marker expression (also drops annotation)
					if(res->getNodeType() == core::NT_MarkerExpr) { return res.as<core::MarkerExprPtr>()->getSubExpression(); }
					if(res->getNodeType() == core::NT_MarkerStmt) { return res.as<core::MarkerStmtPtr>()->getSubStatement(); }
					assert(false && "Only marker expressions and statements should be marked.");
				}

				// return result
				return res;
			}
		};

		NodePtr removeMarks(const core::NodePtr& cur) {
			return MarkEliminator().map(cur);
		}

		NodeAddress removeMarks(const core::NodePtr& newRoot, const core::NodeAddress& cur) {
			// handle terminal case => address only references a root node
			if(cur.isRoot()) {
				// if root is marked => skip
				if(cur->hasAttachedValue<AddressMark>()) { return core::NodeAddress(); }

				// return new root
				return core::NodeAddress(newRoot);
			}

			// get cleaned path to parent node
			NodeAddress parent = removeMarks(newRoot, cur.getParentAddress());

			// skip marked nodes
			if(cur->hasAttachedValue<AddressMark>()) { return parent; }

			// see whether this is the first non-marked node along the path
			if(!parent) { return core::NodeAddress(newRoot); }

			// also fix child of marker node
			if(cur.getDepth() >= 2 && cur.getParentNode()->hasAttachedValue<AddressMark>()) {
				return parent.getAddressOfChild(cur.getParentAddress().getIndex());
			}

			// in all other cases just restore same address path
			return parent.getAddressOfChild(cur.getIndex());
		}


		std::vector<NodeAddress> extractAddresses(NodePtr root) {
			// search all marked locations within the parsed code fragment
			std::vector<NodeAddress> res;
			core::visitDepthFirst(NodeAddress(root), [&](const NodeAddress& cur) {
				if(cur->hasAttachedValue<AddressMark>()) {
					// get address to marked sub-construct
					if(cur->getNodeType() == core::NT_MarkerExpr) {
						res.push_back(cur.as<core::MarkerExprAddress>()->getSubExpression());
					} else if(cur->getNodeType() == core::NT_MarkerStmt) {
						res.push_back(cur.as<core::MarkerStmtAddress>()->getSubStatement());
					} else {
						assert(false && "Only marker expressions and statements should be marked.");
					}
				}
			}, true, true);

			// remove marks from code
			root = removeMarks(root);

			// remove marker nodes from addresses
			for(NodeAddress& cur : res) {
				cur = removeMarks(root, cur);
			}

			// return list of addresses
			return res;
		}

	} // annon namespace

	std::vector<NodeAddress> parseAddressesType(NodeManager& manager, const string& code, bool onFailThrow,
		const DefinitionMap& definitions, const TypeAliasMap& aliases) {

		InspireDriver driver(code, manager);
		saveSymbolTable(driver, definitions);
		appendTypeAliases(driver, aliases);
		auto root = driver.parseType();

		// check the result
		if (!root) {
			checkErrors(driver, onFailThrow);
			return std::vector<NodeAddress>();
		}

		return extractAddresses(root);
	}


	std::vector<NodeAddress> parseAddressesExpression(NodeManager& manager, const string& code, bool onFailThrow,
	                                                   const DefinitionMap& definitions, const TypeAliasMap& aliases) {
		InspireDriver driver(code, manager);
		saveSymbolTable(driver, definitions);
		appendTypeAliases(driver, aliases);
		auto root = driver.parseExpression();

		// check the result
		if(!root) {
			checkErrors(driver, onFailThrow);
			return std::vector<NodeAddress>();
		}

		return extractAddresses(root);
	}

	std::vector<NodeAddress> parseAddressesStatement(NodeManager& manager, const string& code, bool onFailThrow,
	                                                   const DefinitionMap& definitions, const TypeAliasMap& aliases) {
		InspireDriver driver(code, manager);
		saveSymbolTable(driver, definitions);
		appendTypeAliases(driver, aliases);
		auto root = driver.parseStmt();

		// check the result
		if(!root) {
			checkErrors(driver, onFailThrow);
			return std::vector<NodeAddress>();
		}

		return extractAddresses(root);
	}

	std::vector<NodeAddress> parseAddressesProgram(NodeManager& manager, const string& code, bool onFailThrow, const DefinitionMap& definitions, const TypeAliasMap& aliases) {
		InspireDriver driver(code, manager);
		saveSymbolTable(driver, definitions);
		appendTypeAliases(driver, aliases);
		auto root = driver.parseProgram();

		// check the result
		if(!root) {
			checkErrors(driver, onFailThrow);
			return std::vector<NodeAddress>();
		}

		return extractAddresses(root);
	}


} //  parser
} //  core
} // insime
