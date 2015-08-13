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

#include "insieme/core/parser3/ir_parser.h"

#include <sstream>

#include "insieme/core/ir_builder.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"


#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_mapper_utils.h"
#include "insieme/core/encoder/lists.h"

#include "insieme/core/parser3/detail/driver.h"

namespace insieme {
namespace core {
namespace parser3 {

	using namespace detail;

	namespace {

		void checkErrors(inspire_driver& driver, bool onFailThrow) {
			if(!driver.result) {
				if(onFailThrow) {
					std::stringstream ss;
					driver.print_errors(ss);
					throw IRParserException(ss.str());
				} else {
					driver.print_errors();
				}
			}
		}

		void save_symbol_table(inspire_driver& driver, const std::map<string, NodePtr>& definitions) {
			for(const auto& def : definitions) {
				// the use of the line: symbols["name"] = builder.parseX("....", symbols);
				// will append a symbols with a null ptr inside. this is not good
				if(def.second) { driver.add_symb(def.first, def.second); }
			}
		}
	}

	TypePtr parse_type(NodeManager& manager, const string& code, bool onFailThrow, const std::map<string, NodePtr>& definitions) {
		inspire_driver driver(code, manager);
		save_symbol_table(driver, definitions);
		auto x = driver.parseType();
		if(!x) { checkErrors(driver, onFailThrow); }
		return x;
	}

	ExpressionPtr parse_expr(NodeManager& manager, const string& code, bool onFailThrow, const std::map<string, NodePtr>& definitions) {
		inspire_driver driver(code, manager);
		save_symbol_table(driver, definitions);
		auto x = driver.parseExpression();
		if(!x) { checkErrors(driver, onFailThrow); }
		return x;
	}

	StatementPtr parse_stmt(NodeManager& manager, const string& code, bool onFailThrow, const std::map<string, NodePtr>& definitions) {
		inspire_driver driver(code, manager);
		save_symbol_table(driver, definitions);
		auto x = driver.parseStmt();
		if(!x) { checkErrors(driver, onFailThrow); }
		return x;
	}

	ProgramPtr parse_program(NodeManager& manager, const string& code, bool onFailThrow, const std::map<string, NodePtr>& definitions) {
		inspire_driver driver(code, manager);
		save_symbol_table(driver, definitions);
		auto x = driver.parseProgram();
		if(!x) { checkErrors(driver, onFailThrow); }
		return x;
	}

	NodePtr parse_any(NodeManager& manager, const string& code, bool onFailThrow, const std::map<string, NodePtr>& definitions) {
		NodePtr x;
		{
			inspire_driver driver(code, manager);
			save_symbol_table(driver, definitions);
			x = driver.parseType();
		}
		if(!x) {
			inspire_driver driver(code, manager);
			save_symbol_table(driver, definitions);
			x = driver.parseExpression();
		}
		if(!x) {
			inspire_driver driver(code, manager);
			save_symbol_table(driver, definitions);
			x = driver.parseStmt();
		}
		if(!x) {
			inspire_driver driver(code, manager);
			save_symbol_table(driver, definitions);
			x = driver.parseProgram();
		}

		if(!x) {
			std::cerr << "No way to parse the string, and because you use generic parse there is no way to provide errors, good luck there!" << std::endl;
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


		std::vector<NodeAddress> extract_addresses(NodePtr root) {
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
			});

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


	std::vector<NodeAddress> parse_addresses_statement(NodeManager& manager, const string& code, bool onFailThrow,
	                                                   const std::map<string, NodePtr>& definitions) {
		inspire_driver driver(code, manager);
		save_symbol_table(driver, definitions);
		auto root = driver.parseStmt();

		// check the result
		if(!root) {
			checkErrors(driver, onFailThrow);
			return std::vector<NodeAddress>();
		}

		return extract_addresses(root);
	}

	std::vector<NodeAddress> parse_addresses_program(NodeManager& manager, const string& code, bool onFailThrow, const std::map<string, NodePtr>& definitions) {
		inspire_driver driver(code, manager);
		save_symbol_table(driver, definitions);
		auto root = driver.parseProgram();

		// check the result
		if(!root) {
			checkErrors(driver, onFailThrow);
			return std::vector<NodeAddress>();
		}

		return extract_addresses(root);
	}


} //  parser3
} //  core
} // insime
