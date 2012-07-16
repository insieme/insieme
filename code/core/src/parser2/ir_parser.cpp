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

#include "insieme/core/parser2/ir_parser.h"

#include "insieme/core/ir_builder.h"
#include "insieme/core/parser2/simple_parser.h"

namespace insieme {
namespace core {
namespace parser {


	namespace {

		Grammar buildGrammar() {

			vector<RulePtr> rules;


			// handle primitives
//			rules.push_back(rule(
//					any,
//					[](Context& cur)->NodePtr {
//						// the list of terminals
//						static const string terminals = "+-*/%=()<>{}[]&|,:;?!~^°'´\\";
//						if (contains(terminals, (*cur.begin)[0])) return NodePtr();
//						return IRBuilder(cur.manager).stringValue(*cur.begin);
//					}
//			));

			// -------- Add int-type parameter rules --------

			rules.push_back(rule(
					any,
					[](Context& cur)->NodePtr {
						try {
							uint32_t value = utils::numeric_cast<uint32_t>(*cur.begin);
							return IRBuilder(cur.manager).concreteIntTypeParam(value);
						} catch (const boost::bad_lexical_cast&) {}
						return NodePtr();
					}
			));

			rules.push_back(rule(
					seq("#", "inf"),
					[](Context& cur)->NodePtr {
						return IRBuilder(cur.manager).infiniteIntTypeParam();
					}
			));

			rules.push_back(rule(
					seq("#", any),
					[](Context& cur)->NodePtr {
						const string& name = *(cur.end - 1);
						if (name.size() != 1u) return NodePtr();
						return IRBuilder(cur.manager).variableIntTypeParam(name[0]);
					}
			));


			// --------------- add type rules ---------------

			// add type variables
			rules.push_back(rule(
					seq("'", any),
					[](Context& cur)->NodePtr {
						const string& name = *(cur.end - 1);
						return IRBuilder(cur.manager).typeVariable(name);
					}
			));

			// add generic type
			rules.push_back(rule(
					seq(any, opt(seq("<", list(rec, ","), ">"))),
					[](Context& cur)->NodePtr {
						auto& terms = cur.getTerms();

						// extract type parameters
						TypeList typeParams;
						auto it = terms.begin();
						while (it != terms.end()) {
							if (TypePtr type = dynamic_pointer_cast<TypePtr>(*it)) {
								typeParams.push_back(type);
							} else if (dynamic_pointer_cast<IntTypeParamPtr>(*it)) {
								break;
							} else {
								return NodePtr();
							}
							++it;
						}

						// extract int-type parameters
						IntParamList intTypeParams;
						while(it != terms.end()) {
							if (IntTypeParamPtr param = dynamic_pointer_cast<IntTypeParamPtr>(*it)) {
								intTypeParams.push_back(param);
							} else {
								return NodePtr();
							}
							++it;
						}

						return IRBuilder(cur.manager).genericType(*cur.begin, typeParams, intTypeParams);
					}
			));

			// add tuple type
			rules.push_back(rule(
					seq("(", list(rec, ","), ")"),
					[](Context& cur)->NodePtr {
						auto& terms = cur.getTerms();
						TypeList types;
						for(const NodePtr& node : terms) {
							if (TypePtr type = dynamic_pointer_cast<TypePtr>(node)) {
								types.push_back(type);
							} else {
								return NodePtr();
							}
						}
						return IRBuilder(cur.manager).tupleType(types);
					}
			));

			// add function type
			rules.push_back(rule(
					seq("(", list(rec, ","), ")", "-", ">", rec),
					[](Context& cur)->NodePtr {
						auto& terms = cur.getTerms();
						assert(!terms.empty());
						TypeList types;
						for(const NodePtr& node : terms) {
							if (TypePtr type = dynamic_pointer_cast<TypePtr>(node)) {
								types.push_back(type);
							} else {
								return NodePtr();
							}
						}
						TypePtr resType = types.back();
						types.pop_back();
						return IRBuilder(cur.manager).functionType(types, resType, true);
					}
			));

			rules.push_back(rule(
					seq("(", list(rec, ","), ")", "=", ">", rec),
					[](Context& cur)->NodePtr {
						auto& terms = cur.getTerms();
						assert(!terms.empty());
						TypeList types;
						for(const NodePtr& node : terms) {
							if (TypePtr type = dynamic_pointer_cast<TypePtr>(node)) {
								types.push_back(type);
							} else {
								return NodePtr();
							}
						}
						TypePtr resType = types.back();
						types.pop_back();
						return IRBuilder(cur.manager).functionType(types, resType, false);
					}
			));

			// add expression rules


			// add statement rules


			return Grammar(rules);
		}

	}



	NodePtr parse(NodeManager& manager, const string& code, bool onFailThrow) {
		static Grammar inspire = buildGrammar();
		return inspire.match(manager, code, onFailThrow);
	}



} // end namespace parser2
} // end namespace core
} // end namespace insieme
