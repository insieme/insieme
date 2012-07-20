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

#include <sstream>

#include "insieme/core/ir_builder.h"
#include "insieme/core/parser2/parser.h"

namespace insieme {
namespace core {
namespace parser {


	namespace {

		// --- utilities for building rules ---

		NodePtr forward(Context& context) {
			assert(context.getTerms().size() == 1u);
			return context.getTerm(0);
		}

		NodePtr fail(Context& cur, const string& msg) {
			// check whether parser is currently within a speculative mode ..
			if (cur.isSpeculative()) {
				return NodePtr();
			}

			// build message and throw exception
			std::stringstream str;
			str << "Unable to parse '" << join(" ", cur.begin, cur.end, [](std::ostream& out, const Token& token) {
				out << token.getLexeme();
			}) << "' - reason: " << msg;
			throw ParseException(str.str());
		}

		template<typename Target, typename Source>
		const vector<Target>& convertList(const vector<Source>& source) {
			typedef typename std::remove_const<typename Target::element_type>::type element;
			// assert that conversion is save
			assert(all(source, [](const Source& cur) { return dynamic_pointer_cast<Target>(cur); }));
			return core::convertList<element>(source);	// use core converter
		}


		Grammar buildGrammar(const string& start = "N") {

			/**
			 * The Grammar is build as follows:
			 * 	Non-Terminals:
			 * 		P .. int-type parameters
			 * 		T .. types
			 * 		E .. expressions
			 * 		S .. statements
			 * 		N .. any ( P | T | E | S )
			 */

			auto P = rec("P");
			auto T = rec("T");
			auto E = rec("E");
			auto S = rec("S");
			auto N = rec("N");


			Grammar g(start);

			vector<RulePtr> rules;


			// -------- add int-type parameter rules --------

			g.addRule("P", rule(
					any(Token::Int_Literal),
					[](Context& cur)->NodePtr {
						uint32_t value = utils::numeric_cast<uint32_t>(*cur.begin);
						return IRBuilder(cur.manager).concreteIntTypeParam(value);
					}
			));

			g.addRule("P", rule(
					"#inf",
					[](Context& cur)->NodePtr {
						return IRBuilder(cur.manager).infiniteIntTypeParam();
					}
			));

			g.addRule("P", rule(
					seq("#", identifier),
					[](Context& cur)->NodePtr {
						const string& name = *(cur.end - 1);
						if (name.size() != 1u) return fail(cur, "int-type-parameter variable name must not be longer than a single character");
						return IRBuilder(cur.manager).variableIntTypeParam(name[0]);
					}
			));


			// --------------- add type rules ---------------

			// add type variables
			g.addRule("T", rule(
					seq("'", identifier),
					[](Context& cur)->NodePtr {
						const string& name = *(cur.end - 1);
						return IRBuilder(cur.manager).typeVariable(name);
					}
			));

			// add generic type
			g.addRule("T", rule(
					seq(identifier, opt(seq("<", list(P|T, ","), ">"))),
					[](Context& cur)->NodePtr {
						auto& terms = cur.getTerms();

						// extract type parameters
						TypeList typeParams;
						auto it = terms.begin();
						while (it != terms.end()) {
							if (it->getNodeCategory() == NC_Type) {
								typeParams.push_back(it->as<TypePtr>());
							} else if (it->getNodeCategory() == NC_IntTypeParam) {
								break;
							} else {
								assert(false && "Expecting Types and Parameters only!");
							}
							++it;
						}

						// extract int-type parameters
						IntParamList intTypeParams;
						while(it != terms.end()) {
							if (it->getNodeCategory() == NC_IntTypeParam) {
								intTypeParams.push_back(it->as<IntTypeParamPtr>());
							} else if (it->getNodeCategory() == NC_Type) {
								return fail(cur, "Type and int-parameters must be separated!");
							} else {
								assert(false && "Expecting Types and Parameters only!");
							}
							++it;
						}

						return IRBuilder(cur.manager).genericType(*cur.begin, typeParams, intTypeParams);
					}
			));

			// add tuple type
			g.addRule("T", rule(
					seq("(", list(T, ","), ")"),
					[](Context& cur)->NodePtr {
						return IRBuilder(cur.manager).tupleType(convertList<TypePtr>(cur.getTerms()));
					}
			));

			// add function type
			g.addRule("T", rule(
					seq("(", list(T, ","), ")->", T),
					[](Context& cur)->NodePtr {
						auto& terms = cur.getTerms();
						assert(!terms.empty());
						TypeList types = convertList<TypePtr>(terms);
						TypePtr resType = types.back();
						types.pop_back();
						return IRBuilder(cur.manager).functionType(types, resType, true);
					}
			));

			g.addRule("T", rule(
					seq("(", list(T, ","), ")=>", T),
					[](Context& cur)->NodePtr {
						auto& terms = cur.getTerms();
						assert(!terms.empty());
						TypeList types = convertList<TypePtr>(terms);
						TypePtr resType = types.back();
						types.pop_back();
						return IRBuilder(cur.manager).functionType(types, resType, false);
					}
			));


			// --------------- add literal rules ---------------


			// boolean
			g.addRule("E", rule(
					any(Token::Bool_Literal),
					[](Context& cur)->NodePtr {
						const auto& type = cur.manager.getLangBasic().getBool();
						return IRBuilder(cur.manager).literal(type, *cur.begin);
					}
			));

			// integers
			g.addRule("E", rule(
					any(Token::Int_Literal),
					[](Context& cur)->NodePtr {

						// determine type of literal
						const string& lexeme = cur.begin->getLexeme();
						bool lng = lexeme.back() == 'l';
						bool sig = (lng)?(*(lexeme.end()-2) == 'u'):(lexeme.back()=='u');

						auto& basic = cur.manager.getLangBasic();
						TypePtr type = (lng)?
									(sig)?basic.getUInt8():basic.getInt8()
								  : (sig)?basic.getUInt4():basic.getInt4();

						return IRBuilder(cur.manager).literal(type, lexeme);
					}
			));

			// floats
			g.addRule("E", rule(
					any(Token::Float_Literal),
					[](Context& cur)->NodePtr {
						const auto& type = cur.manager.getLangBasic().getReal4();
						return IRBuilder(cur.manager).literal(type, *cur.begin);
					}
			));

			// doubles
			g.addRule("E", rule(
					any(Token::Double_Literal),
					[](Context& cur)->NodePtr {
						const auto& type = cur.manager.getLangBasic().getReal8();
						return IRBuilder(cur.manager).literal(type, *cur.begin);
					}
			));


			// --------------- add expression rules ---------------

			// -- arithmetic expressions --

			g.addRule("E", rule(
					seq(E, "+", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).add(a,b);
					},
					12
			));

			g.addRule("E", rule(
					seq(E, "-", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).sub(a,b);
					},
					12
			));

			g.addRule("E", rule(
					seq(E, "*", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).mul(a,b);
					},
					13
			));

			g.addRule("E", rule(
					seq(E, "/", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).div(a,b);
					},
					13
			));

			g.addRule("E", rule(
					seq(E, "%", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).mod(a,b);
					},
					13
			));

			// -- logical expressions --

			g.addRule("E", rule(
					seq("!", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						return IRBuilder(cur.manager).logicNeg(a);
					},
					14
			));

			g.addRule("E", rule(
					seq(E, "&&", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).logicAnd(a,b);
					},
					5
			));

			g.addRule("E", rule(
					seq(E, "||", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).logicOr(a,b);
					},
					4
			));

			// -- parentheses --

			g.addRule("E", rule(seq("(", E, ")"), forward, 15));

			// --------------- add statement rules ---------------

			// every expression is a statement (if terminated by ;)
			g.addRule("S", rule(seq(E,";"), forward));

			// declaration statement
			g.addRule("S", rule(
					seq("decl", T, cap(identifier), " = ", E, ";"),
					[](Context& cur)->NodePtr {
						TypePtr type = cur.getTerm(0).as<TypePtr>();
						ExpressionPtr value = cur.getTerm(1).as<ExpressionPtr>();
						// TODO: register name + check type
						std::cout << "TODO: register " << cur.getSubRange(0).front().getLexeme() << " within symbol table!\n";
						return IRBuilder(cur.manager).declarationStmt(type, value);
					}
			));

			// compound statement
			g.addRule("S", rule(
					seq("{", loop(S), "}"),
					[](Context& cur)->NodePtr {
						return IRBuilder(cur.manager).compoundStmt(convertList<StatementPtr>(cur.getTerms()));
					}
			));

			// if-then
			g.addRule("S", rule(
					seq("if(", E, ")", S),
					[](Context& cur)->NodePtr {
						const auto& terms = cur.getTerms();
						ExpressionPtr condition = terms[0].as<ExpressionPtr>();
						StatementPtr thenPart = terms[1].as<StatementPtr>();
						if (!cur.manager.getLangBasic().isBool(condition->getType())) {
							return fail(cur, "Conditional expression is not a boolean expression!");
						}
						return IRBuilder(cur.manager).ifStmt(condition, thenPart);
					}
			));

			// if-then-else
			g.addRule("S", rule(
					seq("if(", E, ")", S, "else", S),
					[](Context& cur)->NodePtr {
						const auto& terms = cur.getTerms();
						ExpressionPtr condition = terms[0].as<ExpressionPtr>();
						StatementPtr thenPart = terms[1].as<StatementPtr>();
						StatementPtr elsePart = terms[2].as<StatementPtr>();
						if (!cur.manager.getLangBasic().isBool(condition->getType())) {
							return fail(cur, "Conditional expression is not a boolean expression!");
						}
						return IRBuilder(cur.manager).ifStmt(condition, thenPart, elsePart);
					},
					-1		// lower priority for this rule (default=0)
			));


			// while statement
			g.addRule("S", rule(
					seq("while(", E, ")", S),
					[](Context& cur)->NodePtr {
						const auto& terms = cur.getTerms();
						ExpressionPtr condition = terms[0].as<ExpressionPtr>();
						StatementPtr body = terms[1].as<StatementPtr>();
						if (!cur.manager.getLangBasic().isBool(condition->getType())) {
							return fail(cur, "Conditional expression is not a boolean expression!");
						}
						return IRBuilder(cur.manager).whileStmt(condition, body);
					}
			));


			// add productions for unknown node type N
			g.addRule("N", rule(P, forward));
			g.addRule("N", rule(T, forward));
			g.addRule("N", rule(E, forward));
			g.addRule("N", rule(S, forward));

			std::cout << g << "\n";

			return g;
		}

	}



	NodePtr parse(NodeManager& manager, const string& code, bool onFailThrow) {
		static const Grammar inspire = buildGrammar();
		return inspire.match(manager, code, onFailThrow);
	}

	TypePtr parse_type(NodeManager& manager, const string& code, bool onFailThrow) {
		static const Grammar g = buildGrammar("T");
		return g.match(manager, code, onFailThrow).as<TypePtr>();
	}

	ExpressionPtr parse_expr(NodeManager& manager, const string& code, bool onFailThrow) {
		static const Grammar g = buildGrammar("E");
		return g.match(manager, code, onFailThrow).as<ExpressionPtr>();
	}

	StatementPtr parse_stmt(NodeManager& manager, const string& code, bool onFailThrow) {
		static const Grammar g = buildGrammar("S");
		return g.match(manager, code, onFailThrow).as<StatementPtr>();
	}




} // end namespace parser2
} // end namespace core
} // end namespace insieme
