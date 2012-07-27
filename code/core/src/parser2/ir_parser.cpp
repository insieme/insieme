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

			// add function types
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

			// add struct and union types
			struct process_named_type : public detail::actions {
				void accept(Context& cur, const TokenIter& begin, const TokenIter& end) const {
					TypePtr type = cur.getTerms().back().as<TypePtr>();
					auto iterName = cur.getSubRange(0);
					NamedTypePtr member = IRBuilder(cur.manager).namedType(iterName[0], type);
					cur.swap(member);
					cur.popRange();
				}
			};

			static const auto member = std::make_shared<Action<process_named_type>>(seq(T, cap(identifier)));

			g.addRule("T", rule(
					seq("struct {", loop(seq(member, ";")), "}"),
					[](Context& cur)->NodePtr {
						auto& terms = cur.getTerms();
						NamedTypeList members = convertList<NamedTypePtr>(terms);
						return IRBuilder(cur.manager).structType(members);
					}
			));

			g.addRule("T", rule(
					seq("union {", loop(seq(member, ";")), "}"),
					[](Context& cur)->NodePtr {
						auto& terms = cur.getTerms();
						NamedTypeList members = convertList<NamedTypePtr>(terms);
						return IRBuilder(cur.manager).unionType(members);
					}
			));


			// add array, vector, ref and channel types
			g.addRule("T", rule(
					seq("array<", T, ",", P, ">"),
					[](Context& cur)->NodePtr {
						TypePtr elementType = cur.getTerm(0).as<TypePtr>();
						IntTypeParamPtr dim = cur.getTerm(1).as<IntTypeParamPtr>();
						return IRBuilder(cur.manager).arrayType(elementType, dim);
					},
					1    // higher priority than generic type rule
			));

			g.addRule("T", rule(
					seq("vector<", T, ",", P, ">"),
					[](Context& cur)->NodePtr {
						TypePtr elementType = cur.getTerm(0).as<TypePtr>();
						IntTypeParamPtr size = cur.getTerm(1).as<IntTypeParamPtr>();
						return IRBuilder(cur.manager).vectorType(elementType, size);
					},
					1    // higher priority than generic type rule
			));
			g.addRule("T", rule(
					seq("ref<", T, ">"),
					[](Context& cur)->NodePtr {
						TypePtr elementType = cur.getTerm(0).as<TypePtr>();
						return IRBuilder(cur.manager).refType(elementType);
					},
					1    // higher priority than generic type rule
			));
			g.addRule("T", rule(
					seq("channel<", T, ",", P, ">"),
					[](Context& cur)->NodePtr {
						TypePtr elementType = cur.getTerm(0).as<TypePtr>();
						IntTypeParamPtr size = cur.getTerm(1).as<IntTypeParamPtr>();
						return IRBuilder(cur.manager).channelType(elementType, size);
					},
					1    // higher priority than generic type rule
			));

			// add named types
			g.addRule("T", rule(
					cap(identifier),
					[](Context& cur)->NodePtr {
						// simply lookup name within variable manager
						auto res = cur.getSymbolManager().lookup(cur.getSubRange(0));
						return dynamic_pointer_cast<TypePtr>(res);
					},
					1    // higher priority than generic type rule
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
					-12
			));

			g.addRule("E", rule(
					seq(E, "-", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).sub(a,b);
					},
					-12
			));

			g.addRule("E", rule(
					seq(E, "*", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).mul(a,b);
					},
					-13
			));

			g.addRule("E", rule(
					seq(E, "/", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).div(a,b);
					},
					-13
			));

			g.addRule("E", rule(
					seq(E, "%", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).mod(a,b);
					},
					-13
			));

			// -- logical expressions --

			g.addRule("E", rule(
					seq("!", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						return IRBuilder(cur.manager).logicNeg(a);
					},
					-14
			));

			g.addRule("E", rule(
					seq(E, "&&", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).logicAnd(a,b);
					},
					-5
			));

			g.addRule("E", rule(
					seq(E, "||", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).logicOr(a,b);
					},
					-4
			));

			// -- comparison expressions --

			g.addRule("E", rule(
					seq(E, "<", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).lt(a,b);
					},
					-10
			));

			g.addRule("E", rule(
					seq(E, "<=", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).le(a,b);
					},
					-10
			));

			g.addRule("E", rule(
					seq(E, ">=", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).ge(a,b);
					},
					-10
			));

			g.addRule("E", rule(
					seq(E, ">", E),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).gt(a,b);
					},
					-10
			));

			// -- vector / array access --

			g.addRule("E", rule(
					seq(E, "[", E, "]"),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						ExpressionPtr b = cur.getTerm(1).as<ExpressionPtr>();
						return IRBuilder(cur.manager).arraySubscript(a, b);		// works for arrays and vectors
					},
					-15
			));

			// member access
			g.addRule("E", rule(
					seq(E, ".", cap(identifier)),
					[](Context& cur)->NodePtr {
						ExpressionPtr a = cur.getTerm(0).as<ExpressionPtr>();
						return IRBuilder(cur.manager).accessMember(a, cur.getSubRange(0)[0]);
					},
					-15
			));

			// -- parentheses --

			g.addRule("E", rule(seq("(", E, ")"), forward, 15));

			// -- add Variable --
			g.addRule("E", rule(
					cap(identifier),
					[](Context& cur)->NodePtr {
						// simply lookup name within variable manager
						NodePtr res = cur.getVarScopeManager().lookup(cur.getSubRange(0));
						if (res) return res;
						return cur.getSymbolManager().lookup(cur.getSubRange(0));
					},
					1 // higher priority than other rules
			));

			g.addRule("E", rule(
					cap(identifier),
					[](Context& cur)->NodePtr {
						// simply lookup name within variable manager
						auto res = cur.getSymbolManager().lookup(cur.getSubRange(0));
						return dynamic_pointer_cast<ExpressionPtr>(res);
					},
					1    // higher priority than generic type rule
			));

			// -- call expression --
			g.addRule("E", rule(
					seq(E,"(", list(E,","), ")"),
					[](Context& cur)->NodePtr {
						NodeList terms = cur.getTerms();
						// get function
						ExpressionPtr fun = terms.front().as<ExpressionPtr>();
						terms.erase(terms.begin());
						return IRBuilder(cur.manager).callExpr(fun, convertList<ExpressionPtr>(terms));
					}
			));



			struct register_param : public detail::actions {
				void accept(Context& cur, const TokenIter& begin, const TokenIter& end) const {
					TypePtr type = cur.getTerms().back().as<TypePtr>();
					auto paramName = cur.getSubRange(0);
					VariablePtr param = IRBuilder(cur.manager).variable(type);
					cur.getVarScopeManager().add(paramName, param);
					cur.swap(param);	// exchange type with variable
					cur.popRange();		// drop name from stack
				}
			};

			static const auto param = std::make_shared<Action<register_param>>(seq(T, cap(identifier)));

			// function expressions
			g.addRule("E", rule(
					newScop(seq("(", list(param,","), ")->", T, S)),
					[](Context& cur)->NodePtr {
						// construct the lambda
						NodeList terms = cur.getTerms();
						StatementPtr body = terms.back().as<StatementPtr>();
						terms.pop_back();
						TypePtr resType = terms.back().as<TypePtr>();
						terms.pop_back();
						return IRBuilder(cur.manager).lambdaExpr(resType, body, convertList<VariablePtr>(terms));
					}
			));

			// --------------- add statement rules ---------------

			// every expression is a statement (if terminated by ;)
			g.addRule("S", rule(seq(E,";"), forward));

			// declaration statement
			g.addRule("S", rule(
					seq(T, cap(identifier), " = ", E, ";"),
					[](Context& cur)->NodePtr {
						TypePtr type = cur.getTerm(0).as<TypePtr>();
						ExpressionPtr value = cur.getTerm(1).as<ExpressionPtr>();
						auto decl = IRBuilder(cur.manager).declarationStmt(type, value);
						// register name within variable manager
						cur.getVarScopeManager().add(cur.getSubRange(0), decl->getVariable());
						return decl;
					}
			));

			// declaration statement
			g.addRule("S", rule(
					seq(T, cap(identifier), ";"),
					[](Context& cur)->NodePtr {
						IRBuilder builder(cur.manager);
						TypePtr type = cur.getTerm(0).as<TypePtr>();
						ExpressionPtr value = builder.undefined(type);
						auto decl = builder.declarationStmt(type, value);
						// register name within variable manager
						cur.getVarScopeManager().add(cur.getSubRange(0), decl->getVariable());
						return decl;
					}
			));

			// auto-declaration statement
			g.addRule("S", rule(
					seq("auto", cap(identifier), " = ", E, ";"),
					[](Context& cur)->NodePtr {
						ExpressionPtr value = cur.getTerm(0).as<ExpressionPtr>();
						auto decl = IRBuilder(cur.manager).declarationStmt(value->getType(), value);
						// register name within variable manager
						cur.getVarScopeManager().add(cur.getSubRange(0), decl->getVariable());
						return decl;
					},
					1 // higher priority than ordinary declaration
			));

			// declaration statement
			g.addRule("S", rule(
				seq("let", cap(identifier), "=", (T | E), ";"),
				[](Context& cur)->NodePtr {
					// register name within symbol manager
					cur.getSymbolManager().add(cur.getSubRange(0), cur.getTerm(0));
					return IRBuilder(cur.manager).getNoOp();
				},
				1 //  higher priority than variable declaration
			));

			// compound statement
			g.addRule("S", rule(
					varScop(seq("{", loop(S), "}")),
					[](Context& cur)->NodePtr {
						IRBuilder builder(cur.manager);
						StatementList list;		// filter out no-ops
						for(const StatementPtr& stmt : convertList<StatementPtr>(cur.getTerms())) {
							if (!builder.isNoOp(stmt)) list.push_back(stmt);
						}
						return builder.compoundStmt(list);
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

			struct register_var : public detail::actions {
				void accept(Context& cur, const TokenIter& begin, const TokenIter& end) const {
					TypePtr type = cur.getTerm(0).as<TypePtr>();
					auto iterName = cur.getSubRange(0);
					VariablePtr iter = IRBuilder(cur.manager).variable(type);
					cur.getVarScopeManager().add(iterName, iter);
					cur.push(iter);
				}
			};

			static const auto iter = std::make_shared<Action<register_var>>(seq(T, cap(identifier)));

			// for loop without step size
			g.addRule("S", rule(
					varScop(seq("for(", iter, "=", E, "..", E, ")", S)),
					[](Context& cur)->NodePtr {
						const auto& terms = cur.getTerms();
						TypePtr type = terms[0].as<TypePtr>();
						VariablePtr iter = terms[1].as<VariablePtr>();
						ExpressionPtr start = terms[2].as<ExpressionPtr>();
						ExpressionPtr end = terms[3].as<ExpressionPtr>();
						StatementPtr body = terms[4].as<StatementPtr>();

						auto& basic = cur.manager.getLangBasic();
						if (!basic.isInt(type)) return fail(cur, "Iterator has to be of integer type!");

						IRBuilder builder(cur.manager);

						// build loop
						ExpressionPtr step = builder.literal(type, "1");
						return IRBuilder(cur.manager).forStmt(iter, start, end, step, body);
					}
			));

			// for loop with step size
			g.addRule("S", rule(
					varScop(seq("for(", iter, "=", E, "..", E, ":", E, ")", S)),
					[](Context& cur)->NodePtr {
						const auto& terms = cur.getTerms();
						TypePtr type = terms[0].as<TypePtr>();
						VariablePtr iter = terms[1].as<VariablePtr>();
						ExpressionPtr start = terms[2].as<ExpressionPtr>();
						ExpressionPtr end = terms[3].as<ExpressionPtr>();
						ExpressionPtr step = terms[4].as<ExpressionPtr>();
						StatementPtr body = terms[5].as<StatementPtr>();

						auto& basic = cur.manager.getLangBasic();
						if (!basic.isInt(type)) return fail(cur, "Iterator has to be of integer type!");

						// build loop
						return IRBuilder(cur.manager).forStmt(iter, start, end, step, body);
					}
			));

			g.addRule("S", rule(
					seq("return ", E, ";"),
					[](Context& cur)->NodePtr {
						return IRBuilder(cur.manager).returnStmt(cur.getTerm(0).as<ExpressionPtr>());
					},
					1		// higher priority than variable declaration (of type return)
			));

			g.addRule("S", rule(
					seq("return;"),
					[](Context& cur)->NodePtr {
						return IRBuilder(cur.manager).returnStmt(
								cur.manager.getLangBasic().getUnitConstant()
						);
					}
			));

			g.addRule("S", rule(
					seq("break;"),
					[](Context& cur)->NodePtr {
						return IRBuilder(cur.manager).breakStmt();
					}
			));

			g.addRule("S", rule(
					seq("continue;"),
					[](Context& cur)->NodePtr {
						return IRBuilder(cur.manager).continueStmt();
					}
			));




			// add productions for unknown node type N
			g.addRule("N", rule(P, forward));
			g.addRule("N", rule(T, forward));
			g.addRule("N", rule(E, forward));
			g.addRule("N", rule(S, forward));

//			std::cout << g << "\n\n";
//			std::cout << g.getTermInfo() << "\n";

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
