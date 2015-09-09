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

#include <algorithm>
#include <string>

#include "insieme/core/parser3/detail/driver.h"
#include "insieme/core/ir.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/naming.h"

#include "insieme/core/types/match.h"
#include "insieme/core/types/return_type_deduction.h"

#include "insieme/core/parser3/detail/scanner.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/varargs_extension.h"

#include "insieme/core/lang/extension_registry.h"

// this last one is generated and the path will be provided to the command
#include "inspire_parser.hpp"


namespace insieme {
namespace core {
namespace parser3 {
	namespace detail {

		/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ scope manager ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


		void DeclarationContext::open_scope(const std::string& msg) {
			stack.push_back(Scope());
			stack.back().isFunction = (msg == "function");
		}
		void DeclarationContext::close_scope(const std::string& msg) {
			assert_gt(stack.size(), 1) << "Attempted to pop global scope!";
			stack.pop_back();
		}

		bool DeclarationContext::add_symb(const std::string& name, const node_factory& factory) {
			auto& symbols = stack.back().symbols;
			if (symbols.find(name) != symbols.end()) return false;
			symbols[name] = factory;
			return true;
		}

		bool DeclarationContext::add_type(const std::string& name, const node_factory& factory) {
			auto& types = stack.back().types;
			if (types.find(name) != types.end()) return false;
			types[name] = factory;
			return true;
		}


		NodePtr DeclarationContext::find_symb(const std::string& name) const {
			// lookup symbols throughout scopes
			for(auto it = stack.rbegin(); it != stack.rend(); ++it) {
				const definition_map& cur = it->symbols;
				auto pos = cur.find(name);
				if (pos != cur.end()) return pos->second();
			}
			return nullptr;
		}

		NodePtr DeclarationContext::find_type(const std::string& name) const {
			// lookup symbols throughout scopes
			for(auto it = stack.rbegin(); it != stack.rend(); ++it) {
				const definition_map& cur = it->types;
				auto pos = cur.find(name);
				if (pos != cur.end()) return pos->second();
			}

			// check symbol table too -- as it might have been defined by the user
			NodePtr res = find_symb(name);
			if (auto type = res.isa<TypePtr>()) return type;

			// nothing found
			return nullptr;
		}

		bool DeclarationContext::isInFunctionDefinition() const {
			return stack.back().isFunction;
		}

		void DeclarationContext::add_type_alias(const GenericTypePtr& pattern, const TypePtr& substitute) {
			auto& aliases = stack.back().alias;
			aliases[pattern] = substitute;
		}

		TypePtr DeclarationContext::resolve(const TypePtr& type) const {
			if (!type || !type.isa<GenericTypePtr>()) return type;

			NodeManager& mgr = type.getNodeManager();

			// run through alias lists
			for(auto it = stack.rbegin(); it != stack.rend(); ++it) {
				for(const auto& cur : it->alias) {
					// check whether pattern matches
					if (auto sub = types::match(mgr, type, cur.first)) {
						// compute substitution
						auto next = (*sub).applyTo(cur.second);
						assert_ne(type, next) << "Invalid pattern matching:\n"
								"Target:     " << *type << "\n"
								"Pattern:    " << *cur.first << "\n"
								"Substitute: " << *cur.first << "\n"
								"Match:      " << *sub << "\n"
								"Next:       " << *next << "\n";

						// apply pattern and start over again
						return resolve(next);
					}
				}
			}

			// no matching alias fond => done
			return type;
		}

		/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ inspire_driver ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


		inspire_driver::inspire_driver(const std::string& str, NodeManager& mgr, const DeclarationContext& ctx)
		    : scopes(ctx), mgr(mgr), builder(mgr), file("global scope"), str(str), result(nullptr), glob_loc(&file), ss(str), scanner(&ss),
		      parser(*this, scanner), let_count(0), inhibit_building_count(false) {
		}

		inspire_driver::~inspire_driver() {}

		ProgramPtr inspire_driver::parseProgram() {
			scanner.set_start_program();
			int fail = parser.parse();
			if(fail) {
				// print_errors();
				return nullptr;
			}
			return result.as<ProgramPtr>();
		}

		TypePtr inspire_driver::parseType() {
			scanner.set_start_type();
			inspire_parser parser(*this, scanner);
			int fail = parser.parse();
			if(fail) {
				// print_errors();
				return nullptr;
			}
			return result.as<TypePtr>();
		}

		StatementPtr inspire_driver::parseStmt() {
			scanner.set_start_statement();
			inspire_parser parser(*this, scanner);
			int fail = parser.parse();
			if(fail) {
				// print_errors();
				return nullptr;
			}
			return result.as<StatementPtr>();
		}

		ExpressionPtr inspire_driver::parseExpression() {
			scanner.set_start_expression();
			inspire_parser parser(*this, scanner);
			int fail = parser.parse();
			if(fail) {
				// print_errors();
				return nullptr;
			}
			return result.as<ExpressionPtr>();
		}

		// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Some tools ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		ExpressionPtr inspire_driver::findSymbol(const location& l, const std::string& name) {
			auto x = scopes.find_symb(name);

			if(!x) {
				try {
					x = builder.getLangBasic().getBuiltIn(name);
				} catch(...) {
					// pass, nothing to do really
				}
			}

			if(!x) {
				error(l, format("the symbol %s was not declared in this context", name));
				return nullptr;
			}
			if(!x.isa<ExpressionPtr>()) {
				error(l, format("the symbol %s is not an expression (var/func)", name));
				return nullptr;
			}

			return x.as<ExpressionPtr>();
		}

		TypePtr inspire_driver::findType(const location& l, const std::string& name) {
			if(std::find(let_names.begin(), let_names.end(), name) != let_names.end()) {
				// this is a type in a let binding usage, it might be recursive, so we "mark" it
				return builder.typeVariable(name);
			}

			auto x = scopes.find_type(name);
			if(x && !x.isa<TypePtr>()) {
				error(l, format("the symbol %s is not a type", name));
				return nullptr;
			}
			return x.as<TypePtr>();
		}

		ExpressionPtr inspire_driver::getScalar(ExpressionPtr expr) {
			// auto-unwrap tuple
			if (auto tuple = expr.isa<TupleExprPtr>()) {
				const auto& elements = tuple->getExpressions();
				if (elements.size() == 1) {
					return getScalar(elements[0]);
				}
			}
			// otherwise stay as it is
			return expr;
		}

		ExpressionPtr inspire_driver::getOperand(ExpressionPtr expr) {
			return builder.tryDeref(getScalar(expr));
		}

		ExpressionPtr inspire_driver::genBinaryExpression(const location& l, const std::string& op, ExpressionPtr left, ExpressionPtr right) {
			// Interpret operator
			//  std::cout << op << std::endl;
			//  std::cout << " " << left << " : " << left->getType() << std::endl;
			//  std::cout << " " << right << " : " << right->getType() << std::endl;

			// assign
			// left side must be a ref, right side must be untouched
			if(op == "=") {
				if(!analysis::isRefType(left.getType())) {
					error(l, format("left side on assignment must be a reference and is %s", toString(left.getType())));
				} else if(analysis::getReferencedType(left.getType()) != right->getType()) {
					error(l, format("right side expression of type %s can not be assingend to type %s", toString(right.getType()), toString(left.getType())));
				}

				return builder.assign(left, right);
			}

			auto b = getOperand(right);
			// left side is untouched because of reference subscript operators
			if(op == "[") {
				if(builder.getLangBasic().isUnsignedInt(b->getType())) { b = builder.castExpr(builder.getLangBasic().getInt8(), b); }
				if(analysis::isRefType(left->getType())) {
					auto inType = analysis::getReferencedType(left->getType());
					if(!lang::isArray(inType)) {
						error(l, "expression is neither a vector nor array to subscript");
						return nullptr;
					}

					return builder.arrayRefElem(left, b);
				}
				auto inType = left->getType();
				if(!lang::isArray(inType)) {
					error(l, "expression is neither a vector nor array to subscript");
					return nullptr;
				}
				return builder.arraySubscript(left, b); // works for arrays and vectors
			}


			// if not assign, then left operand must be a value as well
			auto a = getOperand(left);

			// comparators
			if(op == "==") { return builder.eq(a, b); }
			if(op == "!=") { return builder.ne(a, b); }
			if(op == "<") { return builder.lt(a, b); }
			if(op == ">") { return builder.gt(a, b); }
			if(op == "<=") { return builder.le(a, b); }
			if(op == ">=") { return builder.ge(a, b); }

			// bitwise
			if(op == "&") { return builder.bitwiseAnd(a, b); }
			if(op == "|") { return builder.bitwiseOr(a, b); }
			if(op == "^") { return builder.bitwiseXor(a, b); }

			// logic
			if(op == "||") { return builder.logicOr(a, b); }
			if(op == "&&") { return builder.logicAnd(a, b); }

			// arithm
			if(op == "+") { return builder.add(a, b); }
			if(op == "-") { return builder.sub(a, b); }

			// geom
			if(op == "*") { return builder.mul(a, b); }
			if(op == "/") { return builder.div(a, b); }
			if(op == "%") { return builder.mod(a, b); }

			error(l, format("the symbol %s is not a operator", op));
			return nullptr;
		}

		ExpressionPtr inspire_driver::genFieldAccess(const location& l, const ExpressionPtr& expr, const std::string& fieldname) {
			if(!expr) {
				error(l, "no expression");
				return nullptr;
			}

			NamedCompositeTypePtr compositeType;
			if(expr->getType().isa<NamedCompositeTypePtr>()) {
				compositeType = expr->getType().as<NamedCompositeTypePtr>();
			} else if(analysis::isRefType(expr->getType())) {
				TypePtr type = analysis::getReferencedType(expr->getType());

				if(type.isa<RecTypePtr>()) { type = type.as<RecTypePtr>()->unroll(mgr); }

				compositeType = type.isa<NamedCompositeTypePtr>();
				if(!compositeType) {
					error(l, "Accessing element of non-named-composite type");
					return nullptr;
				}
			} else {
				error(l, "Accessing element of non-named-composite type %s");
				return nullptr;
			}

			// check field
			if(!compositeType->getNamedTypeEntryOf(fieldname)) {
				error(l, format("Accessing unknown field %s", fieldname));
				return nullptr;
			}

			// create access
			if(analysis::isRefType(expr->getType())) { return builder.refMember(expr, fieldname); }
			return builder.accessMember(expr, fieldname);
		}

		ExpressionPtr inspire_driver::genTupleAccess(const location& l, const ExpressionPtr& expr, const std::string& member) {
			// check whether access is valid
			TupleTypePtr tupleType;
			if(expr->getType()->getNodeType() == NT_TupleType) {
				tupleType = expr->getType().as<TupleTypePtr>();
			} else if(analysis::isRefType(expr->getType())) {
				TypePtr type = analysis::getReferencedType(expr->getType());

				if(type->getNodeType() == core::NT_RecType) { type = core::static_pointer_cast<const core::RecType>(type)->unroll(type.getNodeManager()); }

				tupleType = type.isa<TupleTypePtr>();
				if(!tupleType) {
					error(l, "Accessing element of non-tuple type");
					return nullptr;
				}
			} else {
				error(l, "Accessing element of non-tuple type");
				return nullptr;
			}

			// get index
			int index = utils::numeric_cast<int>(member);

			// check field
			if(index < 0 || index >= (int)tupleType.size()) {
				error(l, "Accessing unknown field");
				return nullptr;
			}

			// create access
			if(analysis::isRefType(expr->getType())) { return builder.refComponent(expr, index); }
			return builder.accessComponent(expr, index);
		}

		LiteralPtr inspire_driver::genNumericLiteral(const location& l, const TypePtr& type, const std::string& lit) {
			// remove any type modifier suffixes since they're already encoded in "type"
			return builder.literal(type, lit.substr(0, lit.find_first_not_of("0123456789-.+eE")));
		}

		TypePtr inspire_driver::genGenericType(const location& l, const std::string& name, const ParentList& parents, const TypeList& params) {
			if(name == "int") {
				if(params.size() != 1) { error(l, "wrong int size"); }
			}
			if(name == "real") {
				if(params.size() != 1) { error(l, "wrong real size"); }
			}
			for(const auto& p : params) {
				if(!p) {
					std::cerr << "wrong parameter in paramenter list" << std::endl;
					abort();
				}
			}

			return builder.genericType(name, parents, params);
		}

		TypePtr inspire_driver::genNumericType(const location& l, const ExpressionPtr& variable) const {
			if(!variable.isa<VariablePtr>()) {
				error(l, "not a variable");
			}
			return builder.numericType(variable.as<core::VariablePtr>());
		}

		TypePtr inspire_driver::genNumericType(const location& l, const string& value) const {
			return builder.numericType(builder.literal(value, builder.getLangBasic().getIntInf()));
		}

		TypePtr inspire_driver::genFuncType(const location& l, const TypeList& params, const TypePtr& retType, const FunctionKind& fk) {
			return builder.functionType(params, retType, fk);
		}

		TypePtr inspire_driver::resolveTypeAliases(const location& l, const TypePtr& type) {
			return scopes.resolve(type);
		}

		ExpressionPtr inspire_driver::genLambda(const location& l, const VariableList& params, const TypePtr& retType, const StatementPtr& body,
		                                        const FunctionKind& fk) {
			// TODO: cast returns to appropriate type
			TypeList paramTys;
			for(const auto& var : params) {
				paramTys.push_back(var.getType());
			}

			// if it is a function that is defined
			if (scopes.isInFunctionDefinition()) {
				// => skip materialization of parameters
				return builder.lambdaExpr(retType, params, body);
			}

			// replace all variables in the body by their implicitly materialized version
			auto lambdaIngredients = transform::materialize({params, body});

			// build resulting function type
			auto funcType = genFuncType(l, paramTys, retType, fk);
			return builder.lambdaExpr(funcType.as<FunctionTypePtr>(), lambdaIngredients.params, lambdaIngredients.body);
		}

		ExpressionPtr inspire_driver::genClosure(const location& l, const VariableList& params, StatementPtr stmt) {
			if(!stmt) {
				error(l, "closure statement malformed");
				return nullptr;
			}
			CallExprPtr call;
			if(stmt.isa<CallExprPtr>()) {
				call = stmt.as<CallExprPtr>();
			} else if(stmt->getNodeCategory() == NC_Expression) {
				call = builder.id(stmt.as<ExpressionPtr>());
			} else if(transform::isOutlineAble(stmt, true)) {
				call = transform::outline(builder.getNodeManager(), stmt, true);
			}

			// check whether call-conversion was successful
			if(!call) {
				error(l, "Not an outline-able context");
				return nullptr;
			}

			// build bind expression
			return builder.bindExpr(params, call);
		}

		ExpressionPtr inspire_driver::genCall(const location& l, const ExpressionPtr& callable, ExpressionList args) {
			ExpressionPtr func = callable;

			auto ftype = func->getType();
			if(!ftype.isa<FunctionTypePtr>()) { error(l, "attempt to call non function expression"); }

			auto funcParamTypes = ftype.as<FunctionTypePtr>()->getParameterTypeList();
			if(!funcParamTypes.empty()) {
				// fix variadic arguments
				if(lang::isVarList(*funcParamTypes.rbegin())) {
					if(args.size() < funcParamTypes.size()) {
						args.push_back(builder.pack(ExpressionList()));
					} else if(!lang::isVarList(args.rbegin()->getType())) {
						ExpressionList newParams(args.begin(), args.begin() + funcParamTypes.size() - 1);
						ExpressionList packParams(args.begin() + funcParamTypes.size(), args.end());
						newParams.push_back(builder.pack(packParams));
						std::swap(args, newParams);
					}
				}
			}

			if(args.size() != funcParamTypes.size()) {
				error(l, "invalid number of arguments in function call");
				return nullptr;
			}

			TypeList argumentTypes;
			::transform(args, back_inserter(argumentTypes), [](const ExpressionPtr& cur) { return cur->getType(); });

			TypePtr retType;
			try {
				retType = types::tryDeduceReturnType(ftype.as<FunctionTypePtr>(), argumentTypes);
			} catch(types::ReturnTypeDeductionException e) {
				error(l, format("Error in call expression:\n%s", e.what()));
				return nullptr;
			}

			ExpressionPtr res;
			try {
				res = builder.callExpr(retType, func, args);
			} catch(...) {
				error(l, "malformed call expression");
				return nullptr;
			}
			if(!res) { error(l, "malformed call expression"); }
			return res;
		}

		ExpressionPtr inspire_driver::genStructExpression(const location& l, const TypePtr& type, const ExpressionList& list) {
			if(!type) {
				error(l, "Not a struct type");
				return nullptr;
			}

			// retrieve struct type, (unroll rec types)
			StructTypePtr structType = type.isa<StructTypePtr>();
			if(type.isa<RecTypePtr>()) { structType = type.as<RecTypePtr>()->unroll(type.getNodeManager()).isa<StructTypePtr>(); }

			if(!structType) {
				error(l, format("Not a struct type: %s", toString(type)));
				return nullptr;
			}
			if(structType->size() != list.size()) {
				error(l, "init list does not match number of fields");
				return nullptr;
			}

			// build up struct expression
			auto begin = make_paired_iterator(structType->begin(), list.begin());
			auto end = make_paired_iterator(structType->end(), list.end());

			// extract name / value pairs
			NamedValueList values;
			for(auto it = begin; it != end; ++it) {
				values.push_back(builder.namedValue(it->first->getName(), it->second.as<ExpressionPtr>()));
			}

			// build struct expression
			return builder.structExpr(structType, values);
		}

		
		ExpressionPtr inspire_driver::genUnionExpression(const location& l, const TypePtr& type, const std::string field, const ExpressionPtr& expr) {

			// retrieve union type, (unroll rec types)
			UnionTypePtr unionType = type.isa<UnionTypePtr>();
			if(type.isa<RecTypePtr>()) { unionType = type.as<RecTypePtr>()->unroll(type.getNodeManager()).isa<UnionTypePtr>(); }

			if(!unionType) {
				error(l, format("Not a union type: %s", toString(type)));
				return nullptr;
			}
			if(!unionType->getNamedTypeEntryOf(field)) {
				error(l, "field name does not appear in union");
				return nullptr;
			}

			// build union expression
			return builder.unionExpr(unionType, builder.stringValue(field), expr);
		}

		// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Address marking   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		ExpressionPtr inspire_driver::mark_address(const location& l, const ExpressionPtr& expr) {
			NodePtr res = builder.markerExpr(expr);
			res->attachValue<AddressMark>();
			return res.as<ExpressionPtr>();
		}

		StatementPtr inspire_driver::mark_address(const location& l, const StatementPtr& stmt) {
			NodePtr res = builder.markerStmt(stmt);
			res->attachValue<AddressMark>();
			return res.as<StatementPtr>();
		}

		// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Scope management  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		void inspire_driver::add_symb(const location& l, const std::string& name, const node_factory&  factory) {

			if(name.find(".") != std::string::npos) {
				error(l, format("symbol names can not contain dot chars: %s", name));
				return;
			}

			// ignore wildcard for unused variables
			if(name == "_") { return; }

			if(!scopes.add_symb(name, factory)) { error(l, format("symbol %s redefined", name)); }
		}

		void inspire_driver::add_symb(const location& l, const std::string& name, const NodePtr& node) {
			add_symb(l, name, [=]() { return node; });
		}


		void inspire_driver::add_symb(const std::string& name, const node_factory& factory) {
			add_symb(glob_loc, name, factory);
		}

		void inspire_driver::add_symb(const std::string& name, const NodePtr& node) {
			add_symb(name, [=]() { return node; });
		}

		void inspire_driver::add_type_alias(const GenericTypePtr& pattern, const TypePtr& substitute) {
			scopes.add_type_alias(pattern, substitute);
		}

		void inspire_driver::add_type(const location& l, const std::string& name, const node_factory&  factory) {

			if(name.find(".") != std::string::npos) {
				error(l, format("symbol names can not contain dot chars: %s", name));
				return;
			}

			// ignore wildcard for unused variables
			if(name == "_") { return; }

			if(!scopes.add_type(name, factory)) { error(l, format("type name %s redefined", name)); }
		}

		void inspire_driver::add_type(const location& l, const std::string& name, const NodePtr& node) {
			add_type(l, name, [=]() { return node; });
		}


		void inspire_driver::add_type(const std::string& name, const node_factory& factory) {
			add_type(glob_loc, name, factory);
		}

		void inspire_driver::add_type(const std::string& name, const NodePtr& node) {
			add_type(name, [=]() { return node; });
		}


		namespace {

			bool contains_type_variables(const TypePtr& t, const TypeList& variables) {
				bool contains = false;
				visitDepthFirstOnce(t, [&](const TypePtr& t) {
					if(t.isa<TypeVariablePtr>() && (std::find(variables.begin(), variables.end(), t) != variables.end())) { contains = true; }
				});
				return contains;
			}

			// TODO: move this to string utils
			std::vector<std::string> split_string(const std::string& s) {
				std::vector<std::string> res;
				std::string delim = "\n";

				auto start = 0U;
				auto end = s.find(delim);

				if(end == std::string::npos) {
					auto tmp = s;
					std::replace(tmp.begin(), tmp.end(), '\t', ' ');
					res.push_back(tmp);
				}

				while(end != std::string::npos) {
					auto tmp = s.substr(start, end - start);
					std::replace(tmp.begin(), tmp.end(), '\t', ' ');
					res.push_back(tmp);
					start = end + delim.length();
					end = s.find(delim, start);
				}

				// copy last line
				auto tmp = s.substr(start, s.size() - 1);
				std::replace(tmp.begin(), tmp.end(), '\t', ' ');
				res.push_back(tmp);

				assert(res.size() > 0);
				return res;
			}

			std::string get_body_string(const std::string& text, const location& b, const location& e, bool isFunction) {
				auto tmp = text;

				auto strings = split_string(text);
				for(auto& s : strings) {
					s.append("\n");
				}

				std::vector<std::string> subset(strings.begin() + b.begin.line - 1, strings.begin() + e.end.line);
				std::string res;

				subset[subset.size() - 1] = subset[subset.size() - 1].substr(0, e.end.column - 1);
				for(auto it = subset.begin(); it < subset.end(); ++it) {
					res.append(*it);
				}
				res = res.substr(b.begin.column - 1, res.size());
				// the lambda or function keyword is lost during parsing, ammend
				return std::string((isFunction)?"function ":"lambda ").append(res);
			}
		}
		void inspire_driver::add_let_name(const location& l, const std::string& name) {
			let_names.push_back(name);
		}

		void inspire_driver::add_let_lambda(const location& l, const location& begin, const location& end, const TypePtr& retType, const VariableList& params,
		                                    const FunctionKind& fk) {
			if(inhibit_building_count > 1) { return; }
			lambda_lets.push_back(Lambda_let(retType, params, get_body_string(str, begin, end, scopes.isInFunctionDefinition()), fk));
		}

		void inspire_driver::add_this(const location& l, const TypePtr& classType) {
			// gen ref type
			auto refThis = builder.refType(classType);
			// gen var
			auto thisVar = builder.variable(refThis);
			// save in scope
			add_symb(l, "this", thisVar);
		}

		void inspire_driver::add_let_type(const location& l, const TypePtr& type) {
			type_lets.push_back(type);
		}

		void inspire_driver::add_let_expression(const location& l, const ExpressionPtr& expr) {
			if(!expr) {
				error(l, "no expression translated");
				return;
			}
			expr_lets.insert(expr_lets.begin(), expr);
		}

		void inspire_driver::close_let_statement(const location& l) {
			// if we are inside of a let in a let
			if(let_count > 1) {
				let_count--;
				set_inhibit(false);
				return;
			}

			// LAMBDA LETS (functions):
			if(let_names.size() == lambda_lets.size()) {
				// DeclarationContext temp_scope (scopes);

				std::map<std::string, VariablePtr> funcVars;
				unsigned count = 0;
				for(const auto& name : let_names) {
					Lambda_let& tmp = lambda_lets[count];

					TypeList types;
					for(const auto& v : tmp.params) {
						types.push_back(v->getType());
					}

					funcVars[name] = builder.variable(builder.functionType(types, tmp.retType, tmp.fk));
					count++;
				}

				// generate a nested parser to parse the body with the right types
				std::vector<std::pair<VariablePtr, LambdaExprPtr>> funcs;
				count = 0;
				for(const auto& let : lambda_lets) {
					inspire_driver let_driver(let.expression, mgr, scopes);
					for(const auto pair : funcVars) {
						let_driver.add_symb(pair.first, pair.second);
					}
					try {
						ExpressionPtr lambda = let_driver.parseExpression();

						if(!lambda) {
							// write the location with the offset of the current call site
							// FIXME: due to the new string, locations get messed up
							let_driver.print_errors(std::cerr);
							error(l, "lambda expression is wrong");

							return;
						}

						funcs.push_back({funcVars[let_names[count]], lambda.as<LambdaExprPtr>()});
					} catch(...) { error(l, "something went really wrong parsing lambda body"); }

					count++;
				}

				// if all variables in body are defined, this is regular function
				if(funcs.size() == 1 && analysis::getFreeVariables(funcs[0].second).empty()) {
					annotations::attachName(funcs[0].second, let_names[0]);
					add_symb(l, let_names[0], funcs[0].second);
				} else {
					// generate funcs and bound them to the var types
					std::vector<LambdaBindingPtr> lambdas;
					for(const auto& lf : funcs) {
						auto var = lf.first;
						auto params = lf.second->getParameterList();
						auto type = lf.second->getType();
						auto body = lf.second->getBody();
						lambdas.push_back(builder.lambdaBinding(var, builder.lambda(type.as<FunctionTypePtr>(), params, body)));
					}
					LambdaDefinitionPtr lambdaDef = builder.lambdaDefinition(lambdas);
					for(const auto& fv : funcVars) {
						add_symb(l, fv.first, builder.lambdaExpr(fv.second, lambdaDef));
					}
				}

				lambda_lets.clear();
			}
			// TYPE LETS
			else if(let_names.size() == type_lets.size()) {
				std::vector<RecTypeBindingPtr> type_defs;
				std::vector<std::string> names;
				NodeMap non_recursive;
				unsigned count = 0;

				TypeList variables;
				for(const auto& name : let_names) {
					variables.push_back(builder.typeVariable(name));
				}

				// check for non recursive types
				// if the type has no recursion inside, replace all uses of the type variable by a full type
				for(const auto& type : type_lets) {
					const std::string& name = let_names[count];
					if(!contains_type_variables(type, variables)) {
						non_recursive[builder.typeVariable(name)] = type;
						annotations::attachName(type, name);
						add_type(l, name, type);
					}
					count++;
				}

				count = 0;
				// go over the types again and produce recursive type bindings for those who need it
				for(const auto& type : type_lets) {
					const std::string& name = let_names[count];

					if(contains_type_variables(type, variables)) {
						auto tmp = transform::replaceAllGen(mgr, type, non_recursive);

						type_defs.push_back(builder.recTypeBinding(builder.typeVariable(name), tmp));
						names.push_back(name);
					}
					count++;
				}

				// generate a full recursive type and one entry point for each type variable
				RecTypeDefinitionPtr fullType = builder.recTypeDefinition(type_defs);
				for(const auto& n : names) {
					auto t = builder.recType(builder.typeVariable(n), fullType);
					annotations::attachName(t, n);
					add_type(l, n, t);
				}

				type_lets.clear();
			}
			// Expression LETS (includes closures)
			else if(let_names.size() == expr_lets.size()) {
				unsigned count = 0;
				for(const auto& name : let_names) {
					add_symb(l, name, expr_lets[count]);
					count++;
				}

				expr_lets.clear();
			} else {
				//            std::cout << "let count: " << let_count << std::endl;
				//            std::cout << " ihibit? " << inhibit_building_count <<  std::endl;
				//
				//            std::cout << "let_mames " << let_names << std::endl;
				//            std::cout << "let_type " << type_lets << std::endl;
				//            std::cout << "let_expr " << expr_lets << std::endl;
				//            std::cout << "let_lambd " << lambda_lets.size() << std::endl;

				if(lambda_lets.size() + type_lets.size() + expr_lets.size() != let_names.size()) {
					error(l, "not all let names were defined");
					return;
				}

				error(l, "mixed type/function/closure let not allowed");
			}


			let_names.clear();
			let_count--;
			set_inhibit(false);
		}

		void inspire_driver::open_scope(const location& l, const std::string& name) {
			scopes.open_scope(name);
		}

		void inspire_driver::close_scope(const location& l, const std::string& name) {
			scopes.close_scope(name);
		}


		void inspire_driver::set_inhibit(bool flag) {
			if(!flag) {
				if(inhibit_building_count > 0) { inhibit_building_count--; }
			} else {
				inhibit_building_count++;
			}
		}
		bool inspire_driver::inhibit_building() const {
			return inhibit_building_count > 0;
		}


		void inspire_driver::using_scope_handle(const location& l, const std::vector<std::string>& extension_names) {
			for(std::string extensionName : extension_names) {
				extensionName.replace(0, 1, "");
				extensionName.replace(extensionName.size() - 1, 1, "");

				// get extension factory from the registry
				auto optFactory = lang::ExtensionRegistry::getInstance().lookupExtensionFactory(extensionName);
				if (optFactory) {
					import_extension((*optFactory)(mgr));
				} else {
					error(l, format("Unable to locate module: %s", extensionName.c_str()));
				}
			}
		}

		void inspire_driver::import_extension(const lang::Extension& extension) {

			// import symbols
			for(const auto& cur : extension.getSymbols()) {
				add_symb(cur.first, cur.second);
			}

			// import type aliases
			for(const auto& cur : extension.getTypeAliases()) {
				add_type_alias(cur.first, cur.second);
			}

		}

		// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Debug tools  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		namespace {

			// TODO: move this to  utils
			const std::string RED = "\033[31m";
			const std::string GREEN = "\033[32m";
			const std::string BLUE = "\033[34m";
			const std::string BLACK = "\033[30m";
			const std::string CYAN = "\033[96m";
			const std::string YELLOW = "\033[33m";
			const std::string GREY = "\033[37m";

			const std::string RESET = "\033[0m";
			const std::string BOLD = "\033[1m";

		} // annon

		void inspire_driver::print_location(const location& l) const {
			auto buffer = split_string(str);
			int line = 1;

			//  int lineb = l.begin.line;
			int linee = l.end.line;

			// wanna print the previous code? use something like this
			// for (; line< lineb; ++line);

			line = linee;
			std::cout << buffer[line - 1] << std::endl;

			int colb = l.begin.column;
			int cole = l.end.column;

			for(int i = 0; i < colb - 1; ++i) {
				std::cout << " ";
			}
			std::cout << GREEN << "^";
			for(int i = 0; i < cole - colb - 1; ++i) {
				std::cout << "~";
			}
			std::cout << RESET << std::endl;
		}
		// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Error management  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		void inspire_driver::error(const location& l, const std::string& m) const {
			errors.push_back(t_error(l, m));
		}

		void inspire_driver::error(const std::string& m) const {
			errors.push_back(t_error(glob_loc, m));
		}

		bool inspire_driver::where_errors() const {
			if(!errors.empty()) { print_errors(); }
			return !errors.empty();
		}


		void inspire_driver::print_errors(std::ostream& out, bool color) const {
			auto buffer = split_string(str);
			int line = 1;
			for(const auto& err : errors) {
				int lineb = err.l.begin.line;
				int linee = err.l.end.line;
				int colb = err.l.begin.column;
				int cole = err.l.end.column;

				if(color) { out << RED; }
				out << "ERROR: ";
				if(color) { out << RESET; }
				out << err.l << " " << err.msg << std::endl;

				//		std::cout << "=====" << std::endl;
				//		std::cout << "beg " << lineb << ":" << colb << std::endl;
				//		std::cout << "end " << linee << ":" << cole << std::endl;

				//		std::cout << "buff " << str << std::endl;
				//		std::cout << "buff " << buffer << std::endl;

				assert_true(lineb > 0);
				assert_true(linee > 0);
				assert_true(buffer.size() > 0);
				assert_true(lineb <= (int)buffer.size()) << "line beg " << lineb << " : buffer size " << buffer.size() << " \n" << buffer;
				assert_true(linee <= (int)buffer.size()) << "line end " << linee << " : buffer size " << buffer.size() << " \n" << buffer;

				line = linee;
				out << buffer[line - 1] << std::endl;


				for(int i = 0; i < colb - 1; ++i) {
					out << " ";
				}
				if(color) { out << GREEN; }
				out << "^";
				for(int i = 0; i < cole - colb - 1; ++i) {
					out << "~";
				}
				if(color) { out << RESET; }
				out << std::endl;
			}
		}


	} // namespace detail
} // namespace parser3
} // namespace core
} // namespace insieme
