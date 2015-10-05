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


		void DeclarationContext::open_scope() {
			stack.push_back(Scope());
		}
		void DeclarationContext::close_scope() {
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
			// nothing found
			return nullptr;
		}

		NodePtr DeclarationContext::find_type(const std::string& name) const {
			// lookup symbols throughout scopes
			for(auto it = stack.rbegin(); it != stack.rend(); ++it) {
				const definition_map& cur = it->types;
				auto pos = cur.find(name);
				if (pos != cur.end()) return pos->second();
			}
			// nothing found
			return nullptr;
		}

		void DeclarationContext::add_type_alias(const GenericTypePtr& pattern, const TypePtr& substitute) {
			auto& aliases = stack.back().alias;
			assert_true(aliases.find(pattern) == aliases.end());
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


		/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ InspireDriver ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

		InspireDriver::InspireDriver(const std::string& str, NodeManager& mgr)
		    : scopes(), mgr(mgr), builder(mgr), file("global scope"), str(str), result(nullptr), glob_loc(&file), ss(str), scanner(&ss),
		      parser(*this, scanner) {
		}

		InspireDriver::~InspireDriver() {}

		ProgramPtr InspireDriver::parseProgram() {
			scanner.set_start_program();
			int fail = parser.parse();
			if(fail) {
				// print_errors();
				return nullptr;
			}
			return result.as<ProgramPtr>();
		}

		TypePtr InspireDriver::parseType() {
			scanner.set_start_type();
			inspire_parser parser(*this, scanner);
			int fail = parser.parse();
			if(fail) {
				// print_errors();
				return nullptr;
			}
			return result.as<TypePtr>();
		}

		StatementPtr InspireDriver::parseStmt() {
			scanner.set_start_statement();
			inspire_parser parser(*this, scanner);
			int fail = parser.parse();
			if(fail) {
				// print_errors();
				return nullptr;
			}
			return result.as<StatementPtr>();
		}

		ExpressionPtr InspireDriver::parseExpression() {
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

		ExpressionPtr InspireDriver::findSymbol(const location& l, const std::string& name) {
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
				error(l, format("the symbol %s is not an expression but of type %s", name, x->getNodeType()));
				return nullptr;
			}

			return x.as<ExpressionPtr>();
		}

		TypePtr InspireDriver::findType(const location& l, const std::string& name) {

			// search types defined in current scope
			auto x = scopes.find_type(name);
			if(x && !x.isa<TypePtr>()) {
				error(l, format("the symbol %s is not a type", name));
				return nullptr;
			}

			// done
			return x.as<TypePtr>();
		}

		ExpressionPtr InspireDriver::getScalar(ExpressionPtr expr) {
			// auto-unwrap tuple
			if(auto tuple = expr.isa<TupleExprPtr>()) {
				const auto& elements = tuple->getExpressions();
				if (elements.size() == 1) {
					return getScalar(elements[0]);
				}
			}
			// otherwise stay as it is
			return expr;
		}

		ExpressionPtr InspireDriver::getOperand(ExpressionPtr expr) {
			return builder.tryDeref(getScalar(expr));
		}

		ExpressionPtr InspireDriver::genBinaryExpression(const location& l, const std::string& op, ExpressionPtr left, ExpressionPtr right) {
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
						error(l, format("expression is neither a vector nor array to subscript - type: %s", inType));
						return nullptr;
					}

					return builder.arrayRefElem(left, b);
				}
				auto inType = left->getType();
				if(!lang::isArray(inType)) {
					error(l, format("expression is neither a vector nor array to subscript - type: %s", inType));
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

		namespace {

			TypePtr getFieldType(const TypePtr& type, const std::string& name) {

				// remove references
				if (analysis::isRefType(type)) return getFieldType(analysis::getReferencedType(type), name);

				// check for a tag type
				auto tagType = type.isa<TagTypePtr>();
				if (!tagType) return TypePtr();

				// unroll recursive types
				if (tagType->isRecursive()) return getFieldType(tagType->peel(), name);

				// get field type
				return tagType->getFieldType(name);
			}

		}


		ExpressionPtr InspireDriver::genFieldAccess(const location& l, const ExpressionPtr& expr, const std::string& fieldname) {
			if(!expr) {
				error(l, "no expression");
				return nullptr;
			}

			// check that there is such a field
			if (!getFieldType(expr->getType(), fieldname)) {
				error(l, format("Unable to locate field %s in type %s", fieldname, *expr->getType()));
				return nullptr;
			}

			// create access
			if(analysis::isRefType(expr->getType())) {
				return builder.refMember(expr, fieldname);
			}
			return builder.accessMember(expr, fieldname);
		}

		ExpressionPtr InspireDriver::genTupleAccess(const location& l, const ExpressionPtr& expr, const std::string& member) {
			// check whether access is valid
			TupleTypePtr tupleType;
			if(expr->getType()->getNodeType() == NT_TupleType) {
				tupleType = expr->getType().as<TupleTypePtr>();
			} else if(analysis::isRefType(expr->getType())) {
				TypePtr type = analysis::getReferencedType(expr->getType());

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

		LiteralPtr InspireDriver::genNumericLiteral(const location& l, const TypePtr& type, const std::string& lit) {
			// remove any type modifier suffixes since they're already encoded in "type"
			return builder.literal(type, lit.substr(0, lit.find_first_not_of("0123456789-.+eE")));
		}

		TypePtr InspireDriver::genGenericType(const location& l, const std::string& name, const ParentList& parents, const TypeList& params) {
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

		TypePtr InspireDriver::genNumericType(const location& l, const ExpressionPtr& variable) const {
			if(!variable.isa<VariablePtr>()) {
				error(l, "not a variable");
			}
			return builder.numericType(variable.as<core::VariablePtr>());
		}

		TypePtr InspireDriver::genNumericType(const location& l, const string& value) const {
			return builder.numericType(builder.literal(value, builder.getLangBasic().getUIntInf()));
		}

		TypePtr InspireDriver::genFuncType(const location& l, const TypeList& params, const TypePtr& retType, const FunctionKind& fk) {
			return builder.functionType(params, retType, fk);
		}

		TypePtr InspireDriver::genRecordType(const location& l, const NodeType& type, const string& name, const ParentList& parents, const FieldList& fields, const LambdaExprList& ctors,
				const LambdaExprPtr& dtor, const MemberFunctionList& mfuns, const PureVirtualMemberFunctionList& pvmfuns) {
			if (type == NT_Struct) {
				return builder.structType(name,parents,fields,ctors,dtor,mfuns,pvmfuns);
			} else {
				if (!parents.empty()) error(l, "Inheritance not supported for unions!");
				return builder.unionType(name,fields,ctors,dtor,mfuns,pvmfuns);
			}
		}

		TypePtr InspireDriver::resolveTypeAliases(const location& l, const TypePtr& type) {
			return scopes.resolve(type);
		}

		ExpressionPtr InspireDriver::genLambda(const location& l, const VariableList& params, const TypePtr& retType, const StatementPtr& body,
		                                        const FunctionKind& fk, bool isLambda) {
			// TODO: cast returns to appropriate type
			TypeList paramTys;
			for(const auto& var : params) {
				paramTys.push_back(var.getType());
			}

			// if it is a function that is defined
			if (!isLambda) {
				// => skip materialization of parameters
				return builder.lambdaExpr(retType, params, body);
			}

			// replace all variables in the body by their implicitly materialized version
			auto lambdaIngredients = transform::materialize({params, body});

			// build resulting function type
			auto funcType = genFuncType(l, paramTys, retType, fk);
			return builder.lambdaExpr(funcType.as<FunctionTypePtr>(), lambdaIngredients.params, lambdaIngredients.body);
		}

		ExpressionPtr InspireDriver::genClosure(const location& l, const VariableList& params, StatementPtr stmt) {
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

		ExpressionPtr InspireDriver::genCall(const location& l, const ExpressionPtr& callable, ExpressionList args) {
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
						ExpressionList packParams(args.begin() + funcParamTypes.size() - 1, args.end());
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
			} catch(const types::ReturnTypeDeductionException& e) {
				error(l, format("Error in call expression :\n%s", e.what()));
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

		ExpressionPtr InspireDriver::genStructExpression(const location& l, const TypePtr& type, const ExpressionList& list) {
			// check for null
			if (!type) {
				error(l, "Accessing null-type!");
				return nullptr;
			}

			// check for a struct type
			TagTypePtr structType = type.isa<TagTypePtr>();
			if(!structType || !structType->isStruct()) {
				error(l, format("Not a struct type: %s", toString(type)));
				return nullptr;
			}

			// check fields
			auto fields = structType->getFields();
			if(fields->size() != list.size()) {
				error(l, "init list does not match number of fields");
				return nullptr;
			}

			// extract name / value pairs
			NamedValueList values;
			for(const auto& cur : make_paired_range(fields, list)) {
				values.push_back(builder.namedValue(cur.first->getName(), cur.second.as<ExpressionPtr>()));
			}

			// build struct expression
			return builder.structExpr(structType, values);
		}

		
		ExpressionPtr InspireDriver::genUnionExpression(const location& l, const TypePtr& type, const std::string field, const ExpressionPtr& expr) {
			// check for null
			if (!type) {
				error(l, "Accessing null-type!");
				return nullptr;
			}

			// check for a union type
			TagTypePtr unionType = type.isa<TagTypePtr>();
			if(!unionType || !unionType->isUnion()) {
				error(l, format("Not a union type: %s", toString(type)));
				return nullptr;
			}

			// build union expression
			return builder.unionExpr(unionType, builder.stringValue(field), expr);
		}

		// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Address marking   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		ExpressionPtr InspireDriver::mark_address(const location& l, const ExpressionPtr& expr) {
			NodePtr res = builder.markerExpr(expr);
			res->attachValue<AddressMark>();
			return res.as<ExpressionPtr>();
		}

		StatementPtr InspireDriver::mark_address(const location& l, const StatementPtr& stmt) {
			NodePtr res = builder.markerStmt(stmt);
			res->attachValue<AddressMark>();
			return res.as<StatementPtr>();
		}

		// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Scope management  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		void InspireDriver::add_symb(const location& l, const std::string& name, const node_factory&  factory) {

			if(name.find(".") != std::string::npos) {
				error(l, format("symbol names can not contain dot chars: %s", name));
				return;
			}

			// ignore wildcard for unused variables
			if(name == "_") { return; }

			if(!scopes.add_symb(name, factory)) { error(l, format("symbol %s redefined", name)); }
		}

		void InspireDriver::add_symb(const location& l, const std::string& name, const ExpressionPtr& node) {
			add_symb(l, name, [=]() -> NodePtr { return node; });
		}


		void InspireDriver::add_symb(const std::string& name, const node_factory& factory) {
			add_symb(glob_loc, name, factory);
		}

		void InspireDriver::add_symb(const std::string& name, const ExpressionPtr& node) {
			add_symb(name, [=]() -> NodePtr { return node; });
		}

		void InspireDriver::add_type_alias(const GenericTypePtr& pattern, const TypePtr& substitute) {
			scopes.add_type_alias(pattern, substitute);
		}

		void InspireDriver::add_type(const location& l, const std::string& name, const node_factory&  factory) {

			if(name.find(".") != std::string::npos) {
				error(l, format("symbol names can not contain dot chars: %s", name));
				return;
			}

			// ignore wildcard for unused variables
			if(name == "_") { return; }

			// annotate type name
			auto node = factory();
			annotations::attachName(node, name);
			if(auto tagType = node.isa<TagTypePtr>()) {
				annotations::attachName(tagType->getRecord(), name);
			}

			if(!scopes.add_type(name, factory)) { error(l, format("type name %s redefined", name)); }
		}

		void InspireDriver::add_type(const location& l, const std::string& name, const TypePtr& node) {
			add_type(l, name, [=]() -> NodePtr { return node; });
		}


		void InspireDriver::add_type(const std::string& name, const node_factory& factory) {
			add_type(glob_loc, name, factory);
		}

		void InspireDriver::add_type(const std::string& name, const TypePtr& node) {
			add_type(name, [=]() -> NodePtr { return node; });
		}

		void InspireDriver::add_this(const location& l, const TypePtr& classType) {
			// gen ref type
			auto refThis = builder.refType(classType);
			// gen var
			auto thisVar = builder.variable(refThis);
			// save in scope
			add_symb(l, "this", thisVar);
		}


		void InspireDriver::open_scope(const location& l) {
			scopes.open_scope();
		}

		void InspireDriver::close_scope(const location& l) {
			scopes.close_scope();
		}


		void InspireDriver::import_extension(const location& l, const std::string& extension_name) {
			auto name = extension_name;
			name.replace(0, 1, "");
			name.replace(name.size() - 1, 1, "");

			// get extension factory from the registry
			auto optFactory = lang::ExtensionRegistry::getInstance().lookupExtensionFactory(name);
			if (optFactory) {
				import_extension((*optFactory)(mgr));
			} else {
				error(l, format("Unable to locate module: %s", name));
			}
		}

		void InspireDriver::import_extension(const lang::Extension& extension) {

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

		} // annon

		void InspireDriver::print_location(const location& l) const {
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

		void InspireDriver::error(const location& l, const std::string& m) const {
			errors.push_back(ParserError(l, m));
		}

		void InspireDriver::error(const std::string& m) const {
			errors.push_back(ParserError(glob_loc, m));
		}

		bool InspireDriver::where_errors() const {
			if(!errors.empty()) { print_errors(); }
			return !errors.empty();
		}


		void InspireDriver::print_errors(std::ostream& out, bool color) const {
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
