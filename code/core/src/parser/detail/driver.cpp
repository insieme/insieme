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

#include "insieme/core/parser/detail/driver.h"

#include <algorithm>
#include <string>

#include "insieme/core/ir.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/naming.h"

#include "insieme/core/types/match.h"
#include "insieme/core/types/return_type_deduction.h"

#include "insieme/core/parser/detail/scanner.h"

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
namespace parser {
	namespace detail {

		class location;
		class InspireParser;

		/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ scope manager ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


		void DeclarationContext::openScope() {
			stack.push_back(Scope());
		}

		void DeclarationContext::closeScope() {
			assert_gt(stack.size(), 1) << "Attempted to pop global scope!";
			stack.pop_back();
		}

		bool DeclarationContext::addSymb(const std::string& name, const NodeFactory& factory) {
			auto& symbols = stack.back().symbols;
			if (symbols.find(name) != symbols.end()) return false;
			symbols[name] = factory;
			return true;
		}

		bool DeclarationContext::addType(const std::string& name, const NodeFactory& factory) {
			auto& types = stack.back().types;
			if (types.find(name) != types.end()) return false;
			types[name] = factory;
			return true;
		}


		NodePtr DeclarationContext::findSymb(const std::string& name) const {
			// lookup symbols throughout scopes
			for(auto it = stack.rbegin(); it != stack.rend(); ++it) {
				const DefinitionMap& cur = it->symbols;
				auto pos = cur.find(name);
				if (pos != cur.end()) return pos->second();
			}
			// nothing found
			return nullptr;
		}

		NodePtr DeclarationContext::findType(const std::string& name) const {
			// lookup symbols throughout scopes
			for(auto it = stack.rbegin(); it != stack.rend(); ++it) {
				const DefinitionMap& cur = it->types;
				auto pos = cur.find(name);
				if (pos != cur.end()) return pos->second();
			}
			// nothing found
			return nullptr;
		}

		void DeclarationContext::addTypeAlias(const TypePtr& pattern, const TypePtr& substitute) {
			auto& aliases = stack.back().alias;
			assert_true(aliases.find(pattern) == aliases.end());
			aliases[pattern] = substitute;
		}

		TypePtr DeclarationContext::resolve(const TypePtr& type) const {
			if (!type) return type;

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
		    : scopes(), mgr(mgr), builder(mgr), file("global scope"), str(str), tu(mgr), result(nullptr), globLoc(&file), ss(str), scanner(&ss),
		      parser(*this, scanner) {
		}

		InspireDriver::~InspireDriver() {}

		ProgramPtr InspireDriver::parseProgram() {
			scanner.setStartProgram();
			int fail = parser.parse();
			if(whereErrors() || fail) {
				//printErrors();
				return nullptr;
			}
			return result.as<ProgramPtr>();
		}

		TypePtr InspireDriver::parseType() {
			scanner.setStartType();
			InspireParser parser(*this, scanner);
			int fail = parser.parse();
			if(whereErrors() || fail) {
				//printErrors();
				return nullptr;
			}
			return result.as<TypePtr>();
		}

		StatementPtr InspireDriver::parseStmt() {
			scanner.setStartStatement();
			InspireParser parser(*this, scanner);
			int fail = parser.parse();
			if(whereErrors() || fail) {
				//printErrors();
				return nullptr;
			}
			return result.as<StatementPtr>();
		}

		ExpressionPtr InspireDriver::parseExpression() {
			scanner.setStartExpression();
			InspireParser parser(*this, scanner);
			int fail = parser.parse();
			if(whereErrors() || fail) {
				//printErrors();
				return nullptr;
			}
			return result.as<ExpressionPtr>();
		}

		// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Some tools ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		ExpressionPtr InspireDriver::findSymbol(const location& l, const std::string& name) {
			//look in the already defined symbols
			auto x = scopes.findSymb(name);

			//look in lang basic
			if(!x) {
				try {
					x = builder.getLangBasic().getBuiltIn(name);
				} catch(...) {
					// pass, nothing to do really
				}
			}

			//look in the Reference extension
			if(!x) {
				auto symbols = mgr.getLangExtension<core::lang::ReferenceExtension>().getDefinedSymbols();
				auto pos = symbols.find(name);
				if (pos != symbols.end()) {
					x = pos->second();
				}
			}

			//fail if not found
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
			auto x = scopes.findType(name);
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

		NumericTypePtr InspireDriver::genNumericType(const location& l, const ExpressionPtr& variable) const {
			if(!variable.isa<VariablePtr>()) {
				error(l, "not a variable");
			}
			return builder.numericType(variable.as<core::VariablePtr>());
		}

		NumericTypePtr InspireDriver::genNumericType(const location& l, const string& value) const {
			return builder.numericType(builder.literal(value, builder.getLangBasic().getUIntInf()));
		}

		TypePtr InspireDriver::genFuncType(const location& l, const TypeList& params, const TypePtr& retType, const FunctionKind& fk) {
			return builder.functionType(params, retType, fk);
		}

		TagTypePtr InspireDriver::genRecordType(const location& l, const NodeType& type, const string& name, const ParentList& parents, const FieldList& fields, const LambdaExprList& ctors,
				const LambdaExprPtr& dtorIn, const MemberFunctionList& mfuns, const PureVirtualMemberFunctionList& pvmfuns) {

			TagTypePtr res;

			// check whether a default constructor is required
			LambdaExprPtr dtor = (dtorIn) ? dtorIn : builder.getDefaultDestructor(name);

			if (type == NT_Struct) {
				res = builder.structType(name,parents,fields,ctors,dtor,mfuns,pvmfuns);
			} else {
				if (!parents.empty()) error(l, "Inheritance not supported for unions!");
				res = builder.unionType(name,fields,ctors,dtor,mfuns,pvmfuns);
			}

			// register type in translation unit
			tu.addType(builder.genericType(name), res);

			// done
			return res;
		}

		TagTypePtr InspireDriver::genSimpleStructOrUnionType(const location& l, const NodeType& type, const FieldList& fields) {
			if (type == NT_Struct) {
				return builder.structType(fields);
			} else {
				return builder.unionType(fields);
			}
		}

		TypePtr InspireDriver::resolveTypeAliases(const location& l, const TypePtr& type) {
			return scopes.resolve(type);
		}

		LambdaExprPtr InspireDriver::genLambda(const location& l, const VariableList& params, const TypePtr& retType, const StatementPtr& body, bool isLambda) {
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
			auto funcType = genFuncType(l, paramTys, retType);
			return builder.lambdaExpr(funcType.as<FunctionTypePtr>(), lambdaIngredients.params, lambdaIngredients.body);
		}

		BindExprPtr InspireDriver::genClosure(const location& l, const VariableList& params, StatementPtr stmt) {
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

		void InspireDriver::registerFields(const location& l, const FieldList& fields) {
			assert_false(currentRecordStack.empty()) << "Not within record definition!";

			//iterate over all the fields
			for (const auto& field : fields) {
				const auto& name = field->getName()->getValue();
				const auto& type = field->getType();
				// create the member access call to store in the symbol table
				ExpressionPtr access = builder.getLangBasic().getCompositeMemberAccess();
				auto accessExpr = builder.callExpr(type, access, genThis(l), builder.getIdentifierLiteral(name), builder.getTypeLiteral(type));
				annotations::attachName(field, name);
				addSymb(l, name, accessExpr);
			}
		}

		/**
		 * generates a constructor for the currently defined record type
		 */
		LambdaExprPtr InspireDriver::genConstructor(const location& l, const VariableList& params, const StatementPtr& body) {
			assert_false(currentRecordStack.empty()) << "Not within record definition!";

			// get this-type
			auto thisParam = builder.variable(builder.refType(getThisType()));

			// create full parameter list
			VariableList ctorParams;
			ctorParams.push_back(thisParam);
			for(const auto& cur : params) ctorParams.push_back(cur);

			// update body
			auto newBody = transform::replaceAll(mgr, body, genThis(l), thisParam).as<StatementPtr>();

			// create constructor type
			auto ctorType = builder.functionType(extractTypes(ctorParams), thisParam->getType(), FK_CONSTRUCTOR);

			// replace all variables in the body by their implicitly materialized version
			auto ingredients = transform::materialize({ctorParams, newBody});

			// create the constructor
			return builder.lambdaExpr(ctorType, ingredients.params, ingredients.body);
		}

		/**
		 * generates a destructor for the currently defined record type
		 */
		LambdaExprPtr InspireDriver::genDestructor(const location& l, const StatementPtr& body) {
			assert_false(currentRecordStack.empty()) << "Not within record definition!";

			// get this-type
			auto thisParam = builder.variable(builder.refType(getThisType()));

			// create full parameter list
			VariableList params = { thisParam };

			// update body
			auto newBody = transform::replaceAll(mgr, body, genThis(l), thisParam).as<StatementPtr>();

			// create destructor type
			auto dtorType = builder.functionType(extractTypes(params), thisParam->getType(), FK_DESTRUCTOR);

			// replace all variables in the body by their implicitly materialized version
			auto ingredients = transform::materialize({params, newBody});

			// create the destructor
			return builder.lambdaExpr(dtorType, ingredients.params, ingredients.body);
		}

		/**
		 * generates a member function for the currently defined record type
		 */
		MemberFunctionPtr InspireDriver::genMemberFunction(const location& l, bool virtl, bool cnst, bool voltile, const std::string& name, const VariableList& params, const TypePtr& retType, const StatementPtr& body, bool isLambda) {
			//build the lambda
			auto lambda = genLambda(l, params, retType, body, isLambda);

			assert_false(lambda->isRecursive()) << "The parser should not produce recursive functions!";
			assert_false(currentRecordStack.empty()) << "Not within record definition!";

			// get this-type
			auto thisType = builder.refType(getThisType(), cnst, voltile);
			if (!isLambda) thisType = builder.refType(thisType);
			auto thisParam = builder.variable(thisType);

			// create full parameter list
			VariableList fullParams;
			fullParams.push_back(thisParam);
			for(const auto& cur : lambda.getParameterList()) fullParams.push_back(cur);

			// update body
			auto newBody = transform::replaceAll(mgr, lambda->getBody(), genThis(l), thisParam).as<StatementPtr>();

			// create member function type
			auto memberFunType = builder.functionType(extractTypes(fullParams), lambda->getFunctionType()->getReturnType(), FK_MEMBER_FUNCTION);

			// replace all variables in the body by their implicitly materialized version
			auto ingredients = (isLambda) ? transform::materialize({fullParams, newBody}) : (transform::LambdaIngredients){fullParams, newBody};

			// create the member function
			auto fun = builder.lambdaExpr(memberFunType, ingredients.params, ingredients.body);

			// create the member function entry
			return builder.memberFunction(virtl, name, lambda);
		}

		PureVirtualMemberFunctionPtr InspireDriver::genPureVirtualMemberFunction(const location& l,  bool cnst, bool voltile, const std::string& name, const FunctionTypePtr& type) {
			assert_true(type->isPlain()) << "Only plain function types should be covered here!";
			assert_false(currentRecordStack.empty()) << "Not within record definition!";

			// get this-type
			auto thisType = builder.refType(getThisType(), cnst, voltile);

			// create full parameter type list
			TypeList fullParams;
			fullParams.push_back(thisType);
			for(const auto& cur : type->getParameterTypes()) fullParams.push_back(cur);

			// create member function type type
			auto memberFunType = builder.functionType(fullParams, type->getReturnType(), FK_MEMBER_FUNCTION);

			// create the member function entry
			return builder.pureVirtualMemberFunction(name, type);
		}

		TypePtr InspireDriver::findOrGenAbstractType(const location& l, const std::string& name, const ParentList& parents, const TypeList& typeList) {
			auto foundType = findType(l, name);
			if (foundType) {
				return foundType;
			}

			return builder.genericType(name, parents, typeList);
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

		ExpressionPtr InspireDriver::getInitiaizerExpr(const location& l, const TypePtr& type, const ExpressionList& list) {
			// check for a struct type
			TagTypePtr tagType = type.isa<TagTypePtr>();
			if (tagType && tagType->isStruct()) {
				return genStructExpression(l, type, list);

				//check for union type, which has to be initialized differently
			} else if (tagType && tagType->isUnion()) {
				//there must only be one expression in the initializer
				if (list.size() != 1) {
					error(l, "A union initialization expression must only contain one expression");
					return nullptr;
				}

				//we have to find the member of the union which has the same type as the given expression
				ExpressionPtr init = list[0];
				auto unionType = tagType->getUnion();
				for (auto cur : unionType->getFields()) {
					if (cur->getType() == init.getType()) {
						return genUnionExpression(l, type, cur->getName()->getValue(), init);
					}
				}

				//if we end up here we didn't find a matching field
				error(l, "The given expression does not match any of the union's field types");
				return nullptr;
			}

			assert_not_implemented() << "Unimplemented functionality. Can't initialize type " << type;
			return nullptr;
		}

		VariablePtr InspireDriver::genParameter(const location& l, const std::string& name, const TypePtr& type) {
			auto resolvedType = resolveTypeAliases(l, type);
			VariablePtr variable = builder.variable(resolvedType);
			annotations::attachName(variable, name);
			addSymb(l, name, variable);
			return variable;
		}

		ExpressionPtr InspireDriver::genJobExpr(const location& l, const ExpressionPtr& lowerBound, const ExpressionPtr& upperBound, const ExpressionPtr& expr) {
			auto scalarExpr = getScalar(expr);
			if (!scalarExpr.isa<CallExprPtr>()) {
				error(l, "expression in job must be a call expression");
				return nullptr;
			}
			auto bind = builder.bindExpr(VariableList(), scalarExpr.as<CallExprPtr>());
			return builder.jobExpr(getScalar(lowerBound), getScalar(upperBound), bind);
		}

		ExpressionPtr InspireDriver::genJobExpr(const location& l, const ExpressionPtr& expr) {
			auto scalarExpr = getScalar(expr);
			if (!scalarExpr.isa<CallExprPtr>()) {
				error(l, "expression in job must be a call expression");
				return nullptr;
			}
			auto bind = builder.bindExpr(VariableList(), scalarExpr.as<CallExprPtr>());
			return builder.jobExpr(builder.getThreadNumRange(1), bind.as<ExpressionPtr>());
		}

		ExpressionPtr InspireDriver::genSync(const location& l, const ExpressionPtr& expr) {
			return builder.callExpr(builder.getLangBasic().getUnit(), builder.getExtension<lang::ParallelExtension>().getMerge(), getScalar(expr));
		}

		ExpressionPtr InspireDriver::genSyncAll(const location& l) {
			return builder.callExpr(builder.getLangBasic().getUnit(), builder.getExtension<lang::ParallelExtension>().getMergeAll());
		}

		ExpressionPtr InspireDriver::genDerefExpr(const location& l, const ExpressionPtr& expr) {
			auto scalarExpr = getScalar(expr);
			if (!analysis::isRefType(scalarExpr->getType())) {
				error(l, format("cannot deref non ref type: %s", toString(scalarExpr->getType())));
				return nullptr;
			}

			return builder.deref(scalarExpr);
		}

		ExpressionPtr InspireDriver::genAsExpr(const location& l, const ExpressionPtr& expr, const TypePtr& type) {
			auto scalarExpr = getScalar(expr);
			if (!scalarExpr->getType()) {
				error(l, "can not get parent-reference of non referenced expression");
				return nullptr;
			}

			return builder.refParent(scalarExpr, type);
		}

		DeclarationStmtPtr InspireDriver::genVariableDeclaration(const location& l, const TypePtr& type, const std::string name, const ExpressionPtr& init) {
			auto resolvedType = resolveTypeAliases(l, type);
			auto variable = builder.variable(resolvedType);
			annotations::attachName(variable, name);
			addSymb(l, name, variable);
			return builder.declarationStmt(variable, getScalar(init));
		}

		ForStmtPtr InspireDriver::genForStmt(const location& l, const TypePtr& iteratorType, const std::string iteratorName,
		                                     const ExpressionPtr& lowerBound, const ExpressionPtr& upperBound, const ExpressionPtr& stepExpr, const StatementPtr& body) {
			VariablePtr iteratorVariable = findSymbol(l, iteratorName).isa<VariablePtr>();
			assert_true(iteratorVariable) << "Variable doesn't exist or isn't a VariablePtr";
			return builder.forStmt(iteratorVariable, getScalar(lowerBound), getScalar(upperBound), getScalar(stepExpr), body);
		}

		ExpressionPtr InspireDriver::genThis(const location& l) {
			// check valid scope
			if (currentRecordStack.empty()) {
				error(l, "This-pointer in non-record context!");
				return nullptr;
			}

			// build a literal
			return builder.literal("this", builder.refType(getThisType()));
		}


		// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Address marking   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		ExpressionPtr InspireDriver::markAddress(const location& l, const ExpressionPtr& expr) {
			NodePtr res = builder.markerExpr(expr);
			res->attachValue<AddressMark>();
			return res.as<ExpressionPtr>();
		}

		StatementPtr InspireDriver::markAddress(const location& l, const StatementPtr& stmt) {
			NodePtr res = builder.markerStmt(stmt);
			res->attachValue<AddressMark>();
			return res.as<StatementPtr>();
		}

		// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Scope management  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		void InspireDriver::addSymb(const location& l, const std::string& name, const NodeFactory&  factory) {

			if(name.find(".") != std::string::npos) {
				error(l, format("symbol names can not contain dot chars: %s", name));
				return;
			}

			// ignore wildcard for unused variables
			if(name == "_") { return; }

			if(!scopes.addSymb(name, factory)) { error(l, format("symbol %s redefined", name)); }
		}

		void InspireDriver::addSymb(const location& l, const std::string& name, const ExpressionPtr& node) {
			addSymb(l, name, [=]() -> NodePtr { return node; });
		}


		void InspireDriver::addSymb(const std::string& name, const NodeFactory& factory) {
			addSymb(globLoc, name, factory);
		}

		void InspireDriver::addSymb(const std::string& name, const ExpressionPtr& node) {
			addSymb(name, [=]() -> NodePtr { return node; });
		}

		void InspireDriver::addTypeAlias(const TypePtr& pattern, const TypePtr& substitute) {
			scopes.addTypeAlias(pattern, substitute);
		}

		void InspireDriver::addType(const location& l, const std::string& name, const NodeFactory&  factory) {

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

			if(!scopes.addType(name, factory)) { error(l, format("type name %s redefined", name)); }
		}

		void InspireDriver::addType(const location& l, const std::string& name, const TypePtr& node) {
			addType(l, name, [=]() -> NodePtr { return node; });
		}


		void InspireDriver::addType(const std::string& name, const NodeFactory& factory) {
			addType(globLoc, name, factory);
		}

		void InspireDriver::addType(const std::string& name, const TypePtr& node) {
			addType(name, [=]() -> NodePtr { return node; });
		}

		void InspireDriver::addThis(const location& l, const TypePtr& classType) {
			// gen ref type
			auto refThis = builder.refType(classType);
			// gen var
			auto thisVar = builder.variable(refThis);
			// save in scope
			addSymb(l, "this", thisVar);
		}


		void InspireDriver::openScope() {
			scopes.openScope();
		}

		void InspireDriver::closeScope() {
			scopes.closeScope();
		}

		void InspireDriver::beginRecord(const std::string& name) {
			currentRecordStack.push_back(builder.tagTypeReference(name));
			openScope();
		}

		void InspireDriver::endRecord() {
			assert_false(currentRecordStack.empty());
			closeScope();
			currentRecordStack.pop_back();
		}

		TagTypeReferencePtr InspireDriver::getThisType() {
			assert_false(currentRecordStack.empty());
			return currentRecordStack.back();
		}

		void InspireDriver::importExtension(const location& l, const std::string& extensionName) {
			auto name = extensionName;
			name.replace(0, 1, "");
			name.replace(name.size() - 1, 1, "");

			// get extension factory from the registry
			auto optFactory = lang::ExtensionRegistry::getInstance().lookupExtensionFactory(name);
			if (optFactory) {
				importExtension((*optFactory)(mgr));
			} else {
				error(l, format("Unable to locate module: %s", name));
			}
		}

		void InspireDriver::importExtension(const lang::Extension& extension) {

			// import symbols
			for(const auto& cur : extension.getSymbols()) {
				addSymb(cur.first, cur.second);
			}

			// import type aliases
			for(const auto& cur : extension.getTypeAliases()) {
				addTypeAlias(cur.first, cur.second);
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
			std::vector<std::string> splitString(const std::string& s) {
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

		void InspireDriver::printLocation(const location& l) const {
			auto buffer = splitString(str);
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
			errors.push_back(ParserError(globLoc, m));
		}

		bool InspireDriver::whereErrors() const {
			if(!errors.empty()) { printErrors(); }
			return !errors.empty();
		}


		void InspireDriver::printErrors(std::ostream& out, bool color) const {
			auto buffer = splitString(str);
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
} // namespace parser
} // namespace core
} // namespace insieme
