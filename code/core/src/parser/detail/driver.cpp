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
#include "insieme/core/parser/detail/driver.h"

#include <algorithm>
#include <string>

#include <boost/algorithm/string.hpp>

#include "insieme/core/ir.h"

#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/transform/materialize.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/annotations/naming.h"

#include "insieme/core/types/match.h"
#include "insieme/core/types/return_type_deduction.h"
#include "insieme/core/types/type_variable_deduction.h"

#include "insieme/core/parser/detail/scanner.h"

#include "insieme/core/lang/array.h"
#include "insieme/core/lang/reference.h"
#include "insieme/core/lang/parallel.h"
#include "insieme/core/lang/pointer.h"
#include "insieme/core/lang/varargs_extension.h"

#include "insieme/core/lang/extension.h"
#include "insieme/core/lang/extension_registry.h"

#include "insieme/utils/name_mangling.h"
#include "insieme/utils/functional_utils.h"

// this last one is generated and the path will be provided to the command
#include "inspire_parser.hpp"


namespace insieme {
namespace core {
namespace parser {

	namespace detail {

		class location;
		class InspireParser;

		/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ InspireDriver ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

		InspireDriver::InspireDriver(const std::string& str, NodeManager& mgr)
			: scopes(), mgr(mgr), builder(mgr), file("global scope"), str(str), tu(mgr), result(nullptr), globLoc(&file), ss(str), scanner(&ss),
			  parser(*this, scanner), printedErrors(false), parserIRExtension(mgr.getLangExtension<ParserIRExtension>()) {
			// begin the global scope
			openScope();
		}

		InspireDriver::~InspireDriver() {}

		ProgramPtr InspireDriver::parseProgram() {
			scanner.setStartProgram();
			int fail = parser.parse();
			if(wereErrors() || fail) {
				result = nullptr;
				return nullptr;
			}
			return result.as<ProgramPtr>();
		}

		TypePtr InspireDriver::parseType() {
			scanner.setStartType();
			InspireParser parser(*this, scanner);
			int fail = parser.parse();
			if(wereErrors() || fail) {
				result = nullptr;
				return nullptr;
			}
			return result.as<TypePtr>();
		}

		StatementPtr InspireDriver::parseStmt() {
			scanner.setStartStatement();
			InspireParser parser(*this, scanner);
			int fail = parser.parse();
			if(wereErrors() || fail) {
				result = nullptr;
				return nullptr;
			}
			return result.as<StatementPtr>();
		}

		ExpressionPtr InspireDriver::parseExpression() {
			scanner.setStartExpression();
			InspireParser parser(*this, scanner);
			int fail = parser.parse();
			if(wereErrors() || fail) {
				result = nullptr;
				return nullptr;
			}
			return result.as<ExpressionPtr>();
		}

		// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Some tools ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

		ExpressionPtr InspireDriver::getScalar(ExpressionPtr expr) {
			// auto-unwrap tuple
			if(auto tuple = expr.isa<TupleExprPtr>()) {
				const auto& elements = tuple->getExpressions();
				if(elements.size() == 1) { return getScalar(elements[0]); }
			}
			// otherwise stay as it is
			return expr;
		}

		ExpressionPtr InspireDriver::getOperand(ExpressionPtr expr) { return builder.tryDeref(getScalar(expr)); }

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
					return nullptr;
				} else if(analysis::getReferencedType(left.getType()) != right->getType()) {
					error(l, format("right side expression of type %s can not be assigned to type %s", toString(right.getType()), toString(left.getType())));
					return nullptr;
				}

				return builder.assign(left, right);
			}

			auto b = getOperand(right);
			// left side is untouched because of reference subscript operators
			if(op == "[") {
				left = getScalar(left);
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

			// shift operations
			if(op == "<<") { return builder.leftShift(a, b); }
			if(op == ">>") { return builder.rightShift(a, b); }

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
				if(analysis::isRefType(type)) return getFieldType(analysis::getReferencedType(type), name);

				// check for a tag type
				auto tagType = type.isa<TagTypePtr>();
				if(!tagType) return TypePtr();

				// unroll recursive types
				if(tagType->isRecursive()) return getFieldType(tagType->peel(), name);

				// get field type
				return tagType->getFieldType(name);
			}
		}


		ExpressionPtr InspireDriver::genMemberAccess(const location& l, const ExpressionPtr& exprIn, const std::string& memberName) {

			//get rid of single element tuples
			const ExpressionPtr& expr = getScalar(exprIn);

			if(!expr) {
				error(l, "no expression");
				return nullptr;
			}

			const auto& fieldAccess = builder.getLangBasic().getCompositeMemberAccess();
			const auto& refAccess = mgr.getLangExtension<core::lang::ReferenceExtension>().getRefMemberAccess();
			const auto& memberFunctionAccess = parserIRExtension.getMemberFunctionAccess();

			// the type of the expression
			TypePtr exprType = expr->getType();
			// the actual type - maybe stripped of one ref
			TypePtr objectType = exprType;
			if(analysis::isRefType(exprType)) { objectType = analysis::getReferencedType(exprType); }

			// if the type is a generic type, we can proceed with the literal lookup here
			if(auto genericObjectType = objectType.isa<GenericTypePtr>()) {
				// create a key to look up the symbol in the global scope
				std::string keyName = genericObjectType->getName()->getValue() + "::" + memberName;

				// look for the key in the global scope
				auto lookupResult = lookupDeclaredInGlobalScope(keyName);
				if(!lookupResult) {
					//if we didn't find the symbol in the global scope, we search the TU for it
					for(const auto& mapEntry : tu.getFunctions()) {
						const auto& key = mapEntry.first;
						if(key.getValue()->getValue() == keyName) {
							lookupResult = key;
							break;
						}
					}

					if (!lookupResult) {
						error(l, format("Unable to lookup member %s in type %s", memberName, *genericObjectType));
						return nullptr;
					}
				}

				if(!lookupResult.isa<LiteralPtr>()) {
					error(l, format("Member %s in type %s is not a literal", memberName, *genericObjectType));
					return nullptr;
				}

				// now that we found the given member, create a member access expression to return
				TypePtr memberType = lookupResult.as<LiteralPtr>()->getType();

				// if the member is a member function
				if(auto functionType = memberType.isa<FunctionTypePtr>()) {
					if(functionType->getKind() == FK_MEMBER_FUNCTION) {
						return builder.callExpr(memberFunctionAccess, expr, builder.getIdentifierLiteral(memberName));
					}
				}

				// otherwise it is a field
				if(analysis::isRefType(exprType)) {
					return builder.callExpr(refAccess, expr, builder.getIdentifierLiteral(memberName), builder.getTypeLiteral(memberType));
				}
				return builder.callExpr(fieldAccess, expr, builder.getIdentifierLiteral(memberName), builder.getTypeLiteral(memberType));

				// otherwise we have to create the member lookup differently
			} else {
				// check whether there is such a field
				if(auto fieldType = getFieldType(exprType, memberName)) {
					// create access
					if(analysis::isRefType(exprType)) {
						return builder.callExpr(refAccess, expr, builder.getIdentifierLiteral(memberName), builder.getTypeLiteral(fieldType));
					}
					return builder.callExpr(fieldAccess, expr, builder.getIdentifierLiteral(memberName), builder.getTypeLiteral(fieldType));
				}
			}

			error(l, format("Unable to locate member %s in type %s", memberName, *exprType));
			return nullptr;
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
				if(params.size() != 1) {
					error(l, "wrong int size");
					return nullptr;
				}
			}
			if(name == "real") {
				if(params.size() != 1) {
					error(l, "wrong real size");
					return nullptr;
				}
			}
			for(const auto& p : params) {
				if(!p) {
					error(l, "wrong parameter in paramenter list");
					return nullptr;
				}
			}

			return builder.genericType(name, parents, params);
		}

		NumericTypePtr InspireDriver::genNumericType(const location& l, const ExpressionPtr& variable) const {
			if(!variable.isa<VariablePtr>()) {
				error(l, "not a variable");
				return nullptr;
			}
			return builder.numericType(variable.as<core::VariablePtr>());
		}

		NumericTypePtr InspireDriver::genNumericType(const location& l, const string& value) const {
			return builder.numericType(builder.literal(value, builder.getLangBasic().getIntInf()));
		}

		NumericTypePtr InspireDriver::genNegativeNumericType(const location& l, const string& value) const {
			return builder.numericType(builder.literal(std::string("-") + value, builder.getLangBasic().getIntInf()));
		}

		NumericTypePtr InspireDriver::genUnsignedNumericType(const location& l, const string& value) const {
			return builder.numericType(builder.literal(value.substr(0, value.find_first_not_of("0123456789")), builder.getLangBasic().getUIntInf()));
		}

		TypePtr InspireDriver::genFuncType(const location& l, const TypeList& params, const TypePtr& retType, const FunctionKind& fk) {
			return builder.functionType(params, retType, fk);
		}

		TypePtr InspireDriver::genRecordType(const location& l, const NodeType& type, const string& name, const ParentList& parents, const FieldList& fields,
			                                 const ExpressionList& ctors, const ExpressionPtr& dtor, const bool dtorIsVirtual,
			                                 const MemberFunctionList& mfuns, const PureVirtualMemberFunctionList& pvmfuns) {
			// check if this type has already been defined before
			const GenericTypePtr key = builder.genericType(name);
			if(tu[key]) {
				error(l, format("Type %s has already been defined", name));
				return nullptr;
			}

			TagTypePtr res;

			// now we insert new member functions with just literals as implementation for all functions which have only been declared by the user
			MemberFunctionList mfunsNew = mfuns;
			std::vector<LiteralPtr> literalsToRemove;
			const std::string classPrefix = name + "::";
			// iterate over all functions registered in the TU
			for (auto& function : tu.getFunctions()) {
				auto& literal = function.first;
				auto& literalName = literal->getStringValue();
				auto& lambda = function.second;
				// only consider member functions
				if (lambda->getFunctionType().isMemberFunction()) {
					auto body = lambda->getBody();
					// only functions which have been explicitely defined by the user will be inserted as literals
					if (body.size() > 0 && body[0] == parserIRExtension.getExplicitMemberDummyLambda()
							&& boost::starts_with(literalName, classPrefix)) {
						//register this literal for removal from the TU
						literalsToRemove.push_back(literal);
						//and add a new member function with the literal implementation
						mfunsNew.push_back(builder.memberFunction(false, literalName.substr(classPrefix.length()), literal));
					}
				}
			}
			//remove the literals we are supposed to remove
			for (auto& lit : literalsToRemove) {
				tu.getFunctions().erase(lit);
			}

			if(type == NT_Struct) {
				res = builder.structTypeWithDefaults(builder.refType(getThisType()), parents, fields, ctors, dtor, dtorIsVirtual, mfunsNew, pvmfuns);
			} else {
				if(!parents.empty()) {
					error(l, "Inheritance not supported for unions!");
					return nullptr;
				}
				res = builder.unionTypeWithDefaults(builder.refType(getThisType()), fields, ctors, dtor, dtorIsVirtual, mfunsNew, pvmfuns);
			}

			// register type in translation unit
			tu.insertRecordTypeWithDefaults(key, res);

			// done
			return key;
		}

		TypePtr InspireDriver::genSimpleStructOrUnionType(const location& l, const NodeType& type, const FieldList& fields) {
			// create a unique dummy name for this anonymous record.
			// this is needed in order to put this record into the TU also.
			// the name will be set to "" before returning the final parsed IR.
			auto name = builder.stringValue(format("__insieme_anonymous_record_%d_%d", l.begin.line, l.begin.column));
			temporaryAnonymousNames.push_back(name);

			// now we begin a new record with that name
			beginRecord(l, name->getValue());
			// and register the fields here
			registerFields(l, fields);

			const GenericTypePtr key = builder.genericType(name->getValue());
			TagTypePtr res;
			if(type == NT_Struct) {
				res = builder.structTypeWithDefaults(builder.refType(key), ParentList(), fields, ExpressionList(), ExpressionPtr(), false, MemberFunctionList(),
					                                 PureVirtualMemberFunctionList());
			} else {
				res = builder.unionTypeWithDefaults(builder.refType(key), fields, ExpressionList(), ExpressionPtr(), false, MemberFunctionList(),
					                                PureVirtualMemberFunctionList());
			}
			tu.insertRecordTypeWithDefaults(key, res);

			// end the record here
			endRecord();

			return key;
		}

		GenericTypePtr InspireDriver::getThisTypeForLambdaAndFunction(const bool cnst, const bool voltile) {
			return inLambda ? builder.refType(getThisType(), cnst, voltile) : builder.refType(builder.refType(getThisType(), cnst, voltile));
		}

		TypeList InspireDriver::getParamTypesForLambdaAndFunction(const location& l, const VariableList& params) {
			TypeList paramTypes;
			for(const auto& var : params) {
				const auto& varType = var->getType();
				// if we are building a lambda, the function type is already the correct one and the body will be materialized
				if(inLambda) {
					paramTypes.push_back(varType);

					// if we are building a function, we have to calculate the function type differently and leave the body untouched
				} else {
					if(!analysis::isRefType(varType)) {
						error(l, format("Parameter %s is not of ref type", var));
						return TypeList();
					}
					//CPP refs and rrefs get used as is (see transform::materialize)
					if(lang::isCppReference(varType) || lang::isCppRValueReference(varType)) {
						paramTypes.push_back(varType);

						//plain refs get unwrapped
					} else {
						paramTypes.push_back(analysis::getReferencedType(varType));
					}
				}
			}
			return paramTypes;
		}

		LambdaExprPtr InspireDriver::genLambda(const location& l, const VariableList& params, const TypePtr& retType, const StatementPtr& body,
			                                   const FunctionKind functionKind) {

			auto paramTypes = getParamTypesForLambdaAndFunction(l, params);
			if(paramTypes.size() != params.size()) { return nullptr; }

			// build resulting function type
			auto funcType = builder.functionType(paramTypes, retType, functionKind);

			// replace return statement types
			auto retBody = core::transform::transformBottomUpGen(body, [&retType, this](const core::ReturnStmtPtr& ret) {
				auto retT = ret->getReturnType();
				auto replacementT = core::transform::materialize(retType);
				return builder.returnStmt(ret->getReturnExpr(), replacementT);
			});

			// if it is a function with explicitly auto-created parameters no materialization of the parameters is required
			if(!inLambda) {
				// => skip materialization of parameters
				return builder.lambdaExpr(funcType, params, retBody);
			}

			// replace all variables in the body by their implicitly materialized version
			auto lambdaIngredients = transform::materialize({params, retBody});

			return builder.lambdaExpr(funcType, lambdaIngredients.params, lambdaIngredients.body);
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
			} else if(analysis::isOutlineAble(stmt, true)) {
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

			const std::string& recordName = getThisType()->getName()->getValue();

			// iterate over all the fields
			for(const auto& field : fields) {
				registerField(l, recordName, field->getName()->getValue(), field->getType());
			}
		}

		void InspireDriver::registerField(const location l, const std::string& recordName, const std::string& fieldName, const TypePtr& fieldType) {
			const std::string memberName = recordName + "::" + fieldName;

			// create literal to store in the lookup table
			const auto key = builder.literal(memberName, /*builder.refType(*/fieldType/*)*/);

			// only declare the symbol implicitly if it hasn't already been declared
			if(!isSymbolDeclaredInGlobalScope(memberName)) { declareSymbolInGlobalScope(l, memberName, key); }
		}

		/**
		 * generates a constructor for the currently defined record type
		 */
		ExpressionPtr InspireDriver::genConstructor(const location& l, const LambdaExprPtr& ctor) {
			assert_false(currentRecordStack.empty()) << "Not within record definition!";

			// return a special literal if the body indicates to default this function
			if(ctor->getBody() == getParserDefaultCompound()) return parserIRExtension.getDefaultedBodyMarker();

			auto key = builder.getLiteralForConstructor(ctor->getType());

			//register the lambda itself in the TU - but only overwrite dummy declarations
			if (const auto& otherMember = tu.getFunctions()[key]) {
				const auto& otherBody = otherMember->getBody();
				if (otherBody->size() != 1
						|| (otherBody[0] != parserIRExtension.getMemberDummyLambda()
								&& otherBody[0] != parserIRExtension.getExplicitMemberDummyLambda())) {
					error(l, format("Re-definition of constructor of type %s", *key->getType()));
					return nullptr;
				}
			}
			tu.addFunction(key, ctor);

			return key;
		}

		/**
		 * generates a constructor for the currently defined record type
		 */
		LambdaExprPtr InspireDriver::genConstructorLambda(const location& l, const VariableList& params, const StatementPtr& body) {
			assert_false(currentRecordStack.empty()) << "Not within record definition!";

			// create full parameter list
			VariableList ctorParams;
			ctorParams.push_back(thisStack.back());
			for(const auto& cur : params) {
				ctorParams.push_back(cur);
			}

			auto paramTypes = getParamTypesForLambdaAndFunction(l, ctorParams);
			if(paramTypes.size() != params.size() + 1) { return nullptr; }

			// create constructor type
			auto ctorType = builder.functionType(paramTypes, builder.refType(getThisType()), FK_CONSTRUCTOR);

			// create the constructor
			transform::LambdaIngredients ingredients{ctorParams, body};
			if(inLambda) {
				// replace all variables in the body by their implicitly materialized version
				ingredients = transform::materialize(ingredients);
			}
			return builder.lambdaExpr(ctorType, ingredients.params, ingredients.body);
		}

		/**
		 * generates a destructor for the currently defined record type
		 */
		ExpressionPtr InspireDriver::genDestructor(const location& l, const StatementPtr& body) {
			assert_false(currentRecordStack.empty()) << "Not within record definition!";

			if(body == getParserDefaultCompound()) return nullptr;

			// create full parameter list
			VariableList params = {thisStack.back()};
			auto paramTypes = getParamTypesForLambdaAndFunction(l, params);
			if(paramTypes.size() != 1) { return nullptr; }

			// create destructor type
			auto dtorType = builder.functionType(paramTypes, builder.refType(getThisType()), FK_DESTRUCTOR);

			// create the destructor
			transform::LambdaIngredients ingredients{params, body};
			if(inLambda) {
				// replace all variables in the body by their implicitly materialized version
				ingredients = transform::materialize(ingredients);
			}
			auto fun = builder.lambdaExpr(dtorType, ingredients.params, ingredients.body);

			auto key = builder.getLiteralForDestructor(dtorType);
			tu.addFunction(key, fun);

			return key;
		}

		/**
		 * generates a member function for the currently defined record type
		 */
		MemberFunctionPtr InspireDriver::genMemberFunction(const location& l, bool virtl, bool cnst, bool voltile, const std::string& name,
			                                               const VariableList& params, const TypePtr& retType, const StatementPtr& body) {
			assert_false(currentRecordStack.empty()) << "Not within record definition!";

			// return a member function containing a special literal if the body indicates to default this function
			if(body == getParserDefaultCompound()) return builder.memberFunction(false, name, parserIRExtension.getDefaultedBodyMarker());

			// create full parameter list
			VariableList fullParams { thisStack.back() };
			for(const auto& cur : params) {
				fullParams.push_back(cur);
			}

			// create the member function
			auto fun = genLambda(l, fullParams, retType, body, FK_MEMBER_FUNCTION);
			//ensure that the lambda got created without errors
			if(!fun) {
				return nullptr;
			}

			auto memberFunType = fun->getFunctionType();
			assert_false(fun->isRecursive()) << "The parser should not produce recursive functions!";

			//the key used to register this member function in the TU
			auto key = builder.getLiteralForMemberFunction(fun->getFunctionType(), name);

			// rename lambda ref to the correct name for a member function
			std::string lambdaName = key->getStringValue();
			fun = builder.lambdaExpr(fun->getLambda(), lambdaName);
			annotations::attachName(fun, lambdaName);

			//register the lambda itself in the TU - but only overwrite dummy declarations
			if (const auto& otherMember = tu.getFunctions()[key]) {
				const auto& otherBody = otherMember->getBody();
				if (otherBody->size() != 1
						|| (otherBody[0] != parserIRExtension.getMemberDummyLambda()
								&& otherBody[0] != parserIRExtension.getExplicitMemberDummyLambda())) {
					error(l, format("Re-definition of member function %s of type %s", name, *key->getType()));
					return nullptr;
				}
			}
			tu.addFunction(key, fun);

			return builder.memberFunction(virtl, name, key);
		}

		PureVirtualMemberFunctionPtr InspireDriver::genPureVirtualMemberFunction(const location& l, bool cnst, bool voltile, const std::string& name,
			                                                                     const FunctionTypePtr& type) {
			assert_true(type->isPlain()) << "Only plain function types should be covered here!";
			assert_false(currentRecordStack.empty()) << "Not within record definition!";

			// get this-type
			auto thisType = builder.refType(getThisType(), cnst, voltile);

			// create full parameter type list
			TypeList fullParams;
			fullParams.push_back(thisType);
			for(const auto& cur : type->getParameterTypes()) {
				fullParams.push_back(cur);
			}

			// create member function type type
			auto memberFunType = builder.functionType(fullParams, type->getReturnType(), FK_MEMBER_FUNCTION);

			// create the member function entry
			return builder.pureVirtualMemberFunction(name, memberFunType);
		}

		CompoundStmtPtr InspireDriver::getParserDefaultCompound() const {
			return parserIRExtension.getDefaultedBodyCompound();
		}

		CompoundStmtPtr InspireDriver::getParserDeleteCompound() const {
			return parserIRExtension.getDeletedBodyCompound();
		}

		bool InspireDriver::isMarkedAsDefaultedMember(const NodePtr& node) const {
			if(!node) return false;

			// member functions are checked for their implementation
			if(const auto& mFun = node.isa<MemberFunctionPtr>()) {
				return isMarkedAsDefaultedMember(mFun->getImplementation());
			}
			// literals can be compared directly
			if(const auto& lit = node.isa<LiteralPtr>()) {
				return lit == parserIRExtension.getDefaultedBodyMarker();
			}
			// everything else isn't a defaulted member
			return false;
		}

		ExpressionPtr InspireDriver::genFreeConstructor(const location& l, const std::string& name, const LambdaExprPtr& ctor) {
			assert_false(currentRecordStack.empty()) << "Not within record definition!";

			//the given ctor is already complete. all we have to do is register it in the TU and the global scope with the desired name
			const auto key = builder.literal(name, ctor->getType());
			if(tu[key] || isSymbolDeclaredInGlobalScope(name)) {
				error(l, format("Re-definition of function %s", name));
				return nullptr;
			}

			// rename lambda ref to given name
			auto renamedCtor = builder.lambdaExpr(ctor->getLambda(), name);
			annotations::attachName(renamedCtor, name);

			declareSymbolInGlobalScope(l, name, key);
			tu.addFunction(key, renamedCtor);

			return key;
		}

		ExpressionPtr InspireDriver::genFunctionDefinition(const location& l, const std::string name, const LambdaExprPtr& lambda) {
			// check if this type has already been defined before
			const LiteralPtr key = builder.literal(name, lambda->getType());
			if(tu[key]) {
				error(l, format("Re-definition of function %s", name));
				return nullptr;
			}

			// only declare the symbol implicitly if it hasn't already been declared
			if(!isSymbolDeclaredInCurrentScope(name)) { declareSymbol(l, name, key); }

			// rename lambda ref to given name
			auto renamedLambda = builder.lambdaExpr(lambda->getLambda(), name);
			tu.addFunction(key, renamedLambda);
			annotations::attachName(renamedLambda, name);

			return key;
		}

		TypePtr InspireDriver::findOrGenAbstractType(const location& l, const std::string& name, const ParentList& parents, const TypeList& typeList) {
			auto foundType = findType(l, name);
			if(!foundType) { foundType = lookupDeclared(name).isa<TypePtr>(); }
			if(foundType) { return foundType; }

			return builder.genericType(name, parents, typeList);
		}

		ParserTypedExpression InspireDriver::genTypedExpression(const location& l, const ExpressionPtr& expression, const TypePtr& type) {
			if (!types::getTypeVariableInstantiation(mgr, type, expression->getType())) {
				error(l, format("The given explicit expression type %s does not match the expression of type %s", *type, *expression->getType()));
				return {};
			}
			return {expression, type};
		}

		ExpressionPtr InspireDriver::genCall(const location& l, const ExpressionPtr& callable, ParserTypedExpressionList typedArgs) {
			ExpressionList args;
			TypeList argumentTypes;

			for (const auto& cur : typedArgs) {
				args.push_back(cur.expression);
				argumentTypes.push_back(cur.type);
			}

			ExpressionPtr func = getScalar(callable);

			// if this is a member function call we prepend the implicit this parameter
			if(analysis::isCallOf(func, parserIRExtension.getMemberFunctionAccess())) {
				// we add the callable itself as the first argument of the call
				auto thisParam = func.as<CallExprPtr>()->getArgument(0);
				auto thisParamType = thisParam->getType();
				if(analysis::isRefType(thisParamType)) { thisParamType = analysis::getReferencedType(thisParamType); }

				args.insert(args.begin(), thisParam);
				argumentTypes.insert(argumentTypes.begin(), thisParam->getType());

				// replace func with the lookup of the literal. first create the name of the literal
				std::string typeName = thisParamType.as<GenericTypePtr>()->getName()->getValue();
				std::string functionName = func.as<CallExprPtr>()->getArgument(1).as<LiteralPtr>()->getValue()->getValue();
				std::string memberName = typeName + "::" + functionName;

				// now we have to find a function registered in the TU which has the same literal name and the correct parameter types. we have to ignore the
				// result type

				//the first candidate we found
				LiteralPtr candidate;
				//the number of candidates we found. finding no exact match but multiple candidates is an error
				unsigned int candidateCount = 0;

				//now iterate over all functions registered in the TU and filter for the ones with the correct name
				for(const auto& mapEntry : tu.getFunctions()) {
					const auto& key = mapEntry.first;
					if(key.getValue()->getValue() == memberName) {
						if(const auto& keyType = key->getType().isa<FunctionTypePtr>()) {
							if(keyType->isMember()) {
								//if the found function is an exact match, we can stop the search right away
								if (keyType->getParameterTypeList() == argumentTypes) {
									func = key;
									break;

									//otherwise, if we can find a valid parameter substitution, we increment the candidateCount and store the function
								} else if (types::getTypeVariableInstantiation(mgr, keyType->getParameterTypeList(), argumentTypes)) {
									candidateCount++;
									candidate = key;
								}
							}
						}
					}
				}

				//if we didn't change the function, we didn't find an exact match
				if (func == getScalar(callable)) {
					//if we found multiple candidates, this is an error, and the user has to specify which overload to use
					if (candidateCount >= 2) {
						error(l, format("Found %d possible candidates matching the given argument types. Specify which one to call by using the advanced overload selection syntax.",
						                candidateCount));
						return nullptr;

						//if we found a single candidate then we take that one
					} else if (candidateCount == 1) {
						func = candidate;

						//otherwise we fail
					} else {
						error(l, format("Couldn't find member %s in type %s for argument types %s", memberName, typeName, argumentTypes));
						return nullptr;
					}
				}
			}

			auto ftype = func->getType();

			if(!ftype.isa<FunctionTypePtr>()) {
				error(l, format("attempt to call non function expression of type %s", *ftype));
				return nullptr;
			}

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

					//Fix the argument type list accordingly
					while (argumentTypes.size() > args.size() - 1) {
						argumentTypes.pop_back();
					}
					argumentTypes.push_back(args.crbegin()->getType());
				}
			}

			auto lastParamNodeType = funcParamTypes.empty() ? NT_GenericType : funcParamTypes.back()->getNodeType();
			if(args.size() != funcParamTypes.size() && !(lastParamNodeType == NT_VariadicTypeVariable || lastParamNodeType == NT_VariadicGenericTypeVariable)) {
				error(l, format("invalid number of arguments in function call. got %d, expected %d", args.size(), funcParamTypes.size()));
				return nullptr;
			}

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

		ExpressionPtr InspireDriver::genConstructorCall(const location& l, const std::string name, ParserTypedExpressionList args) {
			if (args.size() == 0) {
				error(l, "Constructor calls must have at least the this parameter argument");
				return nullptr;
			}

			// extract the this parameter from the params
			auto thisParam = args[0].expression;

			// and remove it from the parameter list
			args.erase(args.begin());

			// genCall will do everything else
			const auto callable = builder.callExpr(parserIRExtension.getMemberFunctionAccess(), thisParam, builder.getIdentifierLiteral("ctor"));
			return genCall(l, callable, args);
		}

		ExpressionPtr InspireDriver::genDestructorCall(const location& l, const std::string name, const ExpressionPtr& thisArgument) {
			// genCall will do everything else
			const auto callable = builder.callExpr(parserIRExtension.getMemberFunctionAccess(), thisArgument, builder.getIdentifierLiteral("dtor"));
			return genCall(l, callable, ParserTypedExpressionList());
		}

		/**
		 * Generates a new call expression from the given one with the type of the call expression materialized
		 */
		ExpressionPtr InspireDriver::materializeCall(const location& l, const ExpressionPtr& exp) {
			if (const auto& call = exp.isa<CallExprPtr>()) {
				const auto& callType = call->getType();
				TypePtr newType = lang::buildRefType(callType);
				if(analysis::isRefType(callType)) {
					auto callRefType = lang::ReferenceType(callType);
					callRefType.setKind(lang::ReferenceType::Kind::Plain);
					newType = callRefType.toType();
				}
				return builder.callExpr(newType, call->getFunctionExpr(), call->getArgumentList());
			}
			return exp;
		}

		ExpressionPtr InspireDriver::genInitializerExprTemp(const location& l, const TypePtr& type, const ExpressionList& list) {
			return genInitializerExpr(l, type, lang::buildRefTemp(type), list);
		}

		ExpressionPtr InspireDriver::genInitializerExpr(const location& l, const TypePtr& type, const ExpressionPtr& memExpr, const ExpressionList& list) {
			if(!lang::isReference(type)) {
				error(l, format("type for initialization must be a reference type (is %s)", *type));
				return nullptr;
			}
			return builder.initExpr(type.as<GenericTypePtr>(), memExpr, builder.expressions(list));
		}

		VariablePtr InspireDriver::genParameter(const location& l, const std::string& name, const TypePtr& type) {
			auto resolvedType = resolveTypeAliases(l, type);
			VariablePtr variable = builder.variable(resolvedType);
			annotations::attachName(variable, name);
			return variable;
		}

		void InspireDriver::registerParameters(const location& l, const VariableList& params, bool _const, bool _volatile) {
			for(const auto& variable : params) {
				declareSymbol(l, annotations::getAttachedName(variable), variable);
			}
			// get this-type (which is ref<ref in case of a function
			if(isInRecordType()) {
				auto thisType = getThisTypeForLambdaAndFunction(_const, _volatile);
				auto thisParam = builder.variable(thisType);
				thisStack.push_back(thisParam);
			}
		}

		void InspireDriver::unregisterParameters() {
			if(isInRecordType()) {
				assert_false(thisStack.empty());
				thisStack.pop_back();
			}
		}


		ExpressionPtr InspireDriver::genJobInternal(const location& l, const ExpressionPtr& expr,
			                                        const std::function<ExpressionPtr(const ExpressionPtr&)>& jobGenerator) {
			auto scalarExpr = getScalar(expr);
			if(!scalarExpr.isa<CallExprPtr>()) {
				error(l, "expression in job must be a call expression");
				return nullptr;
			}
			auto bind = builder.bindExpr(VariableList(), scalarExpr.as<CallExprPtr>());
			return jobGenerator(bind);
		}

		ExpressionPtr InspireDriver::genJobExpr(const location& l, const ExpressionPtr& lowerBound, const ExpressionPtr& upperBound,
			                                    const ExpressionPtr& expr) {
			return genJobInternal(l, expr, [&](const ExpressionPtr& bind) { return builder.jobExpr(getScalar(lowerBound), getScalar(upperBound), bind); });
		}

		ExpressionPtr InspireDriver::genJobExpr(const location& l, const ExpressionPtr& lowerBound, const ExpressionPtr& upperBound,
			                                    const ExpressionPtr& modExpr, const ExpressionPtr& expr) {
			return genJobInternal(l, expr, [&](const ExpressionPtr& bind) {
				return builder.jobExpr(getScalar(lowerBound), getScalar(upperBound), getScalar(modExpr), bind);
			});
		}

		ExpressionPtr InspireDriver::genJobExpr(const location& l, const ExpressionPtr& lowerBound, const ExpressionPtr& expr) {
			return genJobInternal(l, expr, [&](const ExpressionPtr& bind) { return builder.jobExprUnbounded(getScalar(lowerBound), bind); });
		}

		ExpressionPtr InspireDriver::genJobExpr(const location& l, const ExpressionPtr& expr) {
			return genJobInternal(l, expr, [&](const ExpressionPtr& bind) { return builder.jobExpr(builder.getThreadNumRange(1), bind.as<ExpressionPtr>()); });
		}

		ExpressionPtr InspireDriver::genSync(const location& l, const ExpressionPtr& expr) {
			return builder.callExpr(builder.getLangBasic().getUnit(), builder.getExtension<lang::ParallelExtension>().getMerge(), getScalar(expr));
		}

		ExpressionPtr InspireDriver::genSyncAll(const location& l) {
			return builder.callExpr(builder.getLangBasic().getUnit(), builder.getExtension<lang::ParallelExtension>().getMergeAll());
		}

		ExpressionPtr InspireDriver::genDerefExpr(const location& l, const ExpressionPtr& expr) {
			auto scalarExpr = getScalar(expr);
			if(!analysis::isRefType(scalarExpr->getType())) {
				error(l, format("cannot deref non ref type: %s", toString(scalarExpr->getType())));
				return nullptr;
			}

			return builder.deref(scalarExpr);
		}

		ExpressionPtr InspireDriver::genAsExpr(const location& l, const ExpressionPtr& expr, const TypePtr& type) {
			auto scalarExpr = getScalar(expr);
			if(!scalarExpr->getType()) {
				error(l, "can not get parent-reference of non referenced expression");
				return nullptr;
			}

			return core::lang::buildRefParentCast(scalarExpr, type);
		}

		DeclarationStmtPtr InspireDriver::genVariableDefinition(const location& l, const TypePtr& type, const std::string name, const ExpressionPtr& init) {
			auto var = genVariableDeclaration(l, type, name);
			if (!var) {
				return nullptr;
			}
			return builder.declarationStmt(var, getScalar(init));
		}

		/**
		 * constructs a new variable declaration with a given type
		 */
		VariablePtr InspireDriver::genVariableDeclaration(const location& l, const TypePtr& type, const std::string name) {
			auto resolvedType = resolveTypeAliases(l, type);
			auto variable = builder.variable(resolvedType);
			annotations::attachName(variable, name);
			declareSymbol(l, name, variable);
			if (wereErrors()) {
				return nullptr;
			}
			return variable;
		}

		/**
		 * constructs a new declaration statement for the variable with name name and the given init expression
		 */
		DeclarationStmtPtr InspireDriver::genDeclarationStmt(const location& l, const std::string name, const ExpressionPtr& init) {
			auto var = lookupDeclared(name).isa<VariablePtr>();
			if (!var) {
				error(l, format("Symbol %s is not a variable", name));
				return nullptr;
			}
			return builder.declarationStmt(var, getScalar(init));
		}

		/**
		 * constructs a new declaration statement for the variable with an undefined init expression
		 */
		DeclarationStmtPtr InspireDriver::genUndefinedDeclarationStmt(const location& l, const TypePtr& type, const std::string name) {
			auto var = genVariableDeclaration(l, type, name);
			if (!var) {
				return nullptr;
			}
			return builder.declarationStmt(var, lang::buildRefDecl(var->getType()));
		}

		ForStmtPtr InspireDriver::genForStmt(const location& l, const TypePtr& iteratorType, const std::string iteratorName, const ExpressionPtr& lowerBound,
			                                 const ExpressionPtr& upperBound, const ExpressionPtr& stepExpr, const StatementPtr& body) {
			VariablePtr iteratorVariable = findSymbol(l, iteratorName).isa<VariablePtr>();
			assert_true(iteratorVariable) << "Variable doesn't exist or isn't a VariablePtr";
			return builder.forStmt(iteratorVariable, getScalar(lowerBound), getScalar(upperBound), getScalar(stepExpr), body);
		}

		void InspireDriver::declareRecordType(const location& l, const std::string name) {
			const GenericTypePtr key = builder.genericType(name);

			//declare the type
			declareType(l, name, key);

			//now register all the default members
			TypePtr thisType = builder.refType(key);

			{
				//default constructor
				auto ctorType = builder.functionType(toVector(thisType), thisType, FK_CONSTRUCTOR);
				auto lit = builder.getLiteralForConstructor(ctorType);
				tu.addFunction(lit, builder.lambdaExpr(ctorType, builder.parameters(), parserIRExtension.getMemberDummyLambda()));
			}

			{
				//default copy constructor
				TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), true, false, lang::ReferenceType::Kind::CppReference);
				auto ctorType = builder.functionType(toVector(thisType, otherType), thisType, FK_CONSTRUCTOR);
				auto lit = builder.getLiteralForConstructor(ctorType);
				tu.addFunction(lit, builder.lambdaExpr(ctorType, builder.parameters(), parserIRExtension.getMemberDummyLambda()));
			}

			{
				//default move constructor
				TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppRValueReference);
				auto ctorType = builder.functionType(toVector(thisType, otherType), thisType, FK_CONSTRUCTOR);
				auto lit = builder.getLiteralForConstructor(ctorType);
				tu.addFunction(lit, builder.lambdaExpr(ctorType, builder.parameters(), parserIRExtension.getMemberDummyLambda()));
			}

			{
				//default destructor
				auto dtorType = builder.functionType(toVector(thisType), thisType, FK_DESTRUCTOR);
				auto lit = builder.getLiteralForDestructor(dtorType);
				tu.addFunction(lit, builder.lambdaExpr(dtorType, builder.parameters(), parserIRExtension.getMemberDummyLambda()));
			}

			{
				//default copy assignment operator
				TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), true, false, lang::ReferenceType::Kind::CppReference);
				TypePtr resType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppReference);
				auto funType = builder.functionType(toVector(thisType, otherType), resType, FK_MEMBER_FUNCTION);
				auto lit = builder.getLiteralForMemberFunction(funType, utils::getMangledOperatorAssignName());
				tu.addFunction(lit, builder.lambdaExpr(funType, builder.parameters(), parserIRExtension.getMemberDummyLambda()));
			}

			{
				//default move assignment operator
				TypePtr otherType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppRValueReference);
				TypePtr resType = builder.refType(analysis::getReferencedType(thisType), false, false, lang::ReferenceType::Kind::CppReference);
				auto funType = builder.functionType(toVector(thisType, otherType), resType, FK_MEMBER_FUNCTION);
				auto lit = builder.getLiteralForMemberFunction(funType, utils::getMangledOperatorAssignName());
				tu.addFunction(lit, builder.lambdaExpr(funType, builder.parameters(), parserIRExtension.getMemberDummyLambda()));
			}
		}

		void InspireDriver::genDeclaration(const location& l, const std::string name, const TypePtr& type) {
			//first build the literal we want to register
			auto literal = builder.literal(name, type);

			//now we handle members of classes differently
			if (auto functionType = type.isa<FunctionTypePtr>()) {
				if (functionType->isMember()) {
					if (functionType->isConstructor()) {
						literal = builder.getLiteralForConstructor(functionType);

					} else if (functionType->isDestructor()) {
						literal = builder.getLiteralForDestructor(functionType);

					} else {
						literal = builder.getLiteralForMemberFunction(functionType, name);
					}

					//register the member in the TU
					tu.addFunction(literal, builder.lambdaExpr(functionType, builder.parameters(), parserIRExtension.getExplicitMemberDummyLambda()));
					//and return here. there is no need to register this symbol in the global scope, as member calls are handled differently
					return;
				}
			}

			declareSymbol(l, literal->getValue()->getValue(), literal);
		}

		ExpressionPtr InspireDriver::genThis(const location& l) {
			assert_false(thisStack.empty());
			return inLambda ? thisStack.back() : builder.deref(thisStack.back());
		}

		ExpressionPtr InspireDriver::genMemLambdaReference(const location& l, const string& structName, const string& lambdaName) {
			auto lookupTy = findType(l, structName);
			auto genTy = lookupTy ? lookupTy.isa<core::GenericTypePtr>() : nullptr;
			if(!genTy) {
				error(l, format("Symbol %s is not a struct type declared previously", structName));
				return nullptr;
			}
			auto structTyIt = tu.getTypes().find(genTy);
			if(structTyIt == tu.getTypes().end()) {
				error(l, format("Struct %s not found in current TU", structName));
				return nullptr;
			}
			auto structTy = structTyIt->second;
			for(const auto& memFun : structTy->getRecord()->getMemberFunctions()->getMembers()) {
				if(memFun->getNameAsString() == lambdaName) {
					return memFun->getImplementation();
				}
			}
			error(l, format("Struct %s does not contain member function %s", structName, lambdaName));
			return nullptr;
		}

		namespace {
			bool isDeletedBody(const InspireDriver& driver, const ExpressionPtr& lambda) {
				auto lambdaExpr = lambda.isa<LambdaExprPtr>();
				if(!lambdaExpr) return false;
				if(lambdaExpr->getBody() == driver.getParserDeleteCompound()) return true;
				return false;
			};

			RecordPtr handleDefaultedAndDeletedMembers(const InspireDriver& driver, const RecordPtr& record) {
				auto& builder = driver.builder;
				bool isStruct = record.isa<StructPtr>();
				auto parents = isStruct ? record.as<StructPtr>()->getParents() : builder.parents();

				// handle constructors
				auto ctors = record->getConstructors();
				ExpressionList newCtors;
				for(auto& ctor : ctors) {
					if(isDeletedBody(driver, ctor)) continue;
					newCtors.push_back(ctor);
				}

				// handle destructor
				ExpressionList newDtor { record->getDestructor() };
				if(isDeletedBody(driver, newDtor[0])) {
					newDtor.clear();
				}

				// handle operators
				auto mfuns = record->getMemberFunctions();
				MemberFunctionList newMfuns;
				for(auto& mfun : mfuns) {
					if(isDeletedBody(driver, mfun->getImplementation())) continue;
					newMfuns.push_back(mfun);
				}

				// generate replacements
				if(isStruct) {
					return builder.structRecord(record->getName(), parents, record->getFields(), builder.expressions(newCtors), builder.expressions(newDtor),
						                        record->getDestructorVirtual(), builder.memberFunctions(newMfuns), record->getPureVirtualMemberFunctions());
				} else {
					return builder.unionRecord(record->getName(), record->getFields(), builder.expressions(newCtors), builder.expressions(newDtor),
						                       record->getDestructorVirtual(), builder.memberFunctions(newMfuns), record->getPureVirtualMemberFunctions());
				}
			}
		}

		void InspireDriver::computeResult(const NodePtr& fragment) {
			result = tu.resolve(fragment);

			// replace all temporaries generated for anonymous records
			NodeMap replacements;
			auto emptyName = builder.stringValue("");
			for(auto temporaryName : temporaryAnonymousNames) {
				replacements[temporaryName] = emptyName;
				replacements[builder.stringValue(temporaryName->getValue() + "::ctor")] = builder.stringValue("::ctor");
				replacements[builder.stringValue(temporaryName->getValue() + "::dtor")] = builder.stringValue("::dtor");
				replacements[builder.stringValue(temporaryName->getValue() + "::" + utils::getMangledOperatorAssignName())] = builder.stringValue("::" + utils::getMangledOperatorAssignName());
			}
			result = transform::replaceAll(mgr, result, replacements, transform::globalReplacement);
			replacements.clear();

			// replace defaulted member functions and remove deleted ones
			visitDepthFirstOnce(result, [&](const RecordPtr& record) {
				replacements[record] = handleDefaultedAndDeletedMembers(*this, record);
			}, true);

			result = transform::replaceAll(mgr, result, replacements, transform::globalReplacement);

			// check if artifacts remain
			bool foundArtifacts = false;
			visitDepthFirstOnceInterruptible(result, [&](const NodePtr& node) {
				if(node == parserIRExtension.getMemberDummyLambda() || node == parserIRExtension.getExplicitMemberDummyLambda()
				   || node == parserIRExtension.getDefaultedBodyCompound() || node == parserIRExtension.getDeletedBodyCompound()
				   || node == parserIRExtension.getDefaultedBodyMarker()) {
					foundArtifacts = true;
					return true;
				}
				return false;
			}, true, true);

			if(foundArtifacts) { assert_fail() << "Found parser artifacts in final parser result."; }
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

		NodePtr InspireDriver::lookupDeclared(const std::string& name) {
			// look in declared symbols
			for(auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
				const DefinitionMap& cur = (*it)->declaredSymbols;
				auto pos = cur.find(name);
				if(pos != cur.end()) { return pos->second(); }
			}
			return nullptr;
		}

		NodePtr InspireDriver::lookupDeclaredInGlobalScope(const std::string& name) {
			const DefinitionMap& cur = scopes[0]->declaredSymbols;
			auto pos = cur.find(name);
			if(pos != cur.end()) { return pos->second(); }
			return nullptr;
		}

		ExpressionPtr InspireDriver::findSymbol(const location& l, const std::string& name) {
			assert_false(scopes.empty()) << "Missing global scope!";

			NodePtr result = lookupDeclared(name);

			// look in lang basic
			if(!result) {
				try {
					result = builder.getLangBasic().getBuiltIn(name);
				} catch(...) {
					// pass, nothing to do really
				}
			}

			//if we didn't find anything and we are in a record type currently
			if (!result && isInRecordType()) {
				//we try to locate a record member using the implicit this pointer here
				result = genMemberAccess(l, genThis(l), name);
			}

			// fail if not found
			if(!result) {
				error(l, format("the symbol %s was not declared in this context", name));
				return nullptr;
			}
			if(!result.isa<ExpressionPtr>()) {
				error(l, format("the symbol %s is not an expression but of type %s", name, result->getNodeType()));
				return nullptr;
			}

			return result.as<ExpressionPtr>();
		}

		TypePtr InspireDriver::findType(const location& l, const std::string& name) {
			assert_false(scopes.empty()) << "Missing global scope!";

			NodePtr result;

			// look in declared types
			for(auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
				const DefinitionMap& cur = (*it)->declaredTypes;
				auto pos = cur.find(name);
				if(pos != cur.end()) {
					result = pos->second();
					break;
				}
			}

			// check the type
			if(result && !result.isa<TypePtr>()) {
				error(l, format("the symbol %s is not a type", name));
				return nullptr;
			}

			// done
			return result.as<TypePtr>();
		}

		void InspireDriver::openScope() { scopes.push_back(std::make_shared<Scope>()); }

		void InspireDriver::closeScope() {
			assert_gt(scopes.size(), 1) << "Attempted to pop global scope!";
			scopes.pop_back();
		}

		std::shared_ptr<InspireDriver::Scope> InspireDriver::getCurrentScope() {
			assert_false(scopes.empty()) << "Missing global scope!";
			return scopes.back();
		}

		bool InspireDriver::checkSymbolName(const location& l, const std::string& name) {
			// check symbol name for validity
			if(name.find(".") != std::string::npos) {
				error(l, format("symbol names can not contain dot chars: %s", name));
				return false;
			}
			return true;
		}

		void InspireDriver::declareSymbol(const location& l, const std::string& name, const NodeFactory& factory) {
			assert_false(scopes.empty()) << "Missing global scope!";
			// check name for validity
			if(!checkSymbolName(l, name)) { return; }

			// ignore wildcard for unused variables
			if(name == "_") { return; }

			// make sure that there isn't already a declaration for this symbol
			if(getCurrentScope()->declaredSymbols.find(name) != getCurrentScope()->declaredSymbols.end()) {
				error(l, format("Symbol %s has already been declared", name));
				return;
			}

			getCurrentScope()->declaredSymbols[name] = factory;
		}

		void InspireDriver::declareSymbol(const location& l, const std::string& name, const ExpressionPtr& node) {
			declareSymbol(l, name, [=]() -> NodePtr { return node; });
		}

		void InspireDriver::declareSymbolInGlobalScope(const location& l, const std::string& name, const ExpressionPtr& node) {
			assert_false(scopes.empty()) << "Missing global scope!";
			// check name for validity
			if(!checkSymbolName(l, name)) { return; }

			// make sure that there isn't already a declaration for this symbol
			if(scopes[0]->declaredSymbols.find(name) != scopes[0]->declaredSymbols.end()) {
				error(l, format("Symbol %s has already been declared", name));
				return;
			}

			scopes[0]->declaredSymbols[name] = [=]() -> NodePtr { return node; };
		}

		bool InspireDriver::isSymbolDeclaredInCurrentScope(const std::string name) {
			return getCurrentScope()->declaredSymbols.find(name) != getCurrentScope()->declaredSymbols.end();
		}

		bool InspireDriver::isSymbolDeclaredInGlobalScope(const std::string name) {
			assert_false(scopes.empty()) << "Missing global scope!";
			return scopes[0]->declaredSymbols.find(name) != scopes[0]->declaredSymbols.end();
		}

		void InspireDriver::declareType(const location& l, const std::string& name, const TypePtr& node) {
			assert_false(scopes.empty()) << "Missing global scope!";
			// check name for validity
			if(!checkSymbolName(l, name)) { return; }

			// make sure that there isn't already a declaration for this type
			if(getCurrentScope()->declaredTypes.find(name) != getCurrentScope()->declaredTypes.end()) {
				error(l, format("Type %s has already been declared", name));
				return;
			}

			// insert it into the declared types
			getCurrentScope()->declaredTypes[name] = [=]() -> NodePtr { return node; };
		}

		bool InspireDriver::isTypeDeclaredInCurrentScope(const std::string name) {
			return getCurrentScope()->declaredTypes.find(name) != getCurrentScope()->declaredTypes.end();
		}

		void InspireDriver::addTypeAlias(const TypePtr& pattern, const TypePtr& substitute) {
			if(*pattern == *substitute) return;
			auto& aliases = getCurrentScope()->aliases;
			assert_true(aliases.find(pattern) == aliases.end());
			aliases[pattern] = substitute;
		}

		TypePtr InspireDriver::resolveTypeAliases(const location& l, const TypePtr& type) {
			if(!type) return type;

			NodeManager& mgr = type.getNodeManager();

			// run through alias lists
			for(auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
				for(const auto& cur : (*it)->aliases) {
					// check whether pattern matches
					if(auto sub = types::match(mgr, type, cur.first)) {
						// compute substitution
						auto next = (*sub).applyTo(cur.second);
						assert_ne(type, next) << "Invalid pattern matching:\n"
							                  << "Target:     " << *type << "\n"
							                  << "Pattern:    " << *cur.first << "\n"
							                  << "Substitute: " << *cur.first << "\n"
							                  << "Match:      " << *sub << "\n"
							                  << "Next:       " << *next << "\n";

						// apply pattern and start over again
						return resolveTypeAliases(l, next);
					}
				}
			}

			// no matching alias fond => done
			return type;
		}

		void InspireDriver::beginRecord(const location& l, const std::string& name) {
			auto key = builder.genericType(name);

			// only declare the type implicitly if it hasn't already been declared
			if(!isTypeDeclaredInCurrentScope(name)) { declareRecordType(l, name); }

			openScope();
			currentRecordStack.push_back({key});
		}

		void InspireDriver::endRecord() {
			assert_false(currentRecordStack.empty());
			closeScope();
			currentRecordStack.pop_back();
		}

		bool InspireDriver::isInRecordType() { return !currentRecordStack.empty(); }

		GenericTypePtr InspireDriver::getThisType() {
			assert_false(currentRecordStack.empty());
			return currentRecordStack.back().record;
		}

		void InspireDriver::importExtension(const location& l, const std::string& extensionName) {
			auto name = extensionName;
			name.replace(0, 1, "");
			name.replace(name.size() - 1, 1, "");

			// get extension factory from the registry
			auto optFactory = lang::ExtensionRegistry::getInstance().lookupExtensionFactory(name);
			if(optFactory) {
				importExtension((*optFactory)(mgr));
			} else {
				error(l, format("Unable to locate module: %s", name));
			}
		}

		void InspireDriver::importExtension(const lang::Extension& extension) {
			assert_true(scopes.size() == 1) << "Cannot import extension in non global scope!";

			// import symbols
			for(const auto& cur : extension.getSymbols()) {
				if(!isSymbolDeclaredInCurrentScope(cur.first)) { declareSymbol(location(), cur.first, cur.second); }
			}

			// import type aliases
			auto& aliases = getCurrentScope()->aliases;
			for(const auto& cur : extension.getTypeAliases()) {
				if(aliases.find(cur.first) != aliases.end()) {
					assert_true(aliases[cur.first] == cur.second) << "Confliction type alias for " << *(cur.first) << ". " << *(aliases[cur.first]) << " vs. "
						                                          << *(cur.second);
					continue;
				}
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

		void InspireDriver::error(const location& l, const std::string& m) const { errors.push_back(ParserError(l, m)); }

		void InspireDriver::error(const std::string& m) const { errors.push_back(ParserError(globLoc, m)); }

		bool InspireDriver::wereErrors() const { return !errors.empty(); }


		void InspireDriver::printErrors(std::ostream& out, bool color) const {
			if(&out == &std::cout) {
				// print the errors only once to stdout
				if(printedErrors) { return; }
				printedErrors = true;
			}

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
