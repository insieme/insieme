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

#include "insieme/core/ir_builder.h"

#include <tuple>
#include <limits>
#include <set>

#include "insieme/core/ir_node.h"

#include "insieme/core/ir_values.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_int_type_param.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_program.h"

#include "insieme/core/type_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/core/encoder/lists.h"

#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/parser2/ir_parser.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/functional_utils.h"

namespace insieme {
namespace core {

	const lang::BasicGenerator& IRBuilder::getLangBasic() const {
			return manager.getLangBasic();
	}

namespace {

	typedef std::tuple<vector<VariablePtr>, vector<ExpressionPtr>> InitDetails;

	InitDetails splitUp(const IRBuilder::VarValueMapping& captureInits) {

		// prepare containers
		InitDetails res;
		vector<VariablePtr>& vars = std::get<0>(res);
		vector<ExpressionPtr>& inits = std::get<1>(res);

		// process the given map
		for_each(captureInits, [&](const IRBuilder::VarValueMapping::value_type& cur) {
			vars.push_back(cur.first);
			inits.push_back(cur.second);
		});

		// return results
		return res;
	}

	/**
	 * Returns the list of variables referenced within an expression.
	 * This class is used when a code block needs to be transformed into a function
	 */
	struct VarRefFinder: public IRVisitor<bool> {

	    VarRefFinder() : core::IRVisitor<bool>(false) { }

	    bool visitVariable(const core::VariablePtr& varExpr) {
	    	usedVars.insert(varExpr);
	    	return true;
	    }

	    bool visitLambdaExpr(const core::LambdaExprPtr& lambdaExpr) { return true; }

	    bool visitDeclarationStmt(const core::DeclarationStmtPtr& declStmt) {
	        declaredVars.insert( declStmt->getVariable() );
	        return false;
	    }

	    bool visitNode(const NodePtr& node) { return false; }

	    utils::set::PointerSet<VariablePtr> declaredVars;
	    utils::set::PointerSet<VariablePtr> usedVars;
	};

	std::vector<VariablePtr> getRechingVariables(const core::NodePtr& root) {
		VarRefFinder visitor;
		visitDepthFirstPrunable(root, visitor);

		// Define the comparator for the set 
		auto cmp = [](const VariablePtr& lhs, const VariablePtr& rhs) -> bool { 
			return *lhs < *rhs;
		};
		std::set<VariablePtr, decltype(cmp)> nonDecls(cmp);

		std::set_difference( 
				visitor.usedVars.begin(), visitor.usedVars.end(),
				visitor.declaredVars.begin(), visitor.declaredVars.end(), 
				std::inserter(nonDecls, nonDecls.begin())
			);

		return std::vector<VariablePtr>(nonDecls.begin(), nonDecls.end());
	}

}


NodePtr IRBuilder::get(NodeType type, const NodeList& children) const {

	switch(type) {
	#define CONCRETE(KIND) \
		case NT_ ## KIND : return get< NT_## KIND >(children);
	#include "insieme/core/ir_nodes.def"
	#undef CONCRETE
	}

	assert(false && "Unsupported node type added!");
	return NodePtr();
}

ProgramPtr IRBuilder::createProgram(const ExpressionList& entryPoints) const {
	return Program::get(manager, entryPoints);
}


// ---------------------------- Parser Integration -----------------------------------

NodePtr IRBuilder::parse(const string& code, const std::map<string, NodePtr>& symbols) const {
	return parser::parse(manager, code, true, symbols);
}

TypePtr IRBuilder::parseType(const string& code, const std::map<string, NodePtr>& symbols) const {
	return parser::parse_type(manager, code, true, symbols);
}

ExpressionPtr IRBuilder::parseExpr(const string& code, const std::map<string, NodePtr>& symbols) const {
	return parser::parse_expr(manager, code, true, symbols);
}

StatementPtr IRBuilder::parseStmt(const string& code, const std::map<string, NodePtr>& symbols) const {
	return parser::parse_stmt(manager, code, true, symbols);
}

ProgramPtr IRBuilder::parseProgram(const string& code, const std::map<string, NodePtr>& symbols) const {
	return parser::parse_program(manager, code, true, symbols);
}

vector<NodeAddress> IRBuilder::parseAddresses(const string& code, const std::map<string, NodePtr>& symbols) const {
	return parser::parse_addresses(manager, code, true, symbols);
}


// ---------------------------- Standard Nodes -----------------------------------

#include "ir_builder_impl.inl"

StringValuePtr IRBuilder::stringValue(const char* str) const {
	return stringValue(string(str));
}

StringValuePtr IRBuilder::stringValue(const string& str) const {
	return StringValue::get(manager, str);
}

BoolValuePtr IRBuilder::boolValue(bool value) const {
	return BoolValue::get(manager, value);
}

CharValuePtr IRBuilder::charValue(char value) const {
	return CharValue::get(manager, value);
}

IntValuePtr IRBuilder::intValue(int value) const {
	return IntValue::get(manager, value);
}

UIntValuePtr IRBuilder::uintValue(unsigned value) const {
	return UIntValue::get(manager, value);
}


// ---------------------------- Convenience -------------------------------------

bool IRBuilder::matchType(const std::string& typeStr, const core::TypePtr& irType) const {

	// the type used for caching parser results
	struct ParserCache : public std::map<string, TypePtr> {};
	typedef std::shared_ptr<ParserCache> ParserCachePtr;

	// lookup result within cache
	NodePtr cacheNode = manager.get(breakStmt());		// some node that is easy to find :)

	// get reference to cache
	if (!cacheNode->hasAttachedValue<ParserCachePtr>()) {
		cacheNode->attachValue(std::make_shared<ParserCache>());
	}
	const ParserCachePtr& cache = cacheNode->getAttachedValue<ParserCachePtr>();

	// get cached result or parse
	TypePtr type;
	auto pos = cache->find(typeStr);
	if (pos == cache->end()) {
		type = parseType(typeStr);
		cache->insert(std::make_pair(typeStr, type));
	} else {
		type = pos->second;
	}

	// try unify the parsed type and the given type
	return unify(manager, type, irType);
}

GenericTypePtr IRBuilder::genericType(const StringValuePtr& name, const TypeList& typeParams, const IntParamList& intParams) const {
	return genericType(name, types(typeParams), intTypeParams(intParams));
}


StructTypePtr IRBuilder::structType(const vector<std::pair<StringValuePtr,TypePtr>>& entries) const {
	vector<NamedTypePtr> members;
	::transform(entries, std::back_inserter(members), [&](const std::pair<StringValuePtr,TypePtr>& cur) {
		return namedType(cur.first, cur.second);
	});
	return structType(members);
}

UnionTypePtr IRBuilder::unionType(const vector<std::pair<StringValuePtr,TypePtr>>& entries) const {
	vector<NamedTypePtr> members;
	::transform(entries, std::back_inserter(members), [&](const std::pair<StringValuePtr,TypePtr>& cur) {
		return namedType(cur.first, cur.second);
	});
	return unionType(members);
}

NamedTypePtr IRBuilder::namedType(const string& name, const TypePtr& type) const {
	return namedType(stringValue(name), type);
}

NamedValuePtr IRBuilder::namedValue(const string& name, const ExpressionPtr& value) const {
	return namedValue(stringValue(name), value);
}


TupleExprPtr IRBuilder::tupleExpr(const vector<ExpressionPtr>& values) const {
	TupleTypePtr type = tupleType(extractTypes(values));
	return tupleExpr(type, Expressions::get(manager, values));
}

VectorExprPtr IRBuilder::vectorExpr(const VectorTypePtr& type, const ExpressionList& values) const {
	return vectorExpr(type, expressions(values));
}

VectorExprPtr IRBuilder::vectorExpr(const ExpressionList& values) const {
	assert(!values.empty() && "Cannot infere vector type using empty value vector.");
	return vectorExpr(vectorType(values.front()->getType(), concreteIntTypeParam(values.size())), values);
}

StructExprPtr IRBuilder::structExpr(const StructTypePtr& structType, const vector<NamedValuePtr>& values) const {
	return structExpr(structType, namedValues(values));
}

StructExprPtr IRBuilder::structExpr(const vector<std::pair<StringValuePtr, ExpressionPtr>>& members) const {
	vector<NamedTypePtr> types;
	vector<NamedValuePtr> values;
	for_each(members, [&](const pair<StringValuePtr, ExpressionPtr>& cur) {
		types.push_back(namedType(cur.first, cur.second->getType()));
		values.push_back(namedValue(cur.first, cur.second));
	});
	return structExpr(structType(types), namedValues(values));
}

StructExprPtr IRBuilder::structExpr(const vector<NamedValuePtr>& values) const {
	vector<NamedTypePtr> types;
	for_each(values, [&](const NamedValuePtr& cur) {
		types.push_back(namedType(cur->getName(), cur->getValue()->getType()));
	});
	return structExpr(structType(types), namedValues(values));
}


IfStmtPtr IRBuilder::ifStmt(const ExpressionPtr& condition, const StatementPtr& thenBody, const StatementPtr& elseBody) const {
	if (!elseBody) {
		return ifStmt(condition, wrapBody(thenBody), getNoOp());
	}
	return ifStmt(condition, wrapBody(thenBody), wrapBody(elseBody));
}

WhileStmtPtr IRBuilder::whileStmt(const ExpressionPtr& condition, const StatementPtr& body) const {
	return whileStmt(condition, wrapBody(body));
}

ForStmtPtr IRBuilder::forStmt(const DeclarationStmtPtr& var, const ExpressionPtr& end, const ExpressionPtr& step, const StatementPtr& body) const {
	return forStmt(var->getVariable(), var->getInitialization(), end, step, wrapBody(body));
}

ForStmtPtr IRBuilder::forStmt(const VariablePtr& var, const ExpressionPtr& start, const ExpressionPtr& end, const ExpressionPtr& step, const StatementPtr& body) const {
	return forStmt(var, start, end, step, wrapBody(body));
}

SwitchStmtPtr IRBuilder::switchStmt(const ExpressionPtr& switchExpr, const vector<std::pair<ExpressionPtr, StatementPtr>>& cases, const StatementPtr& defaultCase) const {
	CompoundStmtPtr defCase = (defaultCase)?wrapBody(defaultCase):getNoOp();

	vector<SwitchCasePtr> caseList = ::transform(cases, [&](const pair<ExpressionPtr, StatementPtr>& cur) {
		return switchCase(static_pointer_cast<LiteralPtr>(cur.first), wrapBody(cur.second));
	});

	return switchStmt(switchExpr, switchCases(caseList), defCase);
}

SwitchStmtPtr IRBuilder::switchStmt(const ExpressionPtr& switchExpr, const vector<SwitchCasePtr>& cases, const StatementPtr& defaultCase) const {
	return switchStmt(switchExpr, switchCases(cases), (defaultCase)?wrapBody(defaultCase):getNoOp());
}

FunctionTypePtr IRBuilder::toPlainFunctionType(const FunctionTypePtr& funType) const {
	if (funType->isPlain()) {
		return funType;
	}
	return functionType(funType->getParameterTypes(), funType->getReturnType(), true);
}

FunctionTypePtr IRBuilder::toThickFunctionType(const FunctionTypePtr& funType) const {
	if (!funType->isPlain()) {
		return funType;
	}
	return functionType(funType->getParameterTypes(), funType->getReturnType(), false);
}


LiteralPtr IRBuilder::stringLit(const string& str) const {
	return literal(str, getLangBasic().getString());
}

namespace {

	template<typename T>
	bool isInRange(const int val) {
		return std::numeric_limits<T>::min() <= val && val <= std::numeric_limits<T>::max();
	}

}

LiteralPtr IRBuilder::intLit(const int val, bool tight) const {
	if (!tight) {
		return literal(getLangBasic().getInt4(), toString(val));
	}

	TypePtr type;
	if (isInRange<int8_t>(val)) {
		type = getLangBasic().getInt1();
	} else if (isInRange<int16_t>(val)) {
		type = getLangBasic().getInt2();
	} else {
		type = getLangBasic().getInt4();
	}
    return literal(type, toString(val));
}

LiteralPtr IRBuilder::uintLit(const unsigned int val, bool tight) const {
	if (!tight) {
		return literal(getLangBasic().getUInt4(), toString(val));
	}

	TypePtr type;
	if (isInRange<uint8_t>(val)) {
		type = getLangBasic().getUInt1();
	} else if (isInRange<uint16_t>(val)) {
		type = getLangBasic().getUInt2();
	} else {
		type = getLangBasic().getUInt4();
	}
	return literal(type, toString(val));
}

LiteralPtr IRBuilder::integerLit(const int val, bool tight) const {
	if (val < 0) {
		return intLit(val, tight);
	}
	return uintLit((unsigned int)val, tight);
}

LiteralPtr IRBuilder::boolLit(bool value) const {
	return literal(getLangBasic().getBool(), (value)?"true":"false");
}

LiteralPtr IRBuilder::floatLit(const string& value) const {
	return literal(getLangBasic().getReal4(), value);
}

LiteralPtr IRBuilder::floatLit(float value) const {
	return floatLit(toString(value));
}

LiteralPtr IRBuilder::doubleLit(const string& value) const {
	return literal(getLangBasic().getReal8(), value);
}

LiteralPtr IRBuilder::doubleLit(double value) const {
	return doubleLit(toString(value));
}

ExpressionPtr IRBuilder::undefined(const TypePtr& type) const {
	return callExpr(type, getLangBasic().getUndefined(), getTypeLiteral(type));
}

ExpressionPtr IRBuilder::undefinedVar(const TypePtr& typ) const {
	if(typ->getNodeType() == core::NT_RefType) {
		core::TypePtr elementType = core::analysis::getReferencedType(typ);
		return refVar(undefinedVar(elementType));
	}
	return callExpr(typ, getLangBasic().getUndefined(), getTypeLiteral(typ));
}
ExpressionPtr IRBuilder::undefinedNew(const TypePtr& typ) const {
	if(typ->getNodeType() == core::NT_RefType) {
		core::TypePtr elementType = core::analysis::getReferencedType(typ);
		return refNew(undefinedNew(elementType));
	}
	return callExpr(typ, getLangBasic().getUndefined(), getTypeLiteral(typ));
}


core::ExpressionPtr IRBuilder::getZero(const core::TypePtr& type) const {

	// if it is an integer ...
	if (manager.getLangBasic().isInt(type)) {
		return core::Literal::get(manager, type, "0");
	}

	// if it is a real ..
	if (manager.getLangBasic().isReal(type)) {
		return core::Literal::get(manager, type, "0.0");
	}

	// if it is a struct ...
	if (type->getNodeType() == core::NT_StructType) {

		// extract type and resolve members recursively
		core::StructTypePtr structType = static_pointer_cast<const core::StructType>(type);

		vector<NamedValuePtr> members;
		for_each(structType->getEntries(), [&](const NamedTypePtr& cur) {
			members.push_back(namedValue(cur->getName(), getZero(cur->getType())));
		});

		return core::StructExpr::get(manager, structType, namedValues(members));
	}

	// if it is a ref type ...
	if (type->getNodeType() == core::NT_RefType) {
		// return the corresponding flavor of NULL
		core::TypePtr elementType = core::analysis::getReferencedType(type);
		return callExpr(type, manager.getLangBasic().getAnyRefToRef(), manager.getLangBasic().getNull(), getTypeLiteral(elementType));
	}

	// TODO: extend for more types
	LOG(FATAL) << "Encountered unsupported type: " << *type;
	assert(false && "Given type not supported yet!");

	// fall-back => no default initialization possible
	return callExpr(type, manager.getLangBasic().getInitZero(), getTypeLiteral(type));
}


CallExprPtr IRBuilder::deref(const ExpressionPtr& subExpr) const {
	TypePtr type = subExpr->getType();
	assert(type->getNodeType() == NT_RefType);
	return callExpr(type.as<RefTypePtr>()->getElementType(), manager.getLangBasic().getRefDeref(), subExpr);
}

ExpressionPtr IRBuilder::tryDeref(const ExpressionPtr& subExpr) const {
	if (subExpr->getType()->getNodeType() != NT_RefType) return subExpr;
	return deref(subExpr);
}

CallExprPtr IRBuilder::refVar(const ExpressionPtr& subExpr) const {
	return callExpr(refType(subExpr->getType()), manager.getLangBasic().getRefVar(), subExpr);
}

CallExprPtr IRBuilder::refNew(const ExpressionPtr& subExpr) const {
	return callExpr(refType(subExpr->getType()), manager.getLangBasic().getRefNew(), subExpr);
}

CallExprPtr IRBuilder::refDelete(const ExpressionPtr& subExpr) const {
	auto& basic = manager.getLangBasic();
	if (basic.isAnyRef(subExpr->getType())) {
		return callExpr(basic.getUnit(), basic.getAnyRefDelete(), subExpr);
	}
	return callExpr(basic.getUnit(), basic.getRefDelete(), subExpr);
}

CallExprPtr IRBuilder::assign(const ExpressionPtr& target, const ExpressionPtr& value) const {
	RefTypePtr targetType = dynamic_pointer_cast<const RefType>(target->getType());
	assert(targetType && "Target of an assignmet must be of type ref<'a>");

	// if the rhs is a union while the lhs is not, try to find the appropriate entry in the union
	if(UnionTypePtr uType = dynamic_pointer_cast<const UnionType>(value->getType())) {
		TypePtr nrtt = targetType->getElementType();
		if(nrtt->getNodeType() != NT_UnionType) {
			auto list = uType.getEntries();
			auto pos = std::find_if(list.begin(), list.end(), [&](const NamedTypePtr& cur) {
				return isSubTypeOf(cur->getType(), nrtt);
			});

			assert(pos != list.end() && "UnionType of assignemnt's value does not contain a subtype of the target's type");
			return callExpr(manager.getLangBasic().getUnit(), manager.getLangBasic().getRefAssign(), target,
					accessMember(value, pos->getName()));
		}
	}

	return callExpr(manager.getLangBasic().getUnit(), manager.getLangBasic().getRefAssign(), target, value);
}

ExpressionPtr IRBuilder::invertSign(const ExpressionPtr& subExpr) const {
    // add a vector init expression if subExpr is of vector type
    ExpressionPtr&& elem = dynamic_pointer_cast<const VectorType>(subExpr->getType()) ?
	    scalarToVector(subExpr->getType(), intLit(0)) : castExpr(subExpr->getType(), intLit(0));

	return callExpr(
			subExpr->getType(), manager.getLangBasic().getOperator(subExpr->getType(), lang::BasicGenerator::Sub),
			elem, subExpr
		);
}

ExpressionPtr IRBuilder::negateExpr(const ExpressionPtr& boolExpr) const {
	assert( manager.getLangBasic().isBool(boolExpr->getType()) && "Cannot negate a non boolean expression.");
	return callExpr(manager.getLangBasic().getBool(), manager.getLangBasic().getBoolLNot(), boolExpr);
}


CallExprPtr IRBuilder::arraySubscript(const ExpressionPtr& array, const ExpressionPtr& index) const {
	assert(!dynamic_pointer_cast<const ArrayType>(array->getType()) && "Accessing array by value is not allowed!");
	auto vType = dynamic_pointer_cast<const VectorType>(array->getType());
	assert(vType && "Tried array subscript operation on non-array expression");
	return callExpr(vType->getElementType(), manager.getLangBasic().getVectorSubscript(), array, index);
}
CallExprPtr IRBuilder::arrayRefElem(const ExpressionPtr& array, const ExpressionPtr& index) const {
	assert(analysis::isRefType(array->getType()) && "Tried vector ref elem operation on non-ref expression");

	auto aType = dynamic_pointer_cast<const ArrayType>(analysis::getReferencedType(array->getType()));
	if(aType) return callExpr(refType(aType->getElementType()), manager.getLangBasic().getArrayRefElem1D(), array, index);
	auto vType = dynamic_pointer_cast<const VectorType>(analysis::getReferencedType(array->getType()));
	assert(vType && "Tried array ref elem operation on non-array-ref expression");
	return callExpr(refType(vType->getElementType()), manager.getLangBasic().getVectorRefElem(), array, index);
}

CallExprPtr IRBuilder::arrayAccess(const ExpressionPtr& array, const ExpressionPtr& index) const {
	if(analysis::isRefType(array->getType()))
		return arrayRefElem(array, index);
	return arraySubscript(array, index);
}
//CallExprPtr IRBuilder::vectorSubscript(const ExpressionPtr& vec, unsigned index) const {
//	auto lit = uintLit(index);
//	vectorSubscript(vec, lit);
//}
CallExprPtr IRBuilder::vectorInit(const ExpressionPtr& initExp, const IntTypeParamPtr& size) const {
	return callExpr(vectorType(initExp->getType(), size), manager.getLangBasic().getVectorInitUniform(), initExp, getIntTypeParamLiteral(size));
}

DeclarationStmtPtr IRBuilder::declarationStmt(const TypePtr& type, const ExpressionPtr& value) const {
	return declarationStmt(variable(type), value);
}


CallExprPtr IRBuilder::acquireLock(const ExpressionPtr& lock) const {
	assert(manager.getLangBasic().isLock(lock->getType()) && "Cannot lock a non-lock type.");
	return callExpr(manager.getLangBasic().getUnit(), manager.getLangBasic().getLockAcquire(), lock);
}
CallExprPtr IRBuilder::releaseLock(const ExpressionPtr& lock) const {
	assert(manager.getLangBasic().isLock(lock->getType()) && "Cannot unlock a non-lock type.");
	return callExpr(manager.getLangBasic().getUnit(), manager.getLangBasic().getLockRelease(), lock);
}
CallExprPtr IRBuilder::createLock() const {
	return callExpr(manager.getLangBasic().getLock(), manager.getLangBasic().getLockCreate());
}


CallExprPtr IRBuilder::atomicOp(const ExpressionPtr& location, const ExpressionPtr& testFunc, const ExpressionPtr& replaceFunc) {
	assert(core::analysis::isRefType(location->getType()) && "Atomic must be applied on ref.");
	// should also check types of testFunc and replaceFunc
	return callExpr(manager.getLangBasic().getAtomic(), location, testFunc, replaceFunc);
}

CallExprPtr IRBuilder::atomicAssignment(const CallExprPtr& assignment) {
	const auto &basic = manager.getLangBasic();
	assert(basic.isRefAssign(assignment->getFunctionExpr()) && "Trying to build atomic assignment from non-assigment");

	const auto &lhs = assignment->getArgument(0), &rhs = assignment->getArgument(1);
	const auto &lhsDeref = deref(lhs);
	CallExprPtr rhsCall = dynamic_pointer_cast<CallExprPtr>(rhs);
	assert(rhsCall && "Unsupported atomic assignment structure");

	ExpressionPtr factor;
	if(*lhsDeref == *rhsCall->getArgument(0)) factor = rhsCall->getArgument(1);
	if(*lhsDeref == *rhsCall->getArgument(1)) factor = rhsCall->getArgument(0);
	assert(factor && "LHS not found in RHS of atomic assignment");

	const auto &rhsFun = rhsCall->getFunctionExpr();
	if(basic.isAddOp(rhsFun)) return callExpr(basic.getAtomicFetchAndAdd(), lhs, factor);
	if(basic.isSubOp(rhsFun)) return callExpr(basic.getAtomicFetchAndSub(), lhs, factor);
	if(basic.isBitwiseAndOp(rhsFun)) return callExpr(basic.getAtomicFetchAndAnd(), lhs, factor);
	if(basic.isBitwiseOrOp(rhsFun)) return callExpr(basic.getAtomicFetchAndOr(), lhs, factor);
	if(basic.isBitwiseXorOp(rhsFun)) return callExpr(basic.getAtomicFetchAndXor(), lhs, factor);
	assert(false && "Unsupported atomic operation");
	return assignment;
}

CallExprPtr IRBuilder::atomicConditional(const IfStmtPtr& statement) {
	assert(false && "Not implemented");
	return CallExprPtr();
}


CallExprPtr IRBuilder::pickVariant(const ExpressionList& variants) const {
	assert(!variants.empty() && "Variant list must not be empty!");
	assert(all(variants, [&](const ExpressionPtr& cur) { return *cur->getType() == *variants[0]->getType(); }) && "All options have to have the same type.");
	return callExpr(variants[0]->getType(), manager.getLangBasic().getPick(), encoder::toIR(manager, variants));
}


namespace {

	TypePtr deduceReturnTypeForCall(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
		// check function expression
		assert(functionExpr->getType()->getNodeType() == NT_FunctionType && "Function expression is not a function!");

		// extract function type
		FunctionTypePtr funType = static_pointer_cast<const FunctionType>(functionExpr->getType());
		assert(funType->getParameterTypes().size() == arguments.size() && "Invalid number of arguments!");

		// deduce return type
		core::TypeList argumentTypes;
		::transform(arguments, back_inserter(argumentTypes), [](const ExpressionPtr& cur) { return cur->getType(); });
		return deduceReturnType(funType, argumentTypes);
	}

	/**
	 * Checks whether the given result type is matching the type expected when using automatic type inference.
	 */
	bool checkType(const TypePtr& resultType, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {
		// check types
		if (*resultType != *deduceReturnTypeForCall(functionExpr, arguments)) {
			// print a warning if they are not matching
			LOG(WARNING) << "Potentially invalid return type for call specified - function type: "
							<< toString(*functionExpr->getType())
							<< ", arguments: " << join(", ", arguments, print<deref<ExpressionPtr>>());
		}
		return true;
	}


	CallExprPtr createCall(const IRBuilder& builder, const TypePtr& resultType, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {

		// check user-specified return type - only when compiled in debug mode
		// NOTE: the check returns true in any case, hence this assertion will never fail - its just a warning!
		// TODO: make this check faster
//		assert(checkType(resultType, functionExpr, arguments) && "Incorrect user-specified return type!");

		// create calling expression
		return builder.callExpr(resultType, functionExpr, arguments);
	}
}

CallExprPtr IRBuilder::callExpr(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) const {
	// use deduced return type to construct call
	return callExpr(deduceReturnTypeForCall(functionExpr, arguments), functionExpr, arguments);
}
CallExprPtr IRBuilder::callExpr(const ExpressionPtr& functionExpr, const ExpressionPtr& arg1) const {
	return callExpr(functionExpr, toVector(arg1));
}
CallExprPtr IRBuilder::callExpr(const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2) const {
	return callExpr(functionExpr, toVector(arg1, arg2));
}
CallExprPtr IRBuilder::callExpr(const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2, const ExpressionPtr& arg3) const {
	return callExpr(functionExpr, toVector(arg1, arg2, arg3));
}
CallExprPtr IRBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr) const {
	return createCall(*this, resultType, functionExpr, toVector<ExpressionPtr>());
}
CallExprPtr IRBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1) const {
	return createCall(*this, resultType, functionExpr, toVector(arg1));
}
CallExprPtr IRBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2) const {
	return createCall(*this, resultType, functionExpr, toVector(arg1, arg2));
}
CallExprPtr IRBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2, const ExpressionPtr& arg3) const {
	return createCall(*this, resultType, functionExpr, toVector(arg1, arg2, arg3));
}

LambdaPtr IRBuilder::lambda(const FunctionTypePtr& type, const ParametersPtr& params, const StatementPtr& body) const {
	return lambda(type, params, wrapBody(body));
}

LambdaPtr IRBuilder::lambda(const FunctionTypePtr& type, const VariableList& params, const StatementPtr& body) const {
	return lambda(type, params, wrapBody(body));
}

LambdaExprPtr IRBuilder::lambdaExpr(const StatementPtr& body, const ParametersPtr& params) const {
	return lambdaExpr(functionType(extractTypes(params->getParameters()), manager.getLangBasic().getUnit(), true), params, wrapBody(body));
}
LambdaExprPtr IRBuilder::lambdaExpr(const StatementPtr& body, const VariableList& params) const {
	return lambdaExpr(body, parameters(params));
}
LambdaExprPtr IRBuilder::lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const ParametersPtr& params) const {
	return lambdaExpr(functionType(extractTypes(params->getParameters()), returnType, true), params, wrapBody(body));
}
LambdaExprPtr IRBuilder::lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const VariableList& params) const {
	return lambdaExpr(returnType, body, parameters(params));
}

LambdaExprPtr IRBuilder::lambdaExpr(const FunctionTypePtr& type, const VariableList& params, const StatementPtr& body) const {
	return lambdaExpr(lambda(type, params, body));
}

BindExprPtr IRBuilder::bindExpr(const VariableList& params, const CallExprPtr& call) const {
	FunctionTypePtr type = functionType(extractTypes(params), call->getType(), false);
	return bindExpr(type, parameters(params), call);
}

JobExprPtr IRBuilder::jobExpr(const ExpressionPtr& threadNumRange, const vector<DeclarationStmtPtr>& localDecls, const vector<GuardedExprPtr>& branches, const ExpressionPtr& defaultExpr) const {
	GenericTypePtr type = static_pointer_cast<GenericTypePtr>(manager.getLangBasic().getJob());
	return jobExpr(type, threadNumRange, declarationStmts(localDecls), guardedExprs(branches), defaultExpr);
}

namespace {

	bool isJobBody(const NodePtr& node) {
		// it has to be an expression
		if (node->getNodeCategory() != NC_Expression) return false;

		// the function has to be ()->unit or ()=>unit
		TypePtr type = node.as<ExpressionPtr>()->getType();
		if (type->getNodeType() != NT_FunctionType) return false;
		return type.as<FunctionTypePtr>()->getParameterTypes()->empty();
	}

}

JobExprPtr IRBuilder::jobExpr(const StatementPtr& stmt, int numThreads) const {

	ExpressionPtr jobBody =
			(isJobBody(stmt))?stmt.as<ExpressionPtr>():transform::extractLambda(manager, stmt);

	return jobExpr((numThreads < 1)?getThreadNumRange(1):getThreadNumRange(numThreads, numThreads),
			vector<DeclarationStmtPtr>(), vector<GuardedExprPtr>(), jobBody);
}

MarkerExprPtr IRBuilder::markerExpr(const ExpressionPtr& subExpr, unsigned id) const {
	return markerExpr(subExpr, uintValue(id));
}

MarkerExprPtr IRBuilder::markerExpr(const ExpressionPtr& subExpr, const UIntValuePtr& id) const {
	return markerExpr(id, subExpr);
}

MarkerStmtPtr IRBuilder::markerStmt(const StatementPtr& subStmt, unsigned id) const {
	return markerStmt(subStmt, uintValue(id));
}

MarkerStmtPtr IRBuilder::markerStmt(const StatementPtr& subStmt, const UIntValuePtr& id) const {
	return markerStmt(id, subStmt);
}

CallExprPtr IRBuilder::getThreadNumRange(unsigned min) const {
	TypePtr type = manager.getLangBasic().getUInt8();
	return callExpr(manager.getLangBasic().getCreateMinRange(), literal(type, toString(min)));
}
CallExprPtr IRBuilder::getThreadNumRange(unsigned min, unsigned max) const {
	TypePtr type = manager.getLangBasic().getUInt8();
	return callExpr(manager.getLangBasic().getCreateBoundRange(), literal(type, toString(min)), literal(type, toString(max)));
}

CallExprPtr IRBuilder::getThreadNumRange(const ExpressionPtr& min) const {
	TypePtr type = manager.getLangBasic().getUInt8();
	return callExpr(manager.getLangBasic().getCreateMinRange(), castExpr(type, min));
}
CallExprPtr IRBuilder::getThreadNumRange(const ExpressionPtr& min, const ExpressionPtr& max) const {
	TypePtr type = manager.getLangBasic().getUInt8();
	return callExpr(manager.getLangBasic().getCreateBoundRange(), castExpr(type, min), castExpr(type, max));
}



CallExprPtr IRBuilder::getThreadGroup(ExpressionPtr level) const {
    if(!level) level = uintLit(0);
    return callExpr(manager.getLangBasic().getGetThreadGroup(), level);
}
CallExprPtr IRBuilder::getThreadGroupSize(ExpressionPtr level) const {
    if(!level) level = uintLit(0);
    return callExpr(manager.getLangBasic().getGetGroupSize(), level);
}
CallExprPtr IRBuilder::getThreadId(ExpressionPtr level) const {
	if(!level) level = uintLit(0);
	return callExpr(manager.getLangBasic().getGetThreadId(), level);
}

CallExprPtr IRBuilder::barrier(ExpressionPtr threadgroup) const {
	if(!threadgroup) threadgroup = getThreadGroup();
	return callExpr(manager.getLangBasic().getBarrier(), threadgroup);
}

CallExprPtr IRBuilder::mergeAll() const {
	return callExpr(manager.getLangBasic().getMergeAll());
}

CallExprPtr IRBuilder::pfor(const ExpressionPtr& body, const ExpressionPtr& start, const ExpressionPtr& end, ExpressionPtr step) const {
	if(!step) step = uintLit(1);
	assert(manager.getLangBasic().isInt(start->getType()));
	assert(manager.getLangBasic().isInt(end->getType()));
	assert(manager.getLangBasic().isInt(step->getType()));
	auto ret = callExpr(getLangBasic().getUnit(), manager.getLangBasic().getPFor(), toVector<ExpressionPtr>(getThreadGroup(), start, end, step, body));
	//LOG(INFO) <<  "%%% generated pfor:\n "<< core::printer::PrettyPrinter(ret) << "\n";
	return ret;
}

CallExprPtr IRBuilder::pfor(const ForStmtPtr& initialFor) const {
	auto loopStart = initialFor->getStart();
	auto loopEnd = initialFor->getEnd();
	auto loopStep = initialFor->getStep();
	auto loopVarType = loopStart->getType();
	
	while (loopVarType->getNodeType() == NT_RefType) {
		loopVarType = analysis::getReferencedType(loopVarType);
	}

	// modify body to take iteration variables
	auto pforLambdaParamStart = variable(loopVarType);
	auto pforLambdaParamEnd = variable(loopVarType);
	auto pforLambdaParamStep = variable(loopVarType);
	
	auto adaptedFor = forStmt(initialFor->getIterator(), pforLambdaParamStart, pforLambdaParamEnd, pforLambdaParamStep, initialFor->getBody());

	BindExprPtr lambda = transform::extractLambda(manager, adaptedFor, toVector(pforLambdaParamStart, pforLambdaParamEnd, pforLambdaParamStep));

	return pfor(lambda, loopStart, loopEnd, loopStep);
}

CallExprPtr IRBuilder::parallel(const StatementPtr& stmt, int numThreads) const {
	auto& basic = manager.getLangBasic();
	return callExpr(basic.getThreadGroup(), basic.getParallel(), jobExpr(stmt, numThreads));
}

core::ExpressionPtr IRBuilder::createCallExprFromBody(StatementPtr body, TypePtr retTy, bool lazy) const {
    // Find the variables which are used in the body and not declared
	std::vector<VariablePtr>&& args = getRechingVariables(body);

    core::TypeList argsType;
    VariableList params;
    vector<ExpressionPtr> callArgs;

    utils::map::PointerMap<VariablePtr, VariablePtr> replVariableMap;

    std::for_each(args.begin(), args.end(), [ & ] (const core::ExpressionPtr& curr) {
            assert(curr->getNodeType() == core::NT_Variable);

            const core::VariablePtr& bodyVar = curr.as<core::VariablePtr>();
            const core::TypePtr& varType = bodyVar->getType();

            // we create a new variable to replace the captured variable
            core::VariablePtr&& parmVar = this->variable( varType );
            argsType.push_back( varType );
            callArgs.push_back(curr);
            params.push_back( parmVar );

            replVariableMap.insert( std::make_pair(bodyVar, parmVar) );
        }
    );

    // Replace the variables in the body with the input parameters which have been created
    if ( !replVariableMap.empty() ) {
    	body = core::static_pointer_cast<const core::Statement>(
    			core::transform::replaceVars(manager, body, replVariableMap)
    		);
    }

    core::LambdaExprPtr&& lambdaExpr = this->lambdaExpr(functionType( argsType, retTy, true), params, wrapBody(body) );
    core::CallExprPtr&& callExpr = this->callExpr(retTy, lambdaExpr, callArgs);

    if ( !lazy ) 	return callExpr;

    // build the expression body
    return bindExpr(std::vector<VariablePtr>(), callExpr);
}

CallExprPtr IRBuilder::accessMember(const ExpressionPtr& structExpr, const string& member) const {
	return accessMember(structExpr, stringValue(member));
}

CallExprPtr IRBuilder::accessMember(const ExpressionPtr& structExpr, const StringValuePtr& member) const {
	core::TypePtr type = structExpr->getType();
	assert((type->getNodeType() == core::NT_StructType || type->getNodeType() == core::NT_UnionType) && "Cannot access non-struct type!");

	core::NamedCompositeTypePtr structType = static_pointer_cast<const core::NamedCompositeType>(type);
	core::TypePtr memberType = structType->getTypeOfMember(member);

	// create access instruction
	core::ExpressionPtr access = getLangBasic().getCompositeMemberAccess();
	return callExpr(memberType, access, structExpr, getIdentifierLiteral(member), getTypeLiteral(memberType));
}

CallExprPtr IRBuilder::refMember(const ExpressionPtr& structExpr, const string& member) const {
	return refMember(structExpr, stringValue(member));
}

CallExprPtr IRBuilder::refMember(const ExpressionPtr& structExpr, const StringValuePtr& member) const {
	core::TypePtr type = structExpr->getType();
	assert(type->getNodeType() == core::NT_RefType && "Cannot deref non ref type");

	core::TypePtr elementType = static_pointer_cast<const core::RefType>(type)->getElementType();
	assert((elementType->getNodeType() == core::NT_StructType || elementType->getNodeType() == core::NT_UnionType) && "Cannot access non-struct type!");

	core::NamedCompositeTypePtr structType = static_pointer_cast<const core::NamedCompositeType>(elementType);
	core::TypePtr memberType = structType->getTypeOfMember(member);

	// create access instruction
	core::ExpressionPtr access = getLangBasic().getCompositeRefElem();
	return callExpr(refType(memberType), access, structExpr, getIdentifierLiteral(member), getTypeLiteral(memberType));
}

CallExprPtr IRBuilder::accessComponent(ExpressionPtr tupleExpr, ExpressionPtr component) const {
	unsigned idx = extractNumberFromExpression(component);
	return accessComponent(tupleExpr, idx);
}

CallExprPtr IRBuilder::accessComponent(ExpressionPtr tupleExpr, unsigned component) const {
	core::TypePtr type = tupleExpr->getType();
	assert(type->getNodeType() == core::NT_TupleType && "Cannot access non-tuple type!");

	core::TupleTypePtr tupleType = static_pointer_cast<const core::TupleType>(type);
	assert(component < tupleType->getElementTypes().size() && "Component out of range!");
	core::TypePtr componentType = tupleType->getElementTypes()[component];

	// create access instruction
	core::ExpressionPtr access = getLangBasic().getTupleMemberAccess();
	core::ExpressionPtr index = literal(getLangBasic().getUInt8(), utils::numeric_cast<string>(component));
	core::ExpressionPtr typeLiteral = getTypeLiteral(componentType);
	return callExpr(componentType, access, tupleExpr, index, typeLiteral);
}

CallExprPtr IRBuilder::refComponent(ExpressionPtr tupleExpr, ExpressionPtr component) const {
	unsigned idx = extractNumberFromExpression(component);
	return refComponent(tupleExpr, idx);
}
CallExprPtr IRBuilder::refComponent(ExpressionPtr tupleExpr, unsigned component) const {
	core::TypePtr type = tupleExpr->getType();
	assert(type->getNodeType() == core::NT_RefType && "Cannot deref non ref type");

	core::TypePtr elementType = static_pointer_cast<const core::RefType>(type)->getElementType();
	assert(elementType->getNodeType() == core::NT_TupleType && "Cannot access non-tuple type!");

	core::TupleTypePtr tupleType = static_pointer_cast<const core::TupleType>(elementType);
	assert(component < tupleType->getElementTypes().size() && "Component out of range!");
	core::TypePtr componentType = tupleType->getElementTypes()[component];

	// create access instruction
	core::ExpressionPtr access = getLangBasic().getTupleRefElem();
	core::ExpressionPtr index = literal(getLangBasic().getUInt8(), utils::numeric_cast<string>(component));
	core::ExpressionPtr typeLiteral = getTypeLiteral(componentType);
	return callExpr(refType(componentType), access, tupleExpr, index, typeLiteral);
}


CompoundStmtPtr IRBuilder::getNoOp() const {
	return compoundStmt();
}

bool IRBuilder::isNoOp(const NodePtr& p) const {
	return *p == *getNoOp();
}

LiteralPtr IRBuilder::getIntParamLiteral(unsigned value) const {
	IntTypeParamPtr intTypeParam = concreteIntTypeParam(value);
	return getIntTypeParamLiteral(intTypeParam);
}

LiteralPtr IRBuilder::getIntTypeParamLiteral(const IntTypeParamPtr& param) const {
	auto type = genericType("intTypeParam", TypeList(), toVector(param));
	return literal(type, toString(*param));
}

LiteralPtr IRBuilder::getTypeLiteral(const TypePtr& type) const {
	auto literalType = genericType("type", toVector(type));
	return literal(literalType, toString(*type));
}

LiteralPtr IRBuilder::getIdentifierLiteral(const string& value) const {
	return getIdentifierLiteral(stringValue(value));
}

LiteralPtr IRBuilder::getIdentifierLiteral(const core::StringValuePtr& value) const {
	return literal(getLangBasic().getIdentifier(), value);
}

ExpressionPtr IRBuilder::scalarToVector( const TypePtr& type, const ExpressionPtr& subExpr) const {

	// if it is alread a vector => done
	if (subExpr->getType()->getNodeType() == NT_VectorType) {
		return subExpr;
	}

	assert(getLangBasic().isScalarType(subExpr->getType()) && "Requested to convert non-scalar to scalar!");
	assert(type->getNodeType() == NT_VectorType && "Target type has to be a vector type!");


	VectorTypePtr vt = type.as<VectorTypePtr>();

	// Convert casts form scalars to vectors to vector init exrpessions
	// get vector element type without ref
	core::TypePtr elementType = vt->getElementType();
	core::TypePtr targetType = elementType;// refs in arrays have been removed! (elementType->getNodeType() != core::NT_RefType) ?  vt->getElementType() :
			//dynamic_pointer_cast<const core::RefType>(elementType)->getElementType();

	core::ExpressionPtr arg = (subExpr->getType() == targetType) ? subExpr :
		castExpr(targetType, subExpr); // if the type of the sub expression is not equal the target type we need to cast it

	core::ExpressionPtr&& retExpr = callExpr(type, getLangBasic().getVectorInitUniform(),
		(elementType->getNodeType() == core::NT_RefType && arg->getNodeType() != core::NT_RefType)  ? refVar( arg ) : arg,// if we need a ref type and arg is no ref: add ref
		getIntTypeParamLiteral(vt->getSize()));

	return retExpr;


//    // check for casts from salar pointers to vector pointers
//    if(core::ArrayTypePtr&& array = dynamic_pointer_cast<const core::ArrayType>(type)) {
////        core::RefTypePtr&& refType = dynamic_pointer_cast<const core::RefType>(array->getElementType());
//        core::VectorTypePtr&& vt = dynamic_pointer_cast<const core::VectorType>(array->getElementType());
//        core::ArrayTypePtr&& castedArray = dynamic_pointer_cast<const core::ArrayType>(subExpr->getType());
//        if(castedArray && vt ){
//            core::TypePtr elemTy = /*castedArray->getElementType()->getNodeType() == core::NodeType::NT_RefType ?
//                    dynamic_pointer_cast<const core::RefType>(castedArray->getElementType())->getElementType() :*/ castedArray->getElementType();
//
//            if(elemTy) {
//                // check if they have the same type
//                assert(elemTy == vt->getElementType() && "cast from array to array of vectors only allowed within the same type");
//
//                return  callExpr(getLangBasic().getArrayElemToVec(), subExpr, getIntTypeParamLiteral(vt->getSize()));
//            }
//        }
//    }

//    // expression is either already a vector/array type or the type is not a vector type
//    return subExpr;
}


// ------------------------ Operators ---------------------------

namespace {
	template<typename ... T>
	TypePtr infereExprTypeInternal(const ExpressionPtr& op, const T& ... operators) {
		assert(op->getType()->getNodeType() == NT_FunctionType && "Operation is not a function!");
		FunctionTypePtr funType = static_pointer_cast<FunctionTypePtr>(op->getType());
		return tryDeduceReturnType(funType, extractTypes(toVector(operators ...)));
	}
}

TypePtr IRBuilder::infereExprType(const ExpressionPtr& op, const ExpressionPtr& a) const {
	return infereExprTypeInternal(op, a);
}

TypePtr IRBuilder::infereExprType(const ExpressionPtr& op, const ExpressionPtr& a, const ExpressionPtr& b) const {
	return infereExprTypeInternal(op, a, b);
}

TypePtr IRBuilder::infereExprType(const ExpressionPtr& op, const ExpressionPtr& a, const ExpressionPtr& b, const ExpressionPtr& c) const {
	return infereExprTypeInternal(op, a, b, c);
}


namespace {

	GenericTypePtr toSigned(const IRBuilder& builder, const GenericTypePtr& type) {
		assert(builder.getLangBasic().isScalarType(type) && "Can not alter non-scalar type to signed!");

		// check whether a modification is required
		if (builder.getLangBasic().isUnsignedInt(type)) {
			// alter to signed alternative
			return builder.genericType("int", TypeList(), toVector(type->getIntTypeParameter()->getElement(0)));
		}
		return type;
	}
}


LiteralPtr IRBuilder::minus(const LiteralPtr& lit) const {
	assert(getLangBasic().isScalarType(lit->getType()) && "Can not change sign of non-scalar type!");

	// update type of literal to support unsigned
	TypePtr type = toSigned(*this, lit->getType().as<GenericTypePtr>());

	// update string value of literal
	string value = lit->getStringValue();
	assert(value.size() > 0u);
	if (value[0] == '-') {
		value = value.substr(1);
	} else {
		value = "-" + value;
	}

	// create resulting literal
	return literal(value, type);
}

ExpressionPtr IRBuilder::minus(const ExpressionPtr& a) const {
	assert(getLangBasic().isScalarType(a->getType()) && "Can not change sign of non-scalar type!");

	// check literal type
	if (a->getNodeType() == NT_Literal) {
		return minus(a.as<LiteralPtr>());
	}

	// update type of literal to support unsigned
	TypePtr type = toSigned(*this, a->getType().as<GenericTypePtr>());

	ExpressionPtr value = a;
	if (value->getType() != type) {
		value = castExpr(type, value);
	}

	// return 0 - a
	return sub(getZero(type), a);
}


// ---------------------------- Utilities ---------------------------------------


unsigned IRBuilder::extractNumberFromExpression(ExpressionPtr& expr) const {

	unsigned idx = 0;
	// search for the literal in the second argument
	auto lambdaVisitor = makeLambdaVisitor([&idx, this](const NodePtr& node)->bool {
		// check for literal, assuming it will always be a valid integer
		if(const LiteralPtr& lit = dynamic_pointer_cast<const Literal>(node)) {
			if(getLangBasic().isInt(lit->getType())) {
				idx = atoi(lit->getValue()->getValue().c_str());
				return true;
			}
		}
		return false;
	});

	if(!visitDepthFirstInterruptible(expr, lambdaVisitor)){
		LOG(ERROR) << expr;
		assert(false && "Expression does not contain a literal a number");
	}

	return idx;
}


/**
 * A utility function wrapping a given statement into a compound statement (if necessary).
 */
CompoundStmtPtr IRBuilder::wrapBody(const StatementPtr& stmt) const {
	if (stmt->getNodeType() == NT_CompoundStmt) {
		return static_pointer_cast<CompoundStmtPtr>(stmt);
	}
	return CompoundStmt::get(stmt->getNodeManager(), stmt);
}

ExpressionPtr IRBuilder::wrapLazy(const ExpressionPtr& expr) const {

	// if it is a expression, bind free variables
	VariableList list = analysis::getFreeVariables(expr);
	std::sort(list.begin(), list.end(), compare_target<VariablePtr>());
	ExpressionPtr res = lambdaExpr(expr->getType(),returnStmt(expr), list);

	// if there are no free variables ...
	if (list.empty()) {
		// ... no capturing is necessary
		return res;
	}

	// otherwise: bind the free variables
	return bindExpr(VariableList(), callExpr(expr->getType(), res, convertList<Expression>(list)));
}

CallExprPtr IRBuilder::print(const string& format, const ExpressionList& args) const {
	return print(stringLit(format), args);
}

CallExprPtr IRBuilder::print(const ExpressionPtr& format, const ExpressionList& args) const {
	auto& basic = getLangBasic();
	return callExpr(basic.getUnit(), basic.getPrint(), format, pack(args));
}

CallExprPtr IRBuilder::pack(const ExpressionList& values) const {
	auto& basic = getLangBasic();
	return callExpr(basic.getVarList(), basic.getVarlistPack(), tupleExpr(values));
}

CallExprPtr IRBuilder::select(const ExpressionPtr& a, const ExpressionPtr& b, const ExpressionPtr& op) const {
	const auto& basic = manager.getLangBasic();
	const core::LiteralPtr& select = basic.getSelect();
	return callExpr(infereExprType(select, a, b, op), select, a, b, op);
}

CallExprPtr IRBuilder::select(const ExpressionPtr& a, const ExpressionPtr& b, lang::BasicGenerator::Operator op) const {
	return select(a, b, manager.getLangBasic().getOperator(a->getType(), op));
}

// helper for the pointwise operation
CallExprPtr IRBuilder::pointwise(const ExpressionPtr& callee) const {
	const FunctionTypePtr funTy = dynamic_pointer_cast<const FunctionType>(callee->getType());
	assert(funTy && "The argument of pointwise must be a function");

	TypeList paramTys = funTy->getParameterTypeList();

	assert(paramTys.size() <= 2 && paramTys.size() > 0  && "The function for pointwise must take one or two arguments");

	FunctionTypePtr pointwiseTy;
	ExpressionPtr pointwise;
	const auto& basic = manager.getLangBasic();
	if(paramTys.size() == 1) { // unary function
		TypePtr newParamTy = vectorType(paramTys.at(0), variableIntTypeParam('l'));
		pointwiseTy = functionType(toVector(newParamTy), vectorType(funTy->getReturnType(), variableIntTypeParam('l')));
		pointwise =  basic.getVectorPointwiseUnary();
	} else { // binary functon
		TypePtr newParamTy1 = vectorType(paramTys.at(1), variableIntTypeParam('l'));
		TypePtr newParamTy2 = vectorType(paramTys.at(0), variableIntTypeParam('l'));
		pointwiseTy = functionType(toVector(newParamTy1, newParamTy2), vectorType(funTy->getReturnType(), variableIntTypeParam('l')));

		pointwiseTy = functionType(toVector(newParamTy1, newParamTy2), vectorType(funTy->getReturnType(), variableIntTypeParam('l')));
		pointwise =  basic.getVectorPointwise();
	}
	return callExpr(pointwiseTy, pointwise, callee);
}

// helper for accuraccy functions
CallExprPtr IRBuilder::accuracyHigh(const ExpressionPtr& callee) const {
	const FunctionTypePtr funTy = dynamic_pointer_cast<const FunctionType>(callee->getType());
	assert(funTy && "The argument of accuraccy high must be a function");
	int nArgs = funTy->getParameterTypeList().size();
	assert(nArgs <= 2 && nArgs > 0  && "The function for accuraccy high must take one or two arguments");

	const auto& basic = manager.getLangBasic();
	return nArgs == 1 ?
            callExpr(funTy, basic.getAccuracyFastUnary(), callee) :
            callExpr(funTy, basic.getAccuracyFastBinary(), callee);
}
CallExprPtr IRBuilder::accuracyBestEffort(const ExpressionPtr& callee) const {
	const FunctionTypePtr funTy = dynamic_pointer_cast<const FunctionType>(callee->getType());
	assert(funTy && "The argument of accuraccy best effort must be a function");
	int nArgs = funTy->getParameterTypeList().size();
	assert(nArgs <= 2 && nArgs > 0  && "The function for accuraccy best effort must take one or two arguments");

	const auto& basic = manager.getLangBasic();
	return nArgs == 1 ?
            callExpr(funTy, basic.getAccuracyBestEffortUnary(), callee) :
            callExpr(funTy, basic.getAccuracyBestEffortBinary(), callee);
}
CallExprPtr IRBuilder::accuracyFast(const ExpressionPtr& callee) const {
	const FunctionTypePtr funTy = dynamic_pointer_cast<const FunctionType>(callee->getType());
	assert(funTy && "The argument of accuraccy fast must be a function");
	int nArgs = funTy->getParameterTypeList().size();
	assert(nArgs <= 2 && nArgs > 0  && "The function for accuraccy fast must take one or two arguments");

	const auto& basic = manager.getLangBasic();
	return nArgs == 1 ?
            callExpr(funTy, basic.getAccuracyFastUnary(), callee) :
            callExpr(funTy, basic.getAccuracyFastBinary(), callee);
}

CallExprPtr IRBuilder::vectorPermute(const ExpressionPtr& dataVec, const ExpressionPtr& permutationVec) const {
	const VectorTypePtr dataType = dynamic_pointer_cast<const VectorType>(dataVec->getType());
	assert(dataType && "First argument of vector.permute must be a vector");

	const auto& basic = manager.getLangBasic();
	const VectorTypePtr permuteType = dynamic_pointer_cast<const VectorType>(permutationVec->getType());
	assert(permuteType && "Secont argument of vector.permute must be a vector");
	assert(isSubTypeOf(permuteType->getElementType(), basic.getUIntInf()) && "The stecond argument of vector.permute must be of type vector<uint<#a>,#m>");

	const TypePtr retTy = vectorType(dataType->getElementType(), permuteType->getSize());

	return callExpr(retTy, basic.getVectorPermute(), dataVec, permutationVec);
}

} // namespace core
} // namespace insieme
