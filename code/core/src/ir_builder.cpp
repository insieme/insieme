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

#include "insieme/core/ir_node.h"

#include "insieme/core/ir_values.h"
#include "insieme/core/ir_int_type_param.h"
#include "insieme/core/ir_types.h"
#include "insieme/core/ir_expressions.h"
#include "insieme/core/ir_statements.h"
#include "insieme/core/ir_program.h"

#include "insieme/core/type_utils.h"
#include "insieme/core/transform/manipulation.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/core/analysis/ir_utils.h"

#include "insieme/utils/map_utils.h"
#include "insieme/utils/logging.h"

namespace insieme {
namespace core {

	const lang::BasicGenerator& IRBuilder::getBasicGenerator() const {
			return manager.getBasicGenerator();
	}

namespace {

	typedef boost::tuple<vector<VariablePtr>, vector<ExpressionPtr>> InitDetails;

	InitDetails splitUp(const ASTBuilder::CaptureInits& captureInits) {

		// prepare containers
		InitDetails res;
		vector<VariablePtr>& vars = res.get<0>();
		vector<ExpressionPtr>& inits = res.get<1>();

		// process the given map
		for_each(captureInits, [&](const ASTBuilder::CaptureInits::value_type& cur) {
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
	struct VarRefFinder: public ASTVisitor<bool> {

	    VarRefFinder() : core::ASTVisitor<bool>(false) { }

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

	utils::set::PointerSet<VariablePtr> getRechingVariables(const core::NodePtr& root) {
		VarRefFinder visitor;
		visitDepthFirstPrunable(root, visitor);

		utils::set::PointerSet<VariablePtr> nonDecls;
		std::set_difference( visitor.usedVars.begin(), visitor.usedVars.end(),
				visitor.declaredVars.begin(), visitor.declaredVars.end(), std::inserter(nonDecls, nonDecls.begin()));

		return nonDecls;
	}

}


NodePtr ASTBuilder::get(NodeType type, const NodeList& children) const {

	switch(type) {
	#define CONCRETE(KIND) \
		case NT_ ## KIND : return get< NT_## KIND >(children);
	#include "insieme/core/ast_nodes.def"
	#undef CONCRETE
	}

	assert(false && "Unsupported node type added!");
	return NodePtr();
}

ProgramPtr ASTBuilder::createProgram(const Program::EntryPointList& entryPoints, bool main) {
	return Program::create(manager, entryPoints, main);
}

// ---------------------------- Convenience -------------------------------------

FunctionTypePtr ASTBuilder::toPlainFunctionType(const FunctionTypePtr& funType) const {
	if (funType->isPlain()) {
		return funType;
	}
	return functionType(funType->getParameterTypes(), funType->getReturnType(), true);
}

FunctionTypePtr ASTBuilder::toThickFunctionType(const FunctionTypePtr& funType) const {
	if (!funType->isPlain()) {
		return funType;
	}
	return functionType(funType->getParameterTypes(), funType->getReturnType(), false);
}


LiteralPtr ASTBuilder::stringLit(const string& str) const {
	return literal(str, manager.basic.getString());
}

LiteralPtr ASTBuilder::intLit(const int val) const {
    return literal(manager.basic.getInt4(), toString(val));
}
LiteralPtr ASTBuilder::uintLit(const unsigned int val) const {
    return literal(manager.basic.getUInt4(), toString(val));
}


core::ExpressionPtr ASTBuilder::getZero(const core::TypePtr& type) const {

	// if it is an integer ...
	if (manager.basic.isInt(type)) {
		return core::Literal::get(manager, type, "0");
	}

	// if it is a real ..
	if (manager.basic.isReal(type)) {
		return core::Literal::get(manager, type, "0.0");
	}

	// if it is a struct ...
	if (type->getNodeType() == core::NT_StructType) {

		// extract type and resolve members recursively
		core::StructTypePtr structType = static_pointer_cast<const core::StructType>(type);

		core::StructExpr::Members members;
		for_each(structType->getEntries(), [&](const core::StructType::Entry& cur) {
			members.push_back(std::make_pair(cur.first, getZero(cur.second)));
		});

		return core::StructExpr::get(manager, members);
	}

	// if it is a ref type ...
	if (type->getNodeType() == core::NT_RefType) {
		// return the corresponding flavor of NULL
		core::TypePtr elementType = core::analysis::getReferencedType(type);
		return callExpr(type, manager.basic.getAnyRefToRef(), manager.basic.getNull(), manager.basic.getTypeLiteral(elementType));
	}

	// TODO: extend for more types
	LOG(FATAL) << "Encountered unsupported type: " << *type;
	assert(false && "Given type not supported yet!");

	// fall-back => no default initialization possible
	return callExpr(type, manager.basic.getInitZero(), manager.basic.getTypeLiteral(type));
}


CallExprPtr ASTBuilder::deref(const ExpressionPtr& subExpr) const {
	RefTypePtr&& refTy = dynamic_pointer_cast<const RefType>(subExpr->getType());
	assert(refTy && "Deref a non ref type.");
	return callExpr(refTy->getElementType(), manager.basic.getRefDeref(), subExpr);
}

CallExprPtr ASTBuilder::refVar(const ExpressionPtr& subExpr) const {
	return callExpr(refType(subExpr->getType()), manager.basic.getRefVar(), subExpr);
}

CallExprPtr ASTBuilder::refNew(const ExpressionPtr& subExpr) const {
	return callExpr(refType(subExpr->getType()), manager.basic.getRefNew(), subExpr);
}

CallExprPtr ASTBuilder::assign(const ExpressionPtr& target, const ExpressionPtr& value) const {
	return callExpr(manager.basic.getUnit(), manager.basic.getRefAssign(), target, value);
}

ExpressionPtr ASTBuilder::invertSign(const ExpressionPtr& subExpr) const {
    // add a vector init expression if subExpr is of vector type
    ExpressionPtr&& elem = dynamic_pointer_cast<const VectorType>(subExpr->getType()) ?
	    manager.basic.scalarToVector(subExpr->getType(), intLit(0)) : castExpr(subExpr->getType(), intLit(0));

	return callExpr(
			subExpr->getType(), manager.basic.getOperator(subExpr->getType(), lang::BasicGenerator::Sub),
			elem, subExpr
		);
}

ExpressionPtr ASTBuilder::negateExpr(const ExpressionPtr& boolExpr) const {
	assert( manager.basic.isBool(boolExpr->getType()) && "Cannot negate a non boolean expression.");
	return callExpr(manager.basic.getBool(), manager.basic.getBoolLNot(), boolExpr);
}


CallExprPtr ASTBuilder::vectorSubscript(const ExpressionPtr& vec, const ExpressionPtr& index) const {
	auto vType = dynamic_pointer_cast<const VectorType>(vec->getType());
	assert(vType && "Tried vector subscript operation on non-vector expression");
	return callExpr(vType->getElementType(), manager.basic.getVectorSubscript(), vec, index);
}
//CallExprPtr ASTBuilder::vectorSubscript(const ExpressionPtr& vec, unsigned index) const {
//	auto lit = uintLit(index);
//	vectorSubscript(vec, lit);
//}

CompoundStmtPtr ASTBuilder::compoundStmt(const StatementPtr& s1, const StatementPtr& s2) const {
	return compoundStmt(toVector(s1, s2));
}
CompoundStmtPtr ASTBuilder::compoundStmt(const StatementPtr& s1, const StatementPtr& s2, const StatementPtr& s3) const {
	return compoundStmt(toVector(s1, s2, s3));
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


	CallExprPtr createCall(const ASTBuilder& builder, const TypePtr& resultType, const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) {

		// check user-specified return type - only when compiled in debug mode
		// NOTE: the check returns true in any case, hence this assertion will never fail - its just a warning!
		// TODO: make this check faster
//		assert(checkType(resultType, functionExpr, arguments) && "Incorrect user-specified return type!");

		// create calling expression
		return builder.callExpr(resultType, functionExpr, arguments);
	}
}

CallExprPtr ASTBuilder::callExpr(const ExpressionPtr& functionExpr, const vector<ExpressionPtr>& arguments) const {
	// use deduced return type to construct call
	return callExpr(deduceReturnTypeForCall(functionExpr, arguments), functionExpr, arguments);
}
CallExprPtr ASTBuilder::callExpr(const ExpressionPtr& functionExpr, const ExpressionPtr& arg1) const {
	return callExpr(functionExpr, toVector(arg1));
}
CallExprPtr ASTBuilder::callExpr(const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2) const {
	return callExpr(functionExpr, toVector(arg1, arg2));
}
CallExprPtr ASTBuilder::callExpr(const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2, const ExpressionPtr& arg3) const {
	return callExpr(functionExpr, toVector(arg1, arg2, arg3));
}
CallExprPtr ASTBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr) const {
	return createCall(*this, resultType, functionExpr, toVector<ExpressionPtr>());
}
CallExprPtr ASTBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1) const {
	return createCall(*this, resultType, functionExpr, toVector(arg1));
}
CallExprPtr ASTBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2) const {
	return createCall(*this, resultType, functionExpr, toVector(arg1, arg2));
}
CallExprPtr ASTBuilder::callExpr(const TypePtr& resultType, const ExpressionPtr& functionExpr, const ExpressionPtr& arg1, const ExpressionPtr& arg2, const ExpressionPtr& arg3) const {
	return createCall(*this, resultType, functionExpr, toVector(arg1, arg2, arg3));
}

LambdaExprPtr ASTBuilder::lambdaExpr(const StatementPtr& body, const ParamList& params) const {
	return lambdaExpr(functionType(extractParamTypes(params), manager.basic.getUnit(), true), params, body);
}
LambdaExprPtr ASTBuilder::lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const ParamList& params) const {
	return lambdaExpr(functionType(extractParamTypes(params), returnType, true), params, body);
}


BindExprPtr ASTBuilder::lambdaExpr(const StatementPtr& body, const CaptureInits& captureMap, const ParamList& params) const {
	return lambdaExpr(manager.basic.getUnit(), body, captureMap, params);
}

BindExprPtr ASTBuilder::lambdaExpr(const TypePtr& returnType, const StatementPtr& body, const CaptureInits& captureMap, const ParamList& params) const {

	// process capture map
	InitDetails&& details = splitUp(captureMap);

	vector<VariablePtr>& captureVars = details.get<0>();
	vector<ExpressionPtr>& values = details.get<1>();

	// get list of parameters within inner function
	ParamList parameter;
	parameter.insert(parameter.end(), captureVars.begin(), captureVars.end());
	parameter.insert(parameter.end(), params.begin(), params.end());

	// build function type
	FunctionTypePtr funType = functionType(extractParamTypes(parameter), returnType, true);

	// build inner function
	LambdaExprPtr lambda = lambdaExpr(funType, parameter, body);


	// construct argument list for call expression within bind
	vector<ExpressionPtr> args;
	args.insert(args.end(), values.begin(), values.end());
	args.insert(args.end(), params.begin(), params.end());

	// construct bind expression around lambda
	CallExprPtr call = callExpr(returnType, lambda, args);
	return bindExpr(params, call);
}

CallExprPtr ASTBuilder::getThreadNumRange(unsigned min) const {
	TypePtr type = manager.basic.getUInt8();
	return callExpr(manager.basic.getCreateMinRange(), literal(type, toString(min)));
}

CallExprPtr ASTBuilder::getThreadNumRange(unsigned min, unsigned max) const {
	TypePtr type = manager.basic.getUInt8();
	return callExpr(manager.basic.getCreateBoundRange(), literal(type, toString(min)), literal(type, toString(max)));
}



CallExprPtr ASTBuilder::getThreadGroup(ExpressionPtr level) const {
    if(!level) level = uintLit(0);
    return callExpr(manager.basic.getGetThreadGroup(), level);
}
CallExprPtr ASTBuilder::getThreadId(ExpressionPtr level) const {
	if(!level) level = uintLit(0);
	return callExpr(manager.basic.getGetThreadId(), level);
}

CallExprPtr ASTBuilder::barrier(ExpressionPtr threadgroup) const {
	if(!threadgroup) threadgroup = getThreadGroup();
	return callExpr(manager.basic.getBarrier(), threadgroup);
}

CallExprPtr ASTBuilder::pfor(const ExpressionPtr& body, const ExpressionPtr& start, const ExpressionPtr& end, ExpressionPtr step) const {
	if(!step) step = uintLit(1);
	assert(manager.basic.isInt(start->getType()));
	assert(manager.basic.isInt(end->getType()));
	assert(manager.basic.isInt(step->getType()));
	return callExpr(manager.basic.getPFor(), toVector<ExpressionPtr>(getThreadGroup(), start, end, step, body));
}

CallExprPtr ASTBuilder::pfor(const ForStmtPtr& initialFor) const {
	auto decl = initialFor->getDeclaration();
	auto forBody = initialFor->getBody();
	auto loopvar = decl->getVariable();

	auto loopVarType = loopvar->getType();
	while (loopVarType->getNodeType() == NT_RefType) {
		loopVarType = static_pointer_cast<const RefType>(loopVarType)->getElementType();
	}

	// modify body to take iteration variable
	auto pforLambdaParam = variable(loopVarType);

	insieme::utils::map::PointerMap<NodePtr, NodePtr> modifications;
	modifications.insert(std::make_pair(loopvar, pforLambdaParam));
//	modifications.insert(std::make_pair(loopvar, pforLambdaParam));
	auto adaptedBody = static_pointer_cast<const Statement>(transform::replaceAll(manager, forBody, modifications));

	BindExprPtr lambda = transform::extractLambda(manager, adaptedBody, toVector(pforLambdaParam));
	//LOG(INFO) << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" << lambda->getValues() 
	//	<< "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n" << pforLambdaParam << "\n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
	auto initExp = decl->getInitialization();

	while (analysis::isCallOf(initExp, manager.basic.getRefVar()) || analysis::isCallOf(initExp, manager.basic.getRefNew())) {
		initExp = static_pointer_cast<const CallExpr>(initExp)->getArguments()[0];
	}

	while (initExp->getType()->getNodeType() == NT_RefType) {
		initExp = deref(initExp);
	}

	return pfor(lambda, initExp, initialFor->getEnd(), initialFor->getStep());
}

core::ExpressionPtr ASTBuilder::createCallExprFromBody(StatementPtr body, TypePtr retTy, bool lazy) const {
    // Find the variables which are used in the body and not declared
	utils::set::PointerSet<VariablePtr>&& args = getRechingVariables(body);

    core::TypeList argsType;
    core::Lambda::ParamList params;
    vector<ExpressionPtr> callArgs;

    utils::map::PointerMap<VariablePtr, VariablePtr> replVariableMap;

    std::for_each(args.begin(), args.end(), [ & ] (const core::ExpressionPtr& curr) {
            assert(curr->getNodeType() == core::NT_Variable);

            const core::VariablePtr& bodyVar = core::static_pointer_cast<const core::Variable>(curr);
            core::TypePtr&& varType = bodyVar->getType();

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

    core::LambdaExprPtr&& lambdaExpr = this->lambdaExpr(functionType( argsType, retTy, true), params, body );
    core::CallExprPtr&& callExpr = this->callExpr(retTy, lambdaExpr, callArgs);

    if ( !lazy ) 	return callExpr;

    // build the expression body
    return bindExpr(std::vector<VariablePtr>(), callExpr);
}

ExpressionPtr ASTBuilder::accessMember(ExpressionPtr structExpr, string member) const {
	return accessMember(structExpr, identifier(member));
}

ExpressionPtr ASTBuilder::accessMember(ExpressionPtr structExpr, IdentifierPtr member) const {
	core::TypePtr type = structExpr->getType();
	assert(type->getNodeType() == core::NT_StructType && "Cannot access non-struct type!");

	core::StructTypePtr structType = static_pointer_cast<const core::StructType>(type);
	core::TypePtr memberType = structType->getTypeOfMember(member);

	// create access instruction
	core::ExpressionPtr access = getBasicGenerator().getCompositeMemberAccess();
	return callExpr(memberType, access, structExpr, getBasicGenerator().getIdentifierLiteral(member), getBasicGenerator().getTypeLiteral(memberType));
}

ExpressionPtr ASTBuilder::refMember(ExpressionPtr structExpr, string member) const {
	return refMember(structExpr, identifier(member));
}

ExpressionPtr ASTBuilder::refMember(ExpressionPtr structExpr, IdentifierPtr member) const {
	core::TypePtr type = structExpr->getType();
	assert(type->getNodeType() == core::NT_RefType && "Cannot deref non ref type");

	core::TypePtr elementType = static_pointer_cast<const core::RefType>(type)->getElementType();
	assert(elementType->getNodeType() == core::NT_StructType && "Cannot access non-struct type!");

	core::StructTypePtr structType = static_pointer_cast<const core::StructType>(elementType);
	core::TypePtr memberType = structType->getTypeOfMember(member);

	// create access instruction
	core::ExpressionPtr access = getBasicGenerator().getCompositeRefElem();
	return callExpr(refType(memberType), access, structExpr, getBasicGenerator().getIdentifierLiteral(member), getBasicGenerator().getTypeLiteral(memberType));
}

ExpressionPtr ASTBuilder::accessComponent(ExpressionPtr tupleExpr, ExpressionPtr component) const {
	unsigned idx = extractNumberFromExpression(component);
	return accessComponent(tupleExpr, idx);
}

ExpressionPtr ASTBuilder::accessComponent(ExpressionPtr tupleExpr, unsigned component) const {
	core::TypePtr type = tupleExpr->getType();
	assert(type->getNodeType() == core::NT_TupleType && "Cannot access non-tuple type!");

	core::TupleTypePtr tupleType = static_pointer_cast<const core::TupleType>(type);
	assert(component < tupleType->getElementTypes().size() && "Component out of range!");
	core::TypePtr componentType = tupleType->getElementTypes()[component];

	// create access instruction
	core::ExpressionPtr access = getBasicGenerator().getTupleMemberAccess();
	core::ExpressionPtr index = literal(getBasicGenerator().getUInt8(), utils::numeric_cast<string>(component));
	core::ExpressionPtr typeLiteral = getBasicGenerator().getTypeLiteral(componentType);
	return callExpr(componentType, access, tupleExpr, index, typeLiteral);
}

ExpressionPtr ASTBuilder::refComponent(ExpressionPtr tupleExpr, ExpressionPtr component) const {
	unsigned idx = extractNumberFromExpression(component);
	return refComponent(tupleExpr, idx);
}
ExpressionPtr ASTBuilder::refComponent(ExpressionPtr tupleExpr, unsigned component) const {
	core::TypePtr type = tupleExpr->getType();
	assert(type->getNodeType() == core::NT_RefType && "Cannot deref non ref type");

	core::TypePtr elementType = static_pointer_cast<const core::RefType>(type)->getElementType();
	assert(elementType->getNodeType() == core::NT_TupleType && "Cannot access non-tuple type!");

	core::TupleTypePtr tupleType = static_pointer_cast<const core::TupleType>(elementType);
	assert(component < tupleType->getElementTypes().size() && "Component out of range!");
	core::TypePtr componentType = tupleType->getElementTypes()[component];

	// create access instruction
	core::ExpressionPtr access = getBasicGenerator().getTupleRefElem();
	core::ExpressionPtr index = literal(getBasicGenerator().getUInt8(), utils::numeric_cast<string>(component));
	core::ExpressionPtr typeLiteral = getBasicGenerator().getTypeLiteral(componentType);
	return callExpr(refType(componentType), access, tupleExpr, index, typeLiteral);
}


// ---------------------------- Utilities ---------------------------------------

ASTBuilder::TypeList ASTBuilder::extractParamTypes(const ParamList& params) {
	TypeList paramTypes;
	std::transform(params.cbegin(), params.cend(), std::back_inserter(paramTypes),
		[](const VariablePtr& p) { return p->getType(); });
	return paramTypes;
}

unsigned ASTBuilder::extractNumberFromExpression(ExpressionPtr& expr) const {

	unsigned idx = 0;
	// search for the literal in the second argument
	auto lambdaVisitor = makeLambdaVisitor([&idx, this](const NodePtr& node)->bool {
		// check for literal, assuming it will always be a valid integer
		if(const LiteralPtr& lit = dynamic_pointer_cast<const Literal>(node)) {
			if(getBasicGenerator().isInt(lit->getType())) {
				idx = atoi(lit->getValue().c_str());
				return true;
			}
		}
		return false;
	});

	if(!visitDepthFirstInterruptable(expr, lambdaVisitor)){
		LOG(ERROR) << expr;
		assert(false && "Expression does not contain a literal a number");
	}

	return idx;
}

#include "ast_builder_impl.inl"

} // namespace core
} // namespace insieme
