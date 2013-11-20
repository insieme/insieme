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

#include "insieme/analysis/defuse_collect.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/ir_address.h"

#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/analysis/ir++_utils.h"
#include "insieme/core/lang/basic.h"
#include "insieme/core/lang/ir++_extension.h"

#include "insieme/utils/logging.h"
#include "insieme/utils/string_utils.h"
#include "insieme/utils/unused.h"
#include "insieme/utils/assert.h"

#include <stack>

#define AS_EXPR_ADDR(addr) 		core::static_address_cast<const core::Expression>(addr)
#define AS_CALLEXPR_ADDR(addr) 	core::static_address_cast<const core::CallExpr>(addr)
#define AS_VAR_ADDR(addr) 		core::static_address_cast<const core::Variable>(addr)

namespace insieme {
namespace analysis {

std::string Ref::useTypeToStr(const UseType& usage) {
	switch (usage) {
	case Ref::DEF: 		return "DEF"; 
	case Ref::USE:		return "USE"; 
	case Ref::UNKNOWN:	return "UNKNOWN"; 
	default:			assert(false);
	}
	return "-unknown-";
}

std::string Ref::refTypeToStr(const RefType& type) {
	switch(type) {
	case Ref::SCALAR:	return "SCALAR"; 
	case Ref::ARRAY:	return "ARRAY"; 
	case Ref::MEMBER:	return "MEMBER"; 
	case Ref::CALL: 	return "CALL"; 
	default:  			assert(false);
	}
	return "-unknown-";
}

//===== Ref =========================================================================================

Ref::Ref(const RefType& type, const core::ExpressionAddress& var, const UseType& usage) : 
	 baseExpr(var), type(type), usage(usage) 
{ 
	assert(var->getType()->getNodeType() == core::NT_RefType && 
			"TYpe of base expression must be of RefType"
		); 
}

std::ostream& Ref::printTo(std::ostream& out) const {
	return out << "[" << Ref::useTypeToStr(getUsage()) << "] - " << refTypeToStr(getType());
}

//===== ScalarRef =======================================================================================

ScalarRef::ScalarRef(const core::VariableAddress& var, const Ref::UseType& usage) : 
	Ref(Ref::SCALAR, var, usage) { }

core::VariableAddress ScalarRef::getVariable() const { 
	return AS_VAR_ADDR(baseExpr);
}

std::ostream& ScalarRef::printTo(std::ostream& out) const {
	Ref::printTo(out);
	return out << "(" << *baseExpr << ")";
}

//===== ArrayRef =======================================================================================

std::ostream& ArrayRef::printTo(std::ostream& out) const {
	Ref::printTo(out);
	out << "(" << *baseExpr << ")";
	out << " IDX: {" << 
		join("; ", idxExpr, [&](std::ostream& jout, const core::ExpressionPtr& cur){ 
				jout << *cur; } ) << "}";
	return out;
}

//===== MemberRef =====================================================================================

MemberRef::MemberRef(const core::ExpressionAddress& memberAcc, const UseType& usage) : 
	Ref(Ref::MEMBER, memberAcc, usage) 
{ 
	assert (memberAcc->getNodeType() == core::NT_CallExpr);

	__unused core::NodeManager& mgr = memberAcc->getNodeManager();
	assert (core::analysis::isCallOf(memberAcc.getAddressedNode(), mgr.getLangBasic().getCompositeMemberAccess()) || 
		core::analysis::isCallOf(memberAcc.getAddressedNode(), mgr.getLangBasic().getCompositeRefElem() ) );

	// initialize the value of the literal
	const core::CallExprAddress& callExpr = AS_CALLEXPR_ADDR(memberAcc);
	identifier = core::static_pointer_cast<const core::Literal>(callExpr.getAddressedNode()->getArgument(1));

	// initialize the value of the named composite type 
	
	core::TypePtr subTy;
	if( core::analysis::isCallOf(memberAcc.getAddressedNode(), mgr.getLangBasic().getCompositeMemberAccess()) ) {
		subTy = callExpr->getArgument(0)->getType();	
	} else if( core::analysis::isCallOf(memberAcc.getAddressedNode(), mgr.getLangBasic().getCompositeRefElem() )) {
		const core::TypePtr& refType = callExpr->getArgument(0)->getType();
		assert(refType.isa<core::RefTypePtr>());
		subTy = refType.as<core::RefTypePtr>()->getElementType();
	}

	switch ( subTy->getNodeType() ) {
	case core::NT_StructType:
		type = core::static_pointer_cast<const core::StructType>( subTy );
		break;
	case core::NT_UnionType:
		type = core::static_pointer_cast<const core::UnionType>( subTy );
		break;
	case core::NT_RecType:
		type = core::static_pointer_cast<const core::RecType>( subTy );
		break;
	case core::NT_GenericType:
		type = core::static_pointer_cast<const core::GenericType>( subTy );
		break;
	default:
		assert_fail() << "Type for member access expression not handled: " << subTy->getNodeType();
	}
}

namespace {

struct TypePrinter : public boost::static_visitor<core::TypePtr> {

	core::TypePtr operator()( const core::TypePtr& type) const {
        return type;
    }

};

} // end anonymous namespace

std::ostream& MemberRef::printTo(std::ostream& out) const {
	Ref::printTo(out);
	return out << "(" << *boost::apply_visitor( TypePrinter(), type) << "." << *identifier << ")";
}

//===== CallRef =====================================================================================
CallRef::CallRef(const core::CallExprAddress& callExpr, const UseType& usage) : Ref::Ref(Ref::CALL, callExpr, usage) { }

core::CallExprAddress CallRef::getCallExpr() const { return AS_CALLEXPR_ADDR(baseExpr); }

std::ostream& CallRef::printTo(std::ostream& out) const {
	Ref::printTo(out);
	return out << " (" << *AS_CALLEXPR_ADDR(baseExpr)->getFunctionExpr() << "(...)" << ")";
}

namespace {

/***
 * DefUseCollect finds occurences of variables (or, more in general, expressions) which can appear
 * in expressions. The main goal of this class is to determine whether a variable is used or
 * defined. In case of arrays references also a pointer to the expressions utilized to index each
 * array dimension needs to be stored. 
 */
class DefUseCollect : public core::IRVisitor<void, core::Address> {
	
	RefList& refSet;
	Ref::UseType usage;
	
	typedef std::vector<core::ExpressionAddress> ExpressionList;
	typedef std::pair<core::ExpressionAddress, ExpressionList> SubscriptContext;
	std::stack<SubscriptContext> idxStack;

	const core::StatementSet& skipStmts;

	void addVariable(const core::ExpressionAddress& var, const Ref::RefType& refType=Ref::SCALAR) {
		const core::TypePtr& type = var->getType(); 
		
		// If the variable is not a ref we are not interested in its usage 
		if (type->getNodeType() != core::NT_RefType) { return; }
	
		core::TypePtr subType = type;
		while(subType->getNodeType() == core::NT_RefType) {
			subType = core::static_pointer_cast<const core::RefType>(subType)->getElementType();
		}

		if (subType->getNodeType() == core::NT_ArrayType || subType->getNodeType() == core::NT_VectorType) { 
			// In the case the sub type is a vector type, it means this is an array reference 
			SubscriptContext& subCtx = idxStack.top();
			refSet.push_back( 
				std::make_shared<ArrayRef>(var, 
					// copy the index expressions in reverse order
					ExpressionList(subCtx.second.rbegin(), subCtx.second.rend()),
					subCtx.first,
					usage) 
				);
			idxStack.top() = SubscriptContext();	// reset the expresion list 
			return ;
		} 

		if (refType == Ref::MEMBER ) {
			refSet.push_back( std::make_shared<MemberRef>(var, usage) );
			return;
		}
		if ( refType == Ref::CALL ) {
			assert(var->getNodeType() == core::NT_CallExpr && "Expected call expression");
			refSet.push_back( std::make_shared<CallRef>(AS_CALLEXPR_ADDR(var), usage) );
			return;
		}

		assert(var->getNodeType() == core::NT_Variable && "Expected scalar variable");
		refSet.push_back( std::make_shared<ScalarRef>(AS_VAR_ADDR(var), usage) );
	}

public:
	DefUseCollect(RefList& refSet, const core::StatementSet& skipStmts) : 
		core::IRVisitor<void, core::Address>(false), refSet(refSet), usage(Ref::USE), skipStmts(skipStmts) 
	{ 
			idxStack.push( SubscriptContext() ); // initialize the stack of array index expressions
	}

	void visitDeclarationStmt(const core::DeclarationStmtAddress& declStmt) {
		usage = Ref::DEF;
		addVariable( declStmt->getVariable() ); // getVariable
		usage = Ref::USE;
		visit( declStmt->getInitialization() ); // getInitialization
	}

	void visitVariable(const core::VariableAddress& var) { addVariable(var); }

	void visitCallExpr(const core::CallExprAddress& callExpr) {
		const core::NodeManager& mgr = callExpr->getNodeManager();

		// save the usage before the entering of this callexpression
		Ref::UseType saveUsage = usage;
		
		// take care of Cpp reference representatoin in ir
		const auto& extension = callExpr->getNodeManager().getLangExtension<core::lang::IRppExtensions>();
		if (core::analysis::isCallOf(callExpr.getAddressedNode(), extension.getRefCppToIR()) || 
			core::analysis::isCallOf(callExpr.getAddressedNode(), extension.getRefIRToCpp()) || 
			core::analysis::isCallOf(callExpr.getAddressedNode(), extension.getRefConstCppToIR()) || 
			core::analysis::isCallOf(callExpr.getAddressedNode(), extension.getRefIRToConstCpp()) || 
			core::analysis::isCallOf(callExpr.getAddressedNode(), extension.getRefCppToConstCpp())	)
		{
			//FIXME correct for DEFUSE?
			visit( callExpr->getArgument(0) ); // arg(0)
			//assert(false && "cppRef in defUse");
			return;
		}

		if (core::analysis::isCallOf(callExpr.getAddressedNode(), mgr.getLangBasic().getRefAssign())) {
			assert( usage != Ref::DEF && "Nested assignment operations" );
			usage = Ref::DEF;
			visit( callExpr->getArgument(0) ); // arg(0)
			usage = Ref::USE;
			visit( callExpr->getArgument(1) ); // arg(1)
			usage = saveUsage; // restore the previous usage
			return;
		}

		if (core::analysis::isCallOf(callExpr.getAddressedNode(), mgr.getLangBasic().getCompositeMemberAccess()) || 
			core::analysis::isCallOf(callExpr.getAddressedNode(), mgr.getLangBasic().getCompositeRefElem() ) ) {

			// if the member expression is accessed directly from a struct variable, then add this usage
			if (core::ExpressionAddress var = core::dynamic_address_cast<const core::Variable>(callExpr->getArgument(0))) {
				addVariable(callExpr, Ref::MEMBER);

				// We are done here
				return;
			}

			// otherwise this use was created by another expression which we have to visit recursively
			visit(callExpr->getArgument(0)); // arg(0)
			return;
		}

		if (core::analysis::isCallOf(callExpr.getAddressedNode(), mgr.getLangBasic().getArraySubscript1D()) ||
			core::analysis::isCallOf(callExpr.getAddressedNode(), mgr.getLangBasic().getArrayRefElem1D()) ||
			core::analysis::isCallOf(callExpr.getAddressedNode(), mgr.getLangBasic().getVectorRefElem()) || 
			core::analysis::isCallOf(callExpr.getAddressedNode(), mgr.getLangBasic().getVectorSubscript()) || 
			core::analysis::isCallOf(callExpr.getAddressedNode(), mgr.getLangBasic().getArrayView()) ) 
		{
			usage = Ref::USE;
			assert(callExpr->getArguments().size() == 2 && "Malformed expression");
			
			// Visit the index expression
			idxStack.push( SubscriptContext() );
			visit( callExpr->getArgument(1) ); // arg(1)
			idxStack.pop();
	
			usage = saveUsage; // restore the previous usage 
			assert(idxStack.size() > 0);
			SubscriptContext& subCtx = idxStack.top();
			// if the start of the subscript expression is not set, this is the start
			if (!subCtx.first) { subCtx.first = callExpr; }
			subCtx.second.push_back( AS_EXPR_ADDR(callExpr->getArgument(1)) ); // arg(1)
			visit( callExpr->getArgument(0) ); // arg(0)
			return;
		}

		// List the IR literals which do not alterate the usage of a variable  
		if (core::analysis::isCallOf(callExpr.getAddressedNode(), mgr.getLangBasic().getRefDeref())) {
			visit( callExpr->getArgument(0) ); // arg(0)
			return;
		}

		// List the IR literals which do not alterate the usage of a variable and therefore are used
		// to convert a ref into another ref 
		if (core::analysis::isCallOf(callExpr.getAddressedNode(), mgr.getLangBasic().getRefVectorToRefArray()) ||
			core::analysis::isCallOf(callExpr.getAddressedNode(), mgr.getLangBasic().getRefReinterpret()) ) 
		{
			visit( callExpr->getArgument(0) ); // arg(0)
			return;
		}
	
		// This call expression could return a reference to a variable which can be either used or
		// defined. Therefore we have to add this usage to the list of usages 
		addVariable(callExpr, Ref::CALL);
		
		// we check whether the function is builtin, in that case we are sure that the use of the
		// variable will be a USE and not a definition. For any other unknown literal, we use the
		// UKNWON
		usage = mgr.getLangBasic().isBuiltIn( callExpr.getAddressOfChild(1).getAddressedNode() ) ? Ref::USE : Ref::UNKNOWN;
		visitNode(callExpr);
		usage = saveUsage;
	}

	void visitLambda(const core::LambdaAddress& lambda) {

		Ref::UseType saveUsage = usage;

		// the parameters has to be treated as definitions for the variable
		vector<core::VariableAddress>&& params = lambda->getParameterList();
		for(auto it = params.begin(), end=params.end(); it!=end; ++it) {
			usage = Ref::DEF;	
			addVariable( *it );
		}
		usage = Ref::USE;
		visit( lambda->getBody() );
		usage = saveUsage;
	}

	void visitStatement(const core::StatementAddress& stmt) {
		if ( skipStmts.find(stmt) == skipStmts.end() ) {
			visitNode(stmt);
		}
	}

	// Generic method which recursively visit IR nodes 
	void visitNode(const core::NodeAddress& node) {
		for(size_t it=0, end=node->getChildList().size(); it != end; ++it) {
			visit( node.getAddressOfChild(it) ); 
		}
	}
};

} // end anonymous namespace 

std::ostream& RefList::printTo(std::ostream& out) const {
	return out << "[" << join(", ", *this, print<deref<RefPtr>>()) << "]";
}

RefList collectDefUse(const core::NodeAddress& root, const core::StatementSet& skipList) {
	RefList ret;
	DefUseCollect duCollVis(ret, skipList);
	duCollVis.visit( root );
	return ret;
}

RefList collectDefUse(const core::NodePtr& root, const core::StatementSet& skipStmt) { 
	return collectDefUse( core::NodeAddress(root), skipStmt );
}

} // end namespace analyis 
} // end namespace insieme 
