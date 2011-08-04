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

#include "insieme/transform/pattern/irconvert.h"
#include "insieme/core/ast_address.h"
#include "insieme/utils/container_utils.h"

using std::make_shared;

namespace insieme {
namespace transform {
namespace pattern {

// IRTree

std::ostream& IRTree::printTo(std::ostream& out) const {
	if(!evaluated) {
		return out << "irtree[lazy](" << getId() << "," << *data << ")";
	}
	else {
		return out << "irtree[evaled](" << getId() << "," << join(",", subTrees, print<deref<TreePtr>>()) << ")";
	}
}

bool IRTree::operator==(Tree& other) {
	// do not evaluate if ids different
	// this is a stupid idea since pattern matching will not see the subtree!
	//if(id != other.getId()) {
	//	LOG(INFO) << "~~~~~~~ this/other: " << *this << "/" << other;
	//	return false;
	//}
	evalFunctor(*this);
	bool otherIsIR = typeid(other) == typeid(IRTree);
	if(otherIsIR) static_cast<IRTree&>(other).evalFunctor(static_cast<IRTree&>(other));
	return Tree::operator==(other);
}

// IRLeaf

std::ostream& IRLeaf::printTo(std::ostream& out) const {
	return out << "irleaf(" << getId() << "," << *data << ")";
}

bool IRLeaf::operator==(Tree& other) {
	if(this == &other) {
		return true;
	}
	if(getId() != other.getId()) {
		return false;
	}
	// other is also an IRLeaf
	const IRLeaf& otherleaf = static_cast<const IRLeaf&>(other);
	return *data == *otherleaf.data;
}

// IRBlob

std::ostream& IRBlob::printTo(std::ostream& out) const {
	return out << "irblob(" << getId() << "," << blob << ")";
}

bool IRBlob::operator==(Tree& other) {
	if(this == &other) {
		return true;
	}
	if(getId() != other.getId()) {
		return false;
	}
	// other is also an IRBlob
	const IRBlob& otherblob = static_cast<const IRBlob&>(other);
	return blob == otherblob.blob;
}

// ConversionVisitor

const IRTree::EvalFunctor& ConversionVisitor::getDefaultChildEvaluator() {
	static IRTree::EvalFunctor eval = [this](IRTree& self){
		if(!self.isEvaluated()) {
			auto children = self.getData().getChildAddresses();
			std::transform(children.begin(), children.end(), back_inserter(self.getSubTrees()), 
				[this](const NodeAddress& cadd) { return this->visit(cadd); });
			self.setEvaluated();
		}
	};
	return eval;
}

// TYPES

TreePtr ConversionVisitor::visitTypeVariable(const TypeVariableAddress& addr){
	return make_shared<IRLeaf>(addr);
}
TreePtr ConversionVisitor::visitFunctionType(const FunctionTypeAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitTupleType(const TupleTypeAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitRecType(const RecTypeAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}

TreePtr ConversionVisitor::visitGenericType(const GenericTypeAddress& addr){
	static IRTree::EvalFunctor genEval = [this](IRTree& self){
		if(!self.isEvaluated()) {
			auto selfGenNode = dynamic_pointer_cast<const GenericType>(self.getData().getAddressedNode());
			self.getSubTrees().push_back(make_shared<IRBlob>(selfGenNode->getFamilyName()));
			getDefaultChildEvaluator()(self);
		}
	};
	return make_shared<IRTree>(addr, genEval);
}

TreePtr ConversionVisitor::visitStructType(const StructTypeAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitUnionType(const UnionTypeAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}

// STATEMENTS

TreePtr ConversionVisitor::visitBreakStmt(const BreakStmtAddress& addr){
	return make_shared<IRLeaf>(addr);
}
TreePtr ConversionVisitor::visitContinueStmt(const ContinueStmtAddress& addr){
	return make_shared<IRLeaf>(addr);
}
TreePtr ConversionVisitor::visitReturnStmt(const ReturnStmtAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitDeclarationStmt(const DeclarationStmtAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitCompoundStmt(const CompoundStmtAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitWhileStmt(const WhileStmtAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitForStmt(const ForStmtAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitIfStmt(const IfStmtAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitSwitchStmt(const SwitchStmtAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}

// EXPRESSIONS

TreePtr ConversionVisitor::visitVariable(const VariableAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitLambdaExpr(const LambdaExprAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitBindExpr(const BindExprAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitLiteral(const LiteralAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitCallExpr(const CallExprAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitCastExpr(const CastExprAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitJobExpr(const JobExprAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitTupleExpr(const TupleExprAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitVectorExpr(const VectorExprAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitStructExpr(const StructExprAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitUnionExpr(const UnionExprAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitMemberAccessExpr(const MemberAccessExprAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitTupleProjectionExpr(const TupleProjectionExprAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}

// PROGRAM

TreePtr ConversionVisitor::visitProgram(const ProgramAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}

// SUPPORTING

TreePtr ConversionVisitor::visitIdentifier(const IdentifierAddress& addr){
	return make_shared<IRLeaf>(addr);
}

TreePtr ConversionVisitor::visitVariableIntTypeParam(const VariableIntTypeParamAddress& addr){
	return make_shared<IRLeaf>(addr);
}
TreePtr ConversionVisitor::visitConcreteIntTypeParam(const ConcreteIntTypeParamAddress& addr){
	return make_shared<IRLeaf>(addr);
}
TreePtr ConversionVisitor::visitInfiniteIntTypeParam(const InfiniteIntTypeParamAddress& addr){
	return make_shared<IRLeaf>(addr);
}

TreePtr ConversionVisitor::visitLambda(const LambdaAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitLambdaDefinition(const LambdaDefinitionAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}

TreePtr ConversionVisitor::visitRecTypeDefinition(const RecTypeDefinitionAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}

// MARKER

TreePtr ConversionVisitor::visitMarkerExpr(const MarkerExprAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}
TreePtr ConversionVisitor::visitMarkerStmt(const MarkerStmtAddress& addr){
	return make_shared<IRTree>(addr, getDefaultChildEvaluator());
}


} // end namespace pattern
} // end namespace transform
} // end namespace insieme
