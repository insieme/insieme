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

#include "conversion.h"
#include "ast_node.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include <glog/logging.h>

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

namespace insieme {

struct IRNode {
	core::NodePtr ref;

	IRNode(): ref(core::NodePtr(NULL)) { }
};

class ClangStmtConverter: public StmtVisitor<ClangStmtConverter, IRNode> {

public:
	IRNode VisitVarDecl(clang::VarDecl* varDecl) {
//		PolyVarDeclImpl<clang::VarDecl>* polyVar = new PolyVarDeclImpl<clang::VarDecl>(*varDecl);
//		// registering variable in the var map
//		mDeclMap[varDecl] = polyVar;
//		// a vardecl is stored as a simple varref
//		PolyExprPtr ret = new PolyVarRefImpl<clang::VarDecl>(polyVar, *varDecl );
//		if( varDecl->getInit() ) {
//			// there is an initialization
//			ret = new PolyBinExprImpl<clang::VarDecl>( ret, PolyBinExpr::ASSIGN, Visit(varDecl->getInit()), *varDecl );
//		}
//		return ret;
		return IRNode();
	}

	IRNode VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
//		// look for the vardecl in the map
//		clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(declRef->getDecl());
//		DeclMap::const_iterator fit = mDeclMap.find(varDecl);
//		assert( fit != mDeclMap.end() && "VarDecl is not registered in the DeclMap." );
//		return new PolyVarRefImpl<clang::DeclRefExpr>(fit->second, *declRef );
		return IRNode();
	}

	IRNode VisitIntegerLiteral(clang::IntegerLiteral* intLit){
//		return new PolyIntLitImpl<clang::IntegerLiteral>(*intLit);
		return IRNode();
	}

	IRNode VisitCastExpr(clang::CastExpr* castExpr) {
//		return Visit(castExpr->getSubExpr());
		return IRNode();
	}

	IRNode VisitUnaryOperator(clang::UnaryOperator *unOp) {
//		PolyExprPtr subExpr( BaseClass::Visit(unOp->getSubExpr()) );
//		PolyUnExpr::Op op;
//		switch(unOp->getOpcode()){
//			case clang::UnaryOperator::Minus:
//				op = PolyUnExpr::MINUS; break;
//			case clang::UnaryOperator::Plus:
//				op = PolyUnExpr::PLUS; break;
//			case clang::UnaryOperator::PostInc:
//				op = PolyUnExpr::POST_INC; break;
//			case clang::UnaryOperator::PreInc:
//				op = PolyUnExpr::PRE_INC; break;
//			case clang::UnaryOperator::PostDec:
//				op = PolyUnExpr::POST_DEC; break;
//			case clang::UnaryOperator::PreDec:
//				op = PolyUnExpr::PRE_DEC; break;
//			default:
//				FATAL("Unary operator '" << clang::UnaryOperator::getOpcodeStr(unOp->getOpcode()) <<
//					  "' not (yet) supported in the PolyIR representation.");
//		}
//		return new PolyUnExprImpl<clang::UnaryOperator>( op, subExpr, *unOp );
		return IRNode();
	}

	IRNode VisitBinaryOperator(clang::BinaryOperator* binOp) {
//		PolyExprPtr lhs( BaseClass::Visit(binOp->getLHS()) );
//		PolyExprPtr rhs( BaseClass::Visit(binOp->getRHS()) );
//		PolyBinExpr::Op op;
//		switch(binOp->getOpcode()){
//			case clang::BinaryOperator::Sub:
//				op = PolyBinExpr::SUB; break;
//			case clang::BinaryOperator::Add:
//				op = PolyBinExpr::SUM; break;
//			case clang::BinaryOperator::Assign:
//				op = PolyBinExpr::ASSIGN; break;
//			case clang::BinaryOperator::LT:
//				op = PolyBinExpr::LT; break;
//			case clang::BinaryOperator::LE:
//				op = PolyBinExpr::LE; break;
//			case clang::BinaryOperator::GT:
//				op = PolyBinExpr::GT; break;
//			case clang::BinaryOperator::GE:
//				op = PolyBinExpr::GE; break;
//			case clang::BinaryOperator::EQ:
//				op = PolyBinExpr::EQ; break;
//			default:
//				FATAL("Binary operator '" << clang::BinaryOperator::getOpcodeStr(binOp->getOpcode()) <<
//					  "' not (yet) supported in the PolyIR representation.");
//		}
//		return new PolyBinExprImpl<clang::BinaryOperator>( lhs, op, rhs, *binOp );
		return IRNode();
	}

	IRNode VisitArraySubscriptExpr(clang::ArraySubscriptExpr* arraySubExpr) {
//		return new PolyBinExprImpl<clang::ArraySubscriptExpr>( Visit(arraySubExpr->getBase()),
//				PolyBinExpr::ARRAY_SUB,
//				Visit(arraySubExpr->getIdx()), *arraySubExpr );
		return IRNode();
	}

	// In clang a declstmt is represented as a list of VarDecl
	// the conversion to PolyIR replace with an expressions
	IRNode VisitDeclStmt(clang::DeclStmt* declStmt) {
//		// if there is only one declaration in the DeclStmt we return it
//		if( declStmt->isSingleDecl() && isa<clang::VarDecl>(declStmt->getSingleDecl()) )
//			return VisitVarDecl( dyn_cast<clang::VarDecl>(declStmt->getSingleDecl()) );
//
//		// otherwise we create an an expression list which contains the multiple declaration inside the statement
//		PolyExprListImpl<clang::DeclStmt>* exprList = new PolyExprListImpl<clang::DeclStmt>(*declStmt);
//		for(clang::DeclStmt::decl_iterator it = declStmt->decl_begin(), e = declStmt->decl_end(); it != e; ++it)
//			if( clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(*it) )
//				exprList->push_back( VisitVarDecl(varDecl) );
//		return exprList;
		return IRNode();
	}

};

class ClangTypeConverter: public TypeVisitor<ClangTypeConverter, void> {
public:

	void VisitBuiltinType(BuiltinType* bulinTy) { }

	void VisitComplexType(ComplexType* bulinTy) { }

	void VisitArrayType(ArrayType* arrTy) { }

	void VisitFunctionType(FunctionType* funcTy) { }

	void VisitPointerType(PointerType* pointerTy) { }

	void VisitReferenceType(ReferenceType* refTy) { }

};


void InsiemeIRConsumer::HandleTopLevelDecl (DeclGroupRef D) {
	ClangStmtConverter stmtConv;
	ClangTypeConverter typeConv;
	for(DeclGroupRef::const_iterator it = D.begin(), end = D.end(); it!=end; ++it) {
		Decl* decl = *it;
		if(FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(decl)) {
			// this is a function decl
			if(funcDecl->getBody())
				stmtConv.Visit( funcDecl->getBody() );
		}else if(VarDecl* varDecl = dyn_cast<VarDecl>(decl)) {
			typeConv.Visit(varDecl->getType().getTypePtr());
		}
	}
}

void InsiemeIRConsumer::HandleTranslationUnit (ASTContext &Ctx) {



}


} // End insieme namespace
