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

#include "program.h"
#include "ast_node.h"
#include "types.h"
#include "statements.h"
#include "container_utils.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include <glog/logging.h>

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

namespace {
struct TypeWrapper {
	core::TypePtr ref;
	TypeWrapper(): ref(core::TypePtr(NULL)) { }
	TypeWrapper(core::TypePtr type): ref(type) { }
};

struct StmtWrapper {
	core::StatementPtr ref;
	StmtWrapper(): ref(core::StatementPtr(NULL)) { }
	StmtWrapper(core::StatementPtr stmt): ref(core::StatementPtr(stmt)) { }
};
}

namespace insieme {

class ClangStmtConverter: public StmtVisitor<ClangStmtConverter, StmtWrapper> {
	ConversionFactory& convFact;
public:
	ClangStmtConverter(ConversionFactory& convFact): convFact(convFact) { }

	StmtWrapper VisitVarDecl(clang::VarDecl* varDecl);

	StmtWrapper VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
//		// look for the vardecl in the map
//		clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(declRef->getDecl());
//		DeclMap::const_iterator fit = mDeclMap.find(varDecl);
//		assert( fit != mDeclMap.end() && "VarDecl is not registered in the DeclMap." );
//		return new PolyVarRefImpl<clang::DeclRefExpr>(fit->second, *declRef );
		return StmtWrapper();
	}

	StmtWrapper VisitIntegerLiteral(clang::IntegerLiteral* intLit){
//		return new PolyIntLitImpl<clang::IntegerLiteral>(*intLit);
		return StmtWrapper();
	}

	StmtWrapper VisitCastExpr(clang::CastExpr* castExpr) {
//		return Visit(castExpr->getSubExpr());
		return StmtWrapper();
	}

	StmtWrapper VisitUnaryOperator(clang::UnaryOperator *unOp) {
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
		return StmtWrapper();
	}

	StmtWrapper VisitBinaryOperator(clang::BinaryOperator* binOp) {
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
		return StmtWrapper();
	}

	StmtWrapper VisitArraySubscriptExpr(clang::ArraySubscriptExpr* arraySubExpr) {
//		return new PolyBinExprImpl<clang::ArraySubscriptExpr>( Visit(arraySubExpr->getBase()),
//				PolyBinExpr::ARRAY_SUB,
//				Visit(arraySubExpr->getIdx()), *arraySubExpr );
		return StmtWrapper();
	}

	// In clang a declstmt is represented as a list of VarDecl
	// the conversion to PolyIR replace with an expressions
	StmtWrapper VisitDeclStmt(clang::DeclStmt* declStmt) {
		// if there is only one declaration in the DeclStmt we return it
		if( declStmt->isSingleDecl() && isa<clang::VarDecl>(declStmt->getSingleDecl()) )
			return VisitVarDecl( dyn_cast<clang::VarDecl>(declStmt->getSingleDecl()) );

		// otherwise we create an an expression list which contains the multiple declaration inside the statement
//		PolyExprListImpl<clang::DeclStmt>* exprList = new PolyExprListImpl<clang::DeclStmt>(*declStmt);
//		for(clang::DeclStmt::decl_iterator it = declStmt->decl_begin(), e = declStmt->decl_end(); it != e; ++it)
//			if( clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(*it) )
//				exprList->push_back( VisitVarDecl(varDecl) );
//		return exprList;
		return StmtWrapper();
	}

	StmtWrapper VisitStmt(Stmt* stmt) {
		std::for_each( stmt->child_begin(), stmt->child_end(), [ this ](Stmt* stmt){ this->Visit(stmt); } );
		return StmtWrapper();
	}

};

#define MAKE_SIZE(n)	toVector(core::IntTypeParam::getConcreteIntParam(n))
#define EMPTY_TYPE_LIST	vector<core::TypePtr>()

class ClangTypeConverter: public TypeVisitor<ClangTypeConverter, TypeWrapper> {
	ConversionFactory& 		convFact;
	const core::ASTBuilder&	builder;

public:
	ClangTypeConverter(ConversionFactory& convFact): convFact(convFact), builder(convFact.builder()) { }

	TypeWrapper VisitBuiltinType(BuiltinType* buldInTy) {

		switch(buldInTy->getKind()) {
		case BuiltinType::Void:  	return TypeWrapper( builder.unitType() );
		case BuiltinType::Bool:		return TypeWrapper( builder.boolType() );
		// char types
		case BuiltinType::Char_U:
		case BuiltinType::UChar:
			return TypeWrapper( builder.genericType("uchar") );
		case BuiltinType::Char16:
			return TypeWrapper( builder.genericType("char", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(char16_t) )) );
		case BuiltinType::Char32:
			return TypeWrapper( builder.genericType("char", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(char32_t) )) );
		case BuiltinType::Char_S:
		case BuiltinType::SChar:
			return TypeWrapper( builder.genericType("char") );
		case BuiltinType::WChar:
			return TypeWrapper( builder.genericType("wchar") );
		// short types
		case BuiltinType::UShort:
			return TypeWrapper( builder.genericType("uint", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(unsigned short) )) );
		case BuiltinType::Short:
			return TypeWrapper( builder.genericType("int", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(short) )) );
		// integer types
		case BuiltinType::UInt:
			return TypeWrapper( builder.genericType("uint", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(unsigned int) )) );
		case BuiltinType::Int:
			return TypeWrapper( builder.genericType("int", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(int) )) );
		case BuiltinType::UInt128:
			return TypeWrapper( builder.genericType("uint", EMPTY_TYPE_LIST, MAKE_SIZE( 16 )) );
		case BuiltinType::Int128:
			return TypeWrapper( builder.genericType("int", EMPTY_TYPE_LIST, MAKE_SIZE( 16 )) );
		// long types
		case BuiltinType::ULong:
			return TypeWrapper( builder.genericType("uint", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(unsigned long) )) );
		case BuiltinType::ULongLong:
			return TypeWrapper( builder.genericType("uint", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(unsigned long long) )) );
		case BuiltinType::Long:
			return TypeWrapper( builder.genericType("int", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(long) )) );
		case BuiltinType::LongLong:
			return TypeWrapper( builder.genericType("int", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(long long) )) );
		// float types
		case BuiltinType::Float:
			return TypeWrapper( builder.genericType("real", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(float) )) );
		case BuiltinType::Double:
			return TypeWrapper( builder.genericType("real", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(double) )) );
		case BuiltinType::LongDouble:
			return TypeWrapper( builder.genericType("real", EMPTY_TYPE_LIST, MAKE_SIZE( sizeof(long double) )) );
		case BuiltinType::NullPtr:
		case BuiltinType::Overload:
		case BuiltinType::Dependent:
		case BuiltinType::UndeducedAuto:
		default:
			throw "type not supported"; //todo introduce exception class
		}
		return TypeWrapper();
	}

	TypeWrapper VisitComplexType(ComplexType* bulinTy) {
		LOG(INFO) << "Converting complex type";
		return TypeWrapper();
	}

	TypeWrapper VisitArrayType(ArrayType* arrTy) {
		LOG(INFO) << "Converting array type";
		return TypeWrapper();
	}

	TypeWrapper VisitFunctionType(FunctionType* funcTy) {
		LOG(INFO) << "Converting function type";
		return TypeWrapper();
	}

	TypeWrapper VisitPointerType(PointerType* pointerTy) {
		LOG(INFO) << "Converting pointer type";
		return TypeWrapper( builder.refType(Visit(pointerTy-> getPointeeType().getTypePtr()).ref) );
	}

	TypeWrapper VisitReferenceType(ReferenceType* refTy) { return TypeWrapper(); }

};

ConversionFactory::ConversionFactory(core::SharedNodeManager mgr): mMgr(mgr), mBuilder(mgr),
		stmtConv(new ClangStmtConverter(*this)), typeConv(new ClangTypeConverter(*this)) { }

core::TypePtr ConversionFactory::ConvertType(const clang::Type& type) {
	return typeConv->Visit(const_cast<Type*>(&type)).ref;
}

core::StatementPtr ConversionFactory::ConvertStmt(const clang::Stmt& stmt) {
	return stmtConv->Visit(const_cast<Stmt*>(&stmt)).ref;
}

ConversionFactory::~ConversionFactory() {
	delete typeConv;
	delete stmtConv;
}

// ------------------------------------ ClangStmtConverter ---------------------------

StmtWrapper ClangStmtConverter::VisitVarDecl(clang::VarDecl* varDecl) {

	// TypeWrapper tw = ConversionFactory::ConvertType( *varDecl->getType().getTypePtr() );

//	if(!!tw.ref)
//		DLOG(INFO) << tw.ref->toString();

//	PolyVarDeclImpl<clang::VarDecl>* polyVar = new PolyVarDeclImpl<clang::VarDecl>(*varDecl);
//	// registering variable in the var map
//	mDeclMap[varDecl] = polyVar;
//	// a vardecl is stored as a simple varref
//	PolyExprPtr ret = new PolyVarRefImpl<clang::VarDecl>(polyVar, *varDecl );
//	if( varDecl->getInit() ) {
//		// there is an initialization
//		ret = new PolyBinExprImpl<clang::VarDecl>( ret, PolyBinExpr::ASSIGN, Visit(varDecl->getInit()), *varDecl );
//	}
//	return ret;
	return StmtWrapper();
}

// ------------------------------------ ClangTypeConverter ---------------------------

void InsiemeIRConsumer::HandleTopLevelDecl (DeclGroupRef D) {
	for(DeclGroupRef::const_iterator it = D.begin(), end = D.end(); it!=end; ++it) {
//		Decl* decl = *it;
//		if(FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(decl)) {
			// this is a function decl
//			if(funcDecl->getBody())
//				ConversionFactory::ConvertStmt( *funcDecl->getBody() );
//		}else if(VarDecl* varDecl = dyn_cast<VarDecl>(decl)) {
//			ConversionFactory::ConvertType( *varDecl->getType().getTypePtr() );
//		}
	}
}

void InsiemeIRConsumer::HandleTranslationUnit (ASTContext &Ctx) {



}


} // End insieme namespace
