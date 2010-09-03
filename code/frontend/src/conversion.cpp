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

#include "utils/types_lenght.h"
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

	StmtWrapper VisitVarDecl(clang::VarDecl* varDecl) {
		DLOG(INFO) << "Visiting VarDecl";
		clang::QualType clangType = varDecl->getType();
		clangType->dump();
		if(!clangType.isCanonical())
			clangType = clangType->getCanonicalTypeInternal();
		core::TypePtr type = convFact.ConvertType( *varDecl->getType().getTypePtr() );

		if(!!type)
			DLOG(INFO) << type->toString();

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
	const core::ASTBuilder&	builder;

	typedef std::map<Type*, TypeWrapper> TypeMap;

	TypeMap typeMap;

public:
	ClangTypeConverter(const core::ASTBuilder& builder): builder( builder ) { }

	// -------------------- BUILTIN ------------------------------------------------------------------------------
	/**
	 * This method handles buildin types (void,int,long,float,...).
	 */
	TypeWrapper VisitBuiltinType(BuiltinType* buldInTy) {

		switch(buldInTy->getKind()) {
		case BuiltinType::Void:
			return TypeWrapper( builder.unitType() );
		case BuiltinType::Bool:
			return TypeWrapper( builder.boolType() );

		// char types
		case BuiltinType::Char_U:
		case BuiltinType::UChar:
			return TypeWrapper( builder.genericType("uchar") );
		case BuiltinType::Char16:
			return TypeWrapper( builder.genericType("char", EMPTY_TYPE_LIST, MAKE_SIZE(2)) );
		case BuiltinType::Char32:
			return TypeWrapper( builder.genericType("char", EMPTY_TYPE_LIST, MAKE_SIZE(4)) );
		case BuiltinType::Char_S:
		case BuiltinType::SChar:
			return TypeWrapper( builder.genericType("char") );
		case BuiltinType::WChar:
			return TypeWrapper( builder.genericType("wchar") );

		// integer types
		case BuiltinType::UShort:
			return TypeWrapper( builder.genericType("uint", EMPTY_TYPE_LIST, MAKE_SIZE( SHORT_LENGTH )) );
		case BuiltinType::Short:
			return TypeWrapper( builder.genericType("int", EMPTY_TYPE_LIST, MAKE_SIZE( SHORT_LENGTH )) );
		case BuiltinType::UInt:
			return TypeWrapper( builder.genericType("uint", EMPTY_TYPE_LIST, MAKE_SIZE( INT_LENGTH )) );
		case BuiltinType::Int:
			return TypeWrapper( builder.genericType("int", EMPTY_TYPE_LIST, MAKE_SIZE( INT_LENGTH )) );
		case BuiltinType::UInt128:
			return TypeWrapper( builder.genericType("uint", EMPTY_TYPE_LIST, MAKE_SIZE( 16 )) );
		case BuiltinType::Int128:
			return TypeWrapper( builder.genericType("int", EMPTY_TYPE_LIST, MAKE_SIZE( 16 )) );
		case BuiltinType::ULong:
			return TypeWrapper( builder.genericType("uint", EMPTY_TYPE_LIST, MAKE_SIZE( LONG_LENGTH )) );
		case BuiltinType::ULongLong:
			return TypeWrapper( builder.genericType("uint", EMPTY_TYPE_LIST, MAKE_SIZE( LONG_LONG_LENGTH )) );
		case BuiltinType::Long:
			return TypeWrapper( builder.genericType("int", EMPTY_TYPE_LIST, MAKE_SIZE( LONG_LENGTH )) );
		case BuiltinType::LongLong:
			return TypeWrapper( builder.genericType("int", EMPTY_TYPE_LIST, MAKE_SIZE( LONG_LONG_LENGTH )) );

		// real types
		case BuiltinType::Float:
			return TypeWrapper( builder.genericType("real", EMPTY_TYPE_LIST, MAKE_SIZE( FLOAT_LENGTH )) );
		case BuiltinType::Double:
			return TypeWrapper( builder.genericType("real", EMPTY_TYPE_LIST, MAKE_SIZE( DOUBLE_LENGTH )) );
		case BuiltinType::LongDouble:
			return TypeWrapper( builder.genericType("real", EMPTY_TYPE_LIST, MAKE_SIZE( LONG_DOUBLE_LENGTH )) );

		// not supported types
		case BuiltinType::NullPtr:
		case BuiltinType::Overload:
		case BuiltinType::Dependent:
		case BuiltinType::UndeducedAuto:
		default:
			throw "type not supported"; //todo introduce exception class
		}
		return TypeWrapper();
	}

	// --------------------  COMPLEX   ------------------------------------------------------------------------------
	TypeWrapper VisitComplexType(ComplexType* bulinTy) {
		assert(false && "ComplexType not yet handled!");
	}

	// ------------------------   ARRAYS  ----------------------------------------------------------------
	/**
	 * This method handles the canonical version of C arrays with a specified constant size.
	 * For example, the canonical type for 'int A[4 + 4*100]' is a ConstantArrayType where the element type is 'int' and the size is 404
	 *
	 * The IR representation for such array will be: vector<ref<int<4>>,404>
	 */
	TypeWrapper VisitConstantArrayType(ConstantArrayType* arrTy) {
		if(arrTy->isSugared())
			// if the type is sugared, we Visit the desugared type
			return Visit( arrTy->desugar().getTypePtr() );

		size_t arrSize = *arrTy->getSize().getRawData();
		TypeWrapper elemTy = Visit( arrTy->getElementType().getTypePtr() );
		assert(elemTy.ref && "Conversion of array element type failed.");
		return TypeWrapper( builder.vectorType(builder.refType(elemTy.ref), arrSize) );
	}

	/**
	 * This method handles C arrays with an unspecified size. For example 'int A[]' has an IncompleteArrayType where the element
	 * type is 'int' and the size is unspecified.
	 *
	 * The representation for such array will be: ref<array<ref<int<4>>>>
	 */
	TypeWrapper VisitIncompleteArrayType(IncompleteArrayType* arrTy) {
		if(arrTy->isSugared())
			// if the type is sugared, we Visit the desugared type
			return Visit( arrTy->desugar().getTypePtr() );

		TypeWrapper elemTy = Visit( arrTy->getElementType().getTypePtr() );
		assert(elemTy.ref && "Conversion of array element type failed.");
		return TypeWrapper( builder.refType( builder.arrayType(builder.refType(elemTy.ref)) ) );
	}

	/**
	 * This class represents C arrays with a specified size which is not an integer-constant-expression. For example, 'int s[x+foo()]'.
	 * Since the size expression is an arbitrary expression, we store it as such. Note: VariableArrayType's aren't uniqued
	 * (since the expressions aren't) and should not be: two lexically equivalent variable array types could mean different things,
	 * for example, these variables do not have the same type dynamically:
	 * 	 void foo(int x) { int Y[x]; ++x; int Z[x]; }
	 *
	 * he representation for such array will be: array<ref<int<4>>>( expr() )
	 */
	TypeWrapper VisitVariableArrayType(VariableArrayType* arrTy) {
		if(arrTy->isSugared())
			// if the type is sugared, we Visit the desugared type
			return Visit( arrTy->desugar().getTypePtr() );

		TypeWrapper elemTy = Visit( arrTy->getElementType().getTypePtr() );
		assert(elemTy.ref && "Conversion of array element type failed.");
		return TypeWrapper( builder.arrayType(builder.refType(elemTy.ref)) );
	}

	/**
	 * This type represents an array type in C++ whose size is a value-dependent expression. For example:
	 *
	 *  template<typename T, int Size>
	 *  class array {
	 *     T data[Size];
	 *  };
	 *
	 *  For these types, we won't actually know what the array bound is until template instantiation occurs,
	 *  at which point this will become either a ConstantArrayType or a VariableArrayType.
	 */
	TypeWrapper VisitDependentSizedArrayType(DependentSizedArrayType* arrTy) {
		assert(false && "DependentSizedArrayType not yet handled!");
	}

	// --------------------  FUNCTIONS  ----------------------------------------------------------------------------
	/**
	 * Represents a prototype with argument type info, e.g. 'int foo(int)' or 'int foo(void)'. 'void' is represented as having no arguments,
	 * not as having a single void argument. Such a type can have an exception specification, but this specification is not part of the canonical type.
	 */
	TypeWrapper VisitFunctionProtoType(FunctionProtoType* funcTy) {
		core::TypePtr retTy = Visit( funcTy->getResultType().getTypePtr() ).ref;
		assert(retTy && "Function has no return type!");

		core::TupleType::ElementTypeList argTypes;
		std::for_each(funcTy->arg_type_begin(), funcTy->arg_type_end(), [ &argTypes, this ](const QualType& currArgType){
			argTypes.push_back( this->Visit( currArgType.getTypePtr() ).ref );
		} );

		if( argTypes.size() == 1 && argTypes.front() == builder.unitType()) {
			// we have only 1 argument, and it is a unit type (void), remove it from the list
			argTypes.clear();
		}

		return TypeWrapper( builder.functionType( builder.tupleType(argTypes), retTy) );
	}

	/**
	 *  Represents a K&R-style 'int foo()' function, which has no information available about its arguments.
	 */
	TypeWrapper VisitFunctionNoProtoType(FunctionNoProtoType* funcTy) {
		core::TypePtr retTy = Visit( funcTy->getResultType().getTypePtr() ).ref;
		assert(retTy && "Function has no return type!");

		return TypeWrapper( builder.functionType( builder.tupleType(), retTy) );
	}

	TypeWrapper VisitTypedefType(TypedefType* elabType) {
		LOG(INFO) << "Converting typedef type" << std::endl;
		return TypeWrapper();
	}

	TypeWrapper VisitTagType(TagType* tagType) {
		DLOG(INFO) << "Converting tag type" << std::endl;

		// lookup the type map to see if this type has been already converted
		TypeMap::const_iterator fit = typeMap.find(tagType);
		if(fit != typeMap.end())
			return fit->second;

		// otherwise do the conversion
		TagDecl* tagDecl = tagType->getDecl()->getCanonicalDecl();

		// iterate through all the re-declarations to see if one of them provides a definition
		TagDecl::redecl_iterator i,e = tagDecl->redecls_end();
		for(i = tagDecl->redecls_begin(); i != e && !i->isDefinition(); ++i) ;
		if(i != e) {
			tagDecl = tagDecl->getDefinition();
			// we found a definition for this declaration, use it
			assert(tagDecl->isDefinition() && "TagType is not a definition");

			DLOG(INFO) << tagDecl->getKindName() << " " << tagDecl->getTagKind();
			switch(tagDecl->getTagKind()) {
			case TagDecl::TK_struct:
			case TagDecl::TK_union: {
				RecordDecl* recDecl = dyn_cast<RecordDecl>(tagDecl);
				assert(recDecl && "TagType decl is not of a RecordDecl type!");
				core::NamedCompositeType::Entries structElements;
				std::for_each(recDecl->field_begin(), recDecl->field_end(), [ &structElements, this ]( RecordDecl::field_iterator::value_type curr ){
					structElements.push_back(
							core::NamedCompositeType::Entry(core::Identifier(curr->getNameAsString()), this->Visit( curr->getType().getTypePtr() ).ref )
					);
				});
				TypeWrapper retTy;
				if(tagDecl->getTagKind() == TagDecl::TK_struct)
					retTy = TypeWrapper( builder.structType( structElements ) );
				else
					retTy = TypeWrapper( builder.unionType( structElements ) );
				typeMap[tagType] = retTy;
				return retTy;
			}
			case TagDecl::TK_class:
			case TagDecl::TK_enum:
				return TypeWrapper();
			}
		} else {
			// We didn't find any definition for this type, so we use a name and define it as a generic type
			DLOG(INFO) << builder.genericType( tagDecl->getNameAsString() )->toString();
			return TypeWrapper( builder.genericType( tagDecl->getNameAsString() ) );
		}
		return TypeWrapper();
	}

	TypeWrapper VisitElaboratedType(ElaboratedType* elabType) {
		LOG(INFO) << "Converting elaborated type" << std::endl;
		return TypeWrapper();
	}

	TypeWrapper VisitPointerType(PointerType* pointerTy) {
		return TypeWrapper( builder.refType( Visit(pointerTy->getPointeeType().getTypePtr()).ref ) );
	}

	TypeWrapper VisitReferenceType(ReferenceType* refTy) {
		return TypeWrapper( builder.refType( Visit(refTy->getPointeeType().getTypePtr()).ref ) );
	}

};

ConversionFactory::ConversionFactory(core::SharedNodeManager mgr): mMgr(mgr), mBuilder(mgr),
		stmtConv(new ClangStmtConverter(*this)), typeConv(new ClangTypeConverter(mBuilder)) { }

core::TypePtr ConversionFactory::ConvertType(const clang::Type& type) {
	DLOG(INFO) << "Converting type of class:" << type.getTypeClassName();
	type.dump();
	return typeConv->Visit(const_cast<Type*>(&type)).ref;
}

core::StatementPtr ConversionFactory::ConvertStmt(const clang::Stmt& stmt) {
	DLOG(INFO) << "Converting stmt:";
	stmt.dump();
	return stmtConv->Visit(const_cast<Stmt*>(&stmt)).ref;
}

ConversionFactory::~ConversionFactory() {
	delete typeConv;
	delete stmtConv;
}

// ------------------------------------ ClangTypeConverter ---------------------------

void InsiemeIRConsumer::HandleTopLevelDecl (DeclGroupRef D) {
	for(DeclGroupRef::const_iterator it = D.begin(), end = D.end(); it!=end; ++it) {
		Decl* decl = *it;
		if(FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(decl)) {
			LOG(INFO) << "Converted into: " << fact.ConvertType( *funcDecl->getType().getTypePtr() );
			// this is a function decl
			if(funcDecl->getBody())
				fact.ConvertStmt( *funcDecl->getBody() );
		}else if(VarDecl* varDecl = dyn_cast<VarDecl>(decl)) {
			LOG(INFO) << "Converted into: " << fact.ConvertType( *varDecl->getType().getTypePtr() )->toString();
		}
	}
}

void InsiemeIRConsumer::HandleTranslationUnit (ASTContext &Ctx) {



}


} // End insieme namespace
