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

template <class T>
struct IRWrapper {
	T ref;
	IRWrapper(): ref(T(NULL)) { }
	IRWrapper(const T& type): ref(type) { }
};

struct TypeWrapper: public IRWrapper<core::TypePtr> {
	TypeWrapper(): IRWrapper<core::TypePtr>() { }
	TypeWrapper(const core::TypePtr& type): IRWrapper<core::TypePtr>(type) { }
};

struct ExprWrapper: public IRWrapper<core::ExpressionPtr> {
	ExprWrapper(): IRWrapper<core::ExpressionPtr>() { }
	ExprWrapper(const core::ExpressionPtr& expr): IRWrapper<core::ExpressionPtr>(expr) { }
};

struct StmtWrapper: public std::vector<core::StatementPtr>{
	StmtWrapper(): std::vector<core::StatementPtr>() { }
	StmtWrapper(const core::StatementPtr& stmt):  std::vector<core::StatementPtr>() { push_back(stmt); }

	core::StatementPtr getSingleStmt() const {
		assert(size() == 1 && "More than 1 statement present");
		return front();
	}

	bool isSingleStmt() const { return size() == 1; }
};

unsigned short getSizeSpecifier(core::GenericTypePtr type) {
	LOG(INFO) << type;
	std::vector<core::IntTypeParam> intParamList = type->getIntTypeParameter();
	assert(!intParamList.empty() && "Generic type has no size specifier");
	LOG(INFO) << "Size: " << intParamList[0].getValue();
	return intParamList[0].getValue()/2;
}

}

namespace insieme {

class ClangExprConverter: public StmtVisitor<ClangExprConverter, ExprWrapper> {
	ConversionFactory& convFact;
	const core::ASTBuilder&	builder;

public:
	ClangExprConverter(ConversionFactory& convFact, const core::ASTBuilder& builder): convFact(convFact), builder( builder ) { }

	ExprWrapper VisitIntegerLiteral(clang::IntegerLiteral* intLit) {
		return ExprWrapper( builder.intLiteral( *intLit->getValue().getRawData(),
				getSizeSpecifier( core::dynamic_pointer_cast<const core::GenericType>(convFact.ConvertType( *intLit->getType().getTypePtr() )))) );
	}

	ExprWrapper VisitFloatingLiteral(clang::FloatingLiteral* floatLit) {
		// todo: handle float and doubles
		return ExprWrapper( builder.floatLiteral( floatLit->getValue().convertToDouble(),
				getSizeSpecifier( core::dynamic_pointer_cast<const core::GenericType>(convFact.ConvertType( *floatLit->getType().getTypePtr() )))) );
	}

	ExprWrapper VisitCastExpr(clang::CastExpr* castExpr) {
		return ExprWrapper( builder.castExpr( convFact.ConvertType( *castExpr->getType().getTypePtr() ), Visit(castExpr->getSubExpr()).ref ) );
	}

	ExprWrapper VisitBinaryOperator(clang::BinaryOperator* binOp)  { return ExprWrapper(); }
	ExprWrapper VisitUnaryOperator(clang::UnaryOperator *unOp) { return ExprWrapper(); }
	ExprWrapper VisitArraySubscriptExpr(clang::ArraySubscriptExpr* arraySubExpr) { return ExprWrapper(); }
	ExprWrapper VisitDeclRefExpr(clang::DeclRefExpr* declRef) {	return ExprWrapper(); }

};

class ClangStmtConverter: public StmtVisitor<ClangStmtConverter, StmtWrapper> {
	ConversionFactory& convFact;
public:

	ClangStmtConverter(ConversionFactory& convFact): convFact(convFact) { }

	StmtWrapper VisitVarDecl(clang::VarDecl* varDecl) {
		clang::QualType clangType = varDecl->getType();

		if(!clangType.isCanonical())
			clangType = clangType->getCanonicalTypeInternal();
		core::TypePtr type = convFact.ConvertType( *varDecl->getType().getTypePtr() );

		// initialization value
		core::ExpressionPtr initExpr(NULL);
		if( varDecl->getInit() )
			initExpr = convFact.ConvertExpr( *varDecl->getInit() );
		else {
			Type& ty = *varDecl->getType().getTypePtr();
			if( ty.isFloatingType() || ty.isRealType() || ty.isRealFloatingType() ) {
				// in case of floating types we initialize with a zero value
				initExpr = convFact.builder().floatLiteral(llvm::APFloat::getZero(llvm::APFloat::IEEEsingle).convertToFloat(),
						getSizeSpecifier( core::dynamic_pointer_cast<const core::GenericType>(type) ) );
			} else if ( ty.isIntegerType() || ty.isUnsignedIntegerType() ) {
				// initialize integer value
				initExpr = convFact.builder().intLiteral(*llvm::APInt::getNullValue(16).getRawData(),
						getSizeSpecifier( core::dynamic_pointer_cast<const core::GenericType>(type) ) );
			} else if ( ty.isAnyPointerType() || ty.isRValueReferenceType() || ty.isLValueReferenceType() ) {
				// initialize pointer/reference types with the null value
				//todo
			} else if ( ty.isCharType() || ty.isAnyCharacterType() ) {
				//todo
			} else if ( ty.isBooleanType() ) {
				// boolean values are initialized to false
				initExpr = convFact.builder().boolLiteral(false);
			}
		}
		// todo: initialization for declarations with no initialization value
		return StmtWrapper( convFact.builder().declarationStmt( type, varDecl->getNameAsString(), initExpr ) );
	}

	// In clang a declstmt is represented as a list of VarDecl
	StmtWrapper VisitDeclStmt(clang::DeclStmt* declStmt) {
		// if there is only one declaration in the DeclStmt we return it
		if( declStmt->isSingleDecl() && isa<clang::VarDecl>(declStmt->getSingleDecl()) )
			return VisitVarDecl( dyn_cast<clang::VarDecl>(declStmt->getSingleDecl()) );

		// otherwise we create an an expression list which contains the multiple declaration inside the statement
		StmtWrapper retList;
		for(clang::DeclStmt::decl_iterator it = declStmt->decl_begin(), e = declStmt->decl_end(); it != e; ++it)
			if( clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(*it) )
				retList.push_back( VisitVarDecl(varDecl).getSingleStmt() );
		return retList;
	}

	StmtWrapper VisitReturnStmt(ReturnStmt* retStmt) {
		assert(retStmt->getRetValue() && "ReturnStmt has an empty expression");
		return StmtWrapper( convFact.builder().returnStmt( convFact.ConvertExpr( *retStmt->getRetValue() ) ) );
	}

	StmtWrapper VisitForStmt(ForStmt* forStmt) {
		StmtWrapper retStmt;

		StmtWrapper&& body = Visit(forStmt->getBody());

		LOG(INFO) << "ForStmt body: " << body;

		ExprWrapper&& incExpr = convFact.ConvertExpr( *forStmt->getInc() );
		// Determine the induction variable
		// todo: analyze the incExpr looking for the induction variable for this loop

		LOG(INFO) << "ForStmt incExpr: " << incExpr.ref;

		ExprWrapper condExpr;

		if( VarDecl* condVarDecl = forStmt->getConditionVariable() ) {
			assert(forStmt->getCond() == NULL && "ForLoop condition cannot be a variable declaration and an expression");
			// the for loop has a variable declared in the condition part, e.g.
			// for(...; int a = f(); ...)
			//
			// to handle this kind of situation we have to move the declaration outside the loop body
			// inside a new context
			Expr* expr = condVarDecl->getInit();
			condVarDecl->setInit(NULL); // set the expression to null temporarely
			core::DeclarationStmtPtr&& declStmt = core::dynamic_pointer_cast<const core::DeclarationStmt>( VisitVarDecl(condVarDecl).getSingleStmt() );
			condVarDecl->setInit(expr);

			retStmt.push_back( declStmt );

			// now the condition expression has to be converted into the following form:
			// int a = 0;
			// for(...; a=f(); ...) { }
			const core::VarExprPtr& varExpr = declStmt->getVarExpression();
			core::ExpressionPtr&& initExpr = convFact.ConvertExpr( *expr );
			// todo: build a binary expression
			// condExpr = (varExpr = initExpr);
		} else
			condExpr = convFact.ConvertExpr( *forStmt->getCond() );

		LOG(INFO) << "ForStmt condExpr: " << condExpr.ref;

		StmtWrapper&& initExpr = Visit(forStmt->getInit());
		if( !initExpr.isSingleStmt() ) {
			assert(core::dynamic_pointer_cast<const core::DeclarationStmt>(initExpr[0]) && "Not a declaration statement");
			// we have a multiple declaration in the initialization part of the stmt
			// e.g.
			// for(int a,b=0; ...)
			//
			// to handle this situation we have to create an outer block and declare the variable which is
			// not used as induction variable
			// WE ASSUME FOR NOW THE FIRST DECL IS THE INDUCTION VARIABLE
			std::copy(initExpr.begin()+1, initExpr.end(), std::back_inserter(retStmt));
			initExpr = StmtWrapper( initExpr.front() );
		}

		LOG(INFO) << "ForStmt initExpr: " << initExpr;

		if( initExpr.empty() ) {
			// we are analyzing a loop where the init expression is empty, e.g.:
			// for(;a<..) { }
			//
			// As the IR doesn't support loop stmt with no initialization we represent the for loop as while stmt

		}
		core::DeclarationStmtPtr declStmt = core::dynamic_pointer_cast<const core::DeclarationStmt>(initExpr.getSingleStmt());
		assert(declStmt && "Falied loop init expression conversion");
		retStmt.push_back( convFact.builder().forStmt(declStmt, body.getSingleStmt(), condExpr.ref, incExpr.ref) );
		if(retStmt.size() == 1)
			return retStmt.front();
		// we have to create a CompoundStmt
		return StmtWrapper( convFact.builder().compoundStmt(retStmt) );
	}

	StmtWrapper VisitCompoundStmt(CompoundStmt* compStmt) {
		vector<core::StatementPtr> stmtList;
		std::for_each( compStmt->body_begin(), compStmt->body_end(),
				[ &stmtList, this ](Stmt* stmt){
					// A compoundstmt can contain declaration statements.This means that a clang DeclStmt can be converted in multiple
					// StatementPtr because an initialization list such as: int a,b=1; is converted into the following sequence of statements:
					// int<a> a = 0; int<4> b = 1;
					StmtWrapper&& convertedStmt = this->Visit(stmt);
					std::copy(convertedStmt.begin(), convertedStmt.end(), std::back_inserter(stmtList));
				} );
		return StmtWrapper( convFact.builder().compoundStmt(stmtList) );
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
		assert(false && "TypedefType not yet handled!");
	}

	TypeWrapper VisitTagType(TagType* tagType) {
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
			if(tagDecl->getTagKind() == TagDecl::TK_enum) {

			} else {
				// handle struct/union/class
				RecordDecl* recDecl = dyn_cast<RecordDecl>(tagDecl);
				assert(recDecl && "TagType decl is not of a RecordDecl type!");
				core::NamedCompositeType::Entries structElements;
				std::for_each(recDecl->field_begin(), recDecl->field_end(), [ &structElements, this ]( RecordDecl::field_iterator::value_type curr ){
					structElements.push_back(
							core::NamedCompositeType::Entry(core::Identifier(curr->getNameAsString()), this->Visit( curr->getType().getTypePtr() ).ref )
					);
				});

				TypeWrapper retTy;
				// class and struct are handled in the same way
				if( tagDecl->getTagKind() == TagDecl::TK_struct || tagDecl->getTagKind() ==  TagDecl::TK_class )
					retTy = TypeWrapper( builder.structType( structElements ) );
				else if( tagDecl->getTagKind() == TagDecl::TK_union )
					retTy = TypeWrapper( builder.unionType( structElements ) );
				else
					assert(false);

				typeMap[tagType] = retTy;
				return retTy;
			}
		} else {
			// We didn't find any definition for this type, so we use a name and define it as a generic type
			DLOG(INFO) << builder.genericType( tagDecl->getNameAsString() )->toString();
			return TypeWrapper( builder.genericType( tagDecl->getNameAsString() ) );
		}
		return TypeWrapper();
	}

	TypeWrapper VisitElaboratedType(ElaboratedType* elabType) {
		assert(false && "ElaboratedType not yet handled!");
	}

	TypeWrapper VisitPointerType(PointerType* pointerTy) {
		return TypeWrapper( builder.refType( Visit(pointerTy->getPointeeType().getTypePtr()).ref ) );
	}

	TypeWrapper VisitReferenceType(ReferenceType* refTy) {
		return TypeWrapper( builder.refType( Visit(refTy->getPointeeType().getTypePtr()).ref ) );
	}

};

// ------------------------------------ ConversionFactory ---------------------------

ConversionFactory::ConversionFactory(core::SharedNodeManager mgr): mMgr(mgr), mBuilder(mgr),
		typeConv(new ClangTypeConverter(mBuilder)),
		exprConv(new ClangExprConverter(*this, mBuilder)),
		stmtConv(new ClangStmtConverter(*this)) { }

core::TypePtr ConversionFactory::ConvertType(const clang::Type& type) {
	DLOG(INFO) << "Converting type of class:" << type.getTypeClassName();
	// type.dump();
	return typeConv->Visit(const_cast<Type*>(&type)).ref;
}

core::StatementPtr ConversionFactory::ConvertStmt(const clang::Stmt& stmt) {
	DLOG(INFO) << "Converting stmt:";
	// stmt.dump();
	return stmtConv->Visit(const_cast<Stmt*>(&stmt)).getSingleStmt();
}

core::ExpressionPtr ConversionFactory::ConvertExpr(const clang::Expr& expr) {
	DLOG(INFO) << "Converting expression:";
	// expr.dump();
	return exprConv->Visit(const_cast<Expr*>(&expr)).ref;
}

ConversionFactory::~ConversionFactory() {
	delete typeConv;
	delete stmtConv;
}

// ------------------------------------ ClangTypeConverter ---------------------------

void InsiemeIRConsumer::HandleTopLevelDecl (DeclGroupRef D) {
	if(!mDoConversion)
		return;

	for(DeclGroupRef::const_iterator it = D.begin(), end = D.end(); it!=end; ++it) {
		Decl* decl = *it;
		if(FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(decl)) {
			// finds a definition of this function if any
			for(auto it = funcDecl->redecls_begin(), end = funcDecl->redecls_end(); it!=end; ++it)
				if((*it)->isThisDeclarationADefinition())
					funcDecl = (*it)->getCanonicalDecl();

			core::TypePtr funcType = fact.ConvertType( *funcDecl->getType().getTypePtr() );

			// paramlist
			core::LambdaExpr::ParamList funcParamList;
			std::for_each(funcDecl->param_begin(), funcDecl->param_end(), [&funcParamList, &fact](ParmVarDecl* currParam){
				funcParamList.push_back( fact.builder().paramExpr( fact.ConvertType( *currParam->getType().getTypePtr() ), currParam->getNameAsString()) );
			});
			// this is a function decl
			core::StatementPtr funcBody(NULL);
			if(funcDecl->getBody())
				funcBody =fact.ConvertStmt( *funcDecl->getBody() );

			core::DefinitionPtr IRFuncDecl = fact.builder().definition(funcDecl->getNameAsString(), funcType,
					fact.builder().lambdaExpr(funcType, funcParamList, funcBody), funcDecl->isExternC());
			IRFuncDecl->printTo(std::cout);

		}else if(VarDecl* varDecl = dyn_cast<VarDecl>(decl)) {
			LOG(INFO) << "Converted into: " << fact.ConvertType( *varDecl->getType().getTypePtr() )->toString();
		}
	}
}

void InsiemeIRConsumer::HandleTranslationUnit (ASTContext &Ctx) { }


} // End insieme namespace
