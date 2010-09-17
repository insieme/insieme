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

#include <sstream>

#include <glog/logging.h>

#include <memory>

#include "conversion.h"

#include "utils/types_lenght.h"
#include "utils/source_locations.h"
#include "program.h"
#include "ast_node.h"
#include "types.h"
#include "statements.h"
#include "container_utils.h"
#include "lang_basic.h"
#include "numeric_cast.h"
#include "naming.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include <boost/graph/adjacency_list.hpp>
#include <boost/graph/graphviz.hpp>

using namespace boost;

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

insieme::core::ExpressionPtr EmptyExpr(const insieme::core::ASTBuilder& builder) {
	return builder.literal("0", core::lang::TYPE_INT_GEN_PTR);
}

// Returns a string of the text within the source range of the input stream
// unfortunately clang only keeps the location of the beginning of the literal
// so the end has to be found manually
std::string GetStringFromStream(const SourceManager& srcMgr, const SourceLocation& start) {
	DLOG(INFO) << insieme::frontend::util::Column(start, srcMgr);
	std::pair<FileID, unsigned> startLocInfo = srcMgr.getDecomposedLoc( start);

	std::pair<const char*, const char*> startBuffer = srcMgr.getBufferData(startLocInfo.first);
	const char *strDataStart = startBuffer.first + startLocInfo.second;
//	DLOG(INFO) << "VALUE: " << string(strDataStart, clang::Lexer::MeasureTokenLength(start, srcMgr, clang::LangOptions()));
	return string(strDataStart, clang::Lexer::MeasureTokenLength(start, srcMgr, clang::LangOptions()));
}

// Tried to aggregate statements into a compound statement (if more than 1 statement is present)
core::StatementPtr tryAggregateStmts(const core::ASTBuilder& builder, const vector<core::StatementPtr>& stmtVect) {
	if( stmtVect.size() == 1 )
		return stmtVect.front();
	return builder.compoundStmt(stmtVect);
}

class TypeDependencyGraph {

	struct NameTy {
		typedef vertex_property_tag kind;
	};

public:
	typedef property<NameTy, std::string> NameProperty;
	typedef boost::adjacency_list<boost::vecS, boost::vecS, boost::directedS, NameProperty> NameDepGraph;

	typedef typename boost::graph_traits<NameDepGraph>::vertex_descriptor vertex_descriptor;
	typedef typename boost::graph_traits<NameDepGraph>::edge_descriptor edge_descriptor;

	NameDepGraph graph;

	TypeDependencyGraph() { }

	void operator()(RecordDecl* tag, typename boost::graph_traits<NameDepGraph>::vertex_descriptor* parent = NULL ) {

		boost::property_map<NameDepGraph, NameTy>::type name = get(NameTy(), graph);

		boost::graph_traits<NameDepGraph>::vertex_iterator vertCurrIt, vertEndIt;
		for(boost::tie(vertCurrIt, vertEndIt) = boost::vertices(graph); vertCurrIt != vertEndIt; vertCurrIt++) {
			if( boost::get(name, *vertCurrIt) == tag->getNameAsString() ) {
				if(parent != NULL) {
					// we have to add an edge between this node and the parent
					boost::add_edge(*parent, *vertCurrIt, graph);
				}
				std :: cout << "found it!!\n";
				return;
			}
		}

		// this node is not inside the graph, we have to add it
		vertex_descriptor v = boost::add_vertex(graph);

		if(parent != NULL) {
			// we have to add an edge between this node and the parent
			boost::add_edge(*parent, v, graph);
		}

		std::cout <<"ADDING EDGE for type: " << tag->getNameAsString() << std::endl;
		boost::put(name, v, tag->getNameAsString());

		for(RecordDecl::field_iterator it=tag->field_begin(), end=tag->field_end(); it != end; ++it) {
			const Type* fieldType = (*it)->getType().getTypePtr();
			if( const PointerType *ptrTy = dyn_cast<PointerType>(fieldType) )
				fieldType = ptrTy->getPointeeType().getTypePtr();
			else if( const ReferenceType *refTy = dyn_cast<ReferenceType>(fieldType) )
				fieldType = refTy->getPointeeType().getTypePtr();

			if( const TagType* tagTy = dyn_cast<TagType>(fieldType) ) {
				assert(isa<RecordDecl>(tagTy->getDecl()));
				(*this)( dyn_cast<RecordDecl>(tagTy->getDecl()), &v );
			}
		}
	}


	void print(std::ostream& out) {
		boost::property_map<NameDepGraph, NameTy>::type name = get(NameTy(), graph);
		boost::write_graphviz(out, graph, boost::make_label_writer(name));
	}
};

} // End empty namespace

namespace insieme {
namespace frontend {
namespace conversion {

/**
 * Used to report recursive types
 */
class RecursiveTypeException: public std::exception {
	Type* recTy;
public:
	RecursiveTypeException(Type* recTy): std::exception(), recTy(recTy) { }

	const Type* getRecType() const { return recTy; }

	const char *what() const throw () { return NULL; }
	virtual ~RecursiveTypeException() throw() { }
};


class ClangExprConverter: public StmtVisitor<ClangExprConverter, ExprWrapper> {
	ConversionFactory& convFact;

public:
	ClangExprConverter(ConversionFactory& convFact): convFact(convFact) { }

	ExprWrapper VisitIntegerLiteral(clang::IntegerLiteral* intLit) {
		return ExprWrapper(
				// retrieve the string representation from the source code
				convFact.builder.literal(
						GetStringFromStream(convFact.clangCtx->getSourceManager(), intLit->getExprLoc()),
						convFact.ConvertType( *intLit->getType().getTypePtr() )
				)
		);
	}

	ExprWrapper VisitFloatingLiteral(clang::FloatingLiteral* floatLit) {
		return ExprWrapper(
				// retrieve the string representation from the source code
				convFact.builder.literal(
						GetStringFromStream(convFact.clangCtx->getSourceManager(), floatLit->getExprLoc()),
						convFact.ConvertType( *floatLit->getType().getTypePtr())
				)
		);
	}

	ExprWrapper VisitStringLiteral(clang::StringLiteral* stringLit) {
		std::ostringstream ss;
		ss << "\"" << stringLit->getStrData() << "\"";
		// todo: Handle escape characters
		return ExprWrapper( convFact.builder.literal(ss.str(), convFact.builder.genericType(core::Identifier("string"))) );
	}

	// CXX Extension for boolean types
	ExprWrapper VisitCXXBoolLiteralExpr(CXXBoolLiteralExpr* boolLit) {
		return ExprWrapper(
				// retrieve the string representation from the source code
				convFact.builder.literal(GetStringFromStream(convFact.clangCtx->getSourceManager(), boolLit->getExprLoc()), core::lang::TYPE_BOOL_PTR)
		);
	}

	ExprWrapper VisitCharacterLiteral(CharacterLiteral* charLit) {
		return ExprWrapper(
				// retrieve the string representation from the source code
				convFact.builder.literal(GetStringFromStream(convFact.clangCtx->getSourceManager(), charLit->getExprLoc()),
						(charLit->isWide() ? convFact.builder.genericType("wchar") : convFact.builder.genericType("char")) )
		);
	}

	ExprWrapper VisitCastExpr(clang::CastExpr* castExpr) {
		return ExprWrapper( convFact.builder.castExpr( convFact.ConvertType( *castExpr->getType().getTypePtr() ), Visit(castExpr->getSubExpr()).ref ) );
	}

	ExprWrapper VisitCallExpr(clang::CallExpr* callExpr) {
		if( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(callExpr->getCalleeDecl()) ) {
			if( funcDecl->isExternC() ) {
				const core::ASTBuilder& builder = convFact.builder;

				vector< core::ExpressionPtr > args;
				std::for_each(callExpr->arg_begin(), callExpr->arg_end(), [ &args, this ](Expr* currArg){ args.push_back( this->Visit(currArg).ref ); });
				return ExprWrapper( convFact.builder.callExpr(
						builder.literal( funcDecl->getNameAsString(), convFact.ConvertType( *funcDecl->getType().getTypePtr() ) ),
						args) );
			}
			return ExprWrapper();
		}
		assert(false && "Call expression not referring to a function");
	}

	ExprWrapper VisitBinaryOperator(clang::BinaryOperator* binOp)  { return ExprWrapper( EmptyExpr(convFact.builder) ); }
	ExprWrapper VisitUnaryOperator(clang::UnaryOperator *unOp) { return ExprWrapper(EmptyExpr(convFact.builder) ); }
	ExprWrapper VisitArraySubscriptExpr(clang::ArraySubscriptExpr* arraySubExpr) { return ExprWrapper( EmptyExpr(convFact.builder) ); }

	ExprWrapper VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
		const core::ASTBuilder& builder = convFact.builder;
		// check whether this is a reference to a variable
		if(VarDecl* varDecl = dyn_cast<VarDecl>(declRef->getDecl())) {
			core::TypePtr&& varTy = convFact.ConvertType( *declRef->getType().getTypePtr() );
			core::Identifier id( declRef->getDecl()->getNameAsString() );
			// if this variable is declared in a method signature
			if(isa<ParmVarDecl>(varDecl)) {
				return ExprWrapper( builder.paramExpr(varTy, id) );
			}
			// else it is a
			return ExprWrapper( builder.varExpr(varTy, id) );
		}
		// todo: C++ check whether this is a reference to a class field, or method (function).
		assert(false && "DeclRefExpr not supported!");
	}

};

#define FORWARD_VISITOR_CALL(StmtTy) \
	StmtWrapper Visit##StmtTy( StmtTy* stmt ) { return StmtWrapper( convFact.ConvertExpr(*stmt) ); }

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
				initExpr = convFact.builder.literal("0.0", type);
			} else if ( ty.isIntegerType() || ty.isUnsignedIntegerType() ) {
				// initialize integer value
				initExpr = convFact.builder.literal("0", type);
			} else if ( ty.isAnyPointerType() || ty.isRValueReferenceType() || ty.isLValueReferenceType() ) {
				// initialize pointer/reference types with the null value
				//todo
			} else if ( ty.isCharType() || ty.isAnyCharacterType() ) {
				//todo
			} else if ( ty.isBooleanType() ) {
				// boolean values are initialized to false
				initExpr = convFact.builder.literal("false", core::lang::TYPE_BOOL_PTR);
			}
		}
		// todo: initialization for declarations with no initialization value
		return StmtWrapper( convFact.builder.declarationStmt( type, varDecl->getNameAsString(), initExpr ) );
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
		return StmtWrapper( convFact.builder.returnStmt( convFact.ConvertExpr( *retStmt->getRetValue() ) ) );
	}

	StmtWrapper VisitForStmt(ForStmt* forStmt) {
		const core::ASTBuilder& builder = convFact.builder;

		StmtWrapper retStmt;
		StmtWrapper&& body = Visit(forStmt->getBody());

		LOG(INFO) << "ForStmt body: " << body;

		ExprWrapper&& incExpr = convFact.ConvertExpr( *forStmt->getInc() );
		// Determine the induction variable
		// analyze the incExpr looking for the induction variable for this loop

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
			condVarDecl->setInit(NULL); // set the expression to null temporarily
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

		Stmt* initStmt = forStmt->getInit();
		// if there is no initialization stmt, we transform the ForStmt into a WhileStmt
		if( !initStmt ) {
			// we are analyzing a loop where the init expression is empty, e.g.:
			// for(; cond; inc) { body }
			//
			// As the IR doesn't support loop stmt with no initialization we represent the for loop as while stmt, i.e.
			// while( cond ) {
			//	{ body }
			//  inc;
			// }
			vector<core::StatementPtr> whileBody;
			// adding the body
			std::copy(body.begin(), body.end(), std::back_inserter(whileBody));
			// adding the incExpr at after the loop body
			whileBody.push_back(incExpr.ref);
			return StmtWrapper( builder.whileStmt( condExpr.ref, builder.compoundStmt(whileBody) ) );
		}

		StmtWrapper&& initExpr = Visit( initStmt );
		if( !initExpr.isSingleStmt() ) {
			assert(core::dynamic_pointer_cast<const core::DeclarationStmt>(initExpr[0]) && "Not a declaration statement");
			// we have a multiple declaration in the initialization part of the stmt
			// e.g.
			// for(int a,b=0; ...)
			//
			// to handle this situation we have to create an outer block and declare the variable which is
			// not used as induction variable
			// todo: WE ASSUME (FOR NOW) THE FIRST DECL IS THE INDUCTION VARIABLE
			std::copy(initExpr.begin()+1, initExpr.end(), std::back_inserter(retStmt));
			initExpr = StmtWrapper( initExpr.front() );
		}

		// We are in the case where we are sure there is exactly 1 element in the initialization expression

		LOG(INFO) << "ForStmt initExpr: " << initExpr;

		core::DeclarationStmtPtr declStmt = core::dynamic_pointer_cast<const core::DeclarationStmt>(initExpr.getSingleStmt());
		assert(declStmt && "Falied loop init expression conversion");
		retStmt.push_back( builder.forStmt(declStmt, body.getSingleStmt(), condExpr.ref, incExpr.ref) );
		if(retStmt.size() == 1)
			return retStmt.front();
		// we have to create a CompoundStmt
		return StmtWrapper( builder.compoundStmt(retStmt) );
	}

	StmtWrapper VisitIfStmt(IfStmt* ifStmt) {
		const core::ASTBuilder& builder = convFact.builder;
		StmtWrapper retStmt;

		core::StatementPtr thenBody = tryAggregateStmts( builder, Visit( ifStmt->getThen() ) );
		assert(thenBody && "Couldn't convert 'then' body of the IfStmt");

		DLOG(INFO) << "IfStmt then body: " << thenBody;

		core::ExpressionPtr condExpr(NULL);
		if( VarDecl* condVarDecl = ifStmt->getConditionVariable() ) {
			assert(ifStmt->getCond() == NULL && "IfStmt condition cannot contains both a variable declaration and an expression");

			// we are in the situation where a variable is declared in the if condition, i.e.:
			// if(int a = ...) { }
			//
			// this will be converted into the following IR representation:
			// { int a = ...; if(a){ } }
			core::DeclarationStmtPtr&& declStmt = core::dynamic_pointer_cast<const core::DeclarationStmt>( VisitVarDecl(condVarDecl).getSingleStmt() );
			retStmt.push_back( declStmt );

			// the expression will be a reference to the declared variable
			condExpr = declStmt->getVarExpression();
		} else {
			Expr* cond = ifStmt->getCond();
			assert(cond && "If statement with no condition.");
			condExpr = convFact.ConvertExpr( *cond );
		}
		assert(condExpr && "Couldn't convert 'condition' expression of the IfStmt");
		DLOG(INFO) << "IfStmt condition expression: " << condExpr;

		core::StatementPtr elseBody(NULL);
		// check for else statement
		if(Stmt* elseStmt = ifStmt->getElse()) {
			elseBody = tryAggregateStmts( builder, Visit( elseStmt ) );
		} else {
			// create an empty compound statement in the case there is no else stmt
			elseBody = builder.compoundStmt();
		}
		assert(elseBody && "Couldn't convert 'else' body of the IfStmt");
		DLOG(INFO) << "IfStmt else body: " << elseBody;

		// adding the ifstmt to the list of returned stmts
		retStmt.push_back( builder.ifStmt(condExpr, thenBody, elseBody) );

		// if we have only 1 statement resulting from the if, we return it
		if( retStmt.isSingleStmt() ) { return retStmt; }

		// otherwise we introduce an outer CompoundStmt
		return StmtWrapper( builder.compoundStmt(retStmt) );
	}

	StmtWrapper VisitWhileStmt(WhileStmt* whileStmt) {
		const core::ASTBuilder& builder = convFact.builder;
		StmtWrapper retStmt;

		core::StatementPtr body = tryAggregateStmts( builder, Visit( whileStmt->getBody() ) );
		assert(body && "Couldn't convert body of the WhileStmt");

		DLOG(INFO) << "WhileStmt then body: " << body;

		core::ExpressionPtr condExpr(NULL);
		if( VarDecl* condVarDecl = whileStmt->getConditionVariable() ) {
			assert(whileStmt->getCond() == NULL && "WhileStmt condition cannot contains both a variable declaration and an expression");

			// we are in the situation where a variable is declared in the if condition, i.e.:
			// while(int a = expr) { }
			//
			// this will be converted into the following IR representation:
			// { int a = 0; while(a = expr){ } }
			Expr* expr = condVarDecl->getInit();
			condVarDecl->setInit(NULL); // set the expression to null temporarily
			core::DeclarationStmtPtr&& declStmt = core::dynamic_pointer_cast<const core::DeclarationStmt>( VisitVarDecl(condVarDecl).getSingleStmt() );
			condVarDecl->setInit(expr);

			retStmt.push_back( declStmt );

			// the expression will be an a = expr
			// condExpr = declStmt->getVarExpression();
			assert(false && "WhileStmt with a declaration of a condition variable not supported");
		} else {
			Expr* cond = whileStmt->getCond();
			assert(cond && "WhileStmt with no condition.");
			condExpr = convFact.ConvertExpr( *cond );
		}
		assert(condExpr && "Couldn't convert 'condition' expression of the WhileStmt");
		DLOG(INFO) << "WhileStmt condition expression: " << condExpr;

		// adding the WhileStmt to the list of returned stmts
		retStmt.push_back( builder.whileStmt(condExpr, body) );

		// if we have only 1 statement resulting from the if, we return it
		if( retStmt.isSingleStmt() ) { return retStmt; }

		// otherwise we introduce an outer CompoundStmt
		return StmtWrapper( builder.compoundStmt(retStmt) );
	}

	StmtWrapper VisitSwitchStmt(SwitchStmt* switchStmt) {
		const core::ASTBuilder& builder = convFact.builder;
		StmtWrapper retStmt;

		core::ExpressionPtr condExpr(NULL);
		if( VarDecl* condVarDecl = switchStmt->getConditionVariable() ) {
			assert(switchStmt->getCond() == NULL && "SwitchStmt condition cannot contains both a variable declaration and an expression");

			core::DeclarationStmtPtr&& declStmt = core::dynamic_pointer_cast<const core::DeclarationStmt>( VisitVarDecl(condVarDecl).getSingleStmt() );
			retStmt.push_back( declStmt );

			// the expression will be a reference to the declared variable
			condExpr = declStmt->getVarExpression();
		} else {
			Expr* cond = switchStmt->getCond();
			assert(cond && "SwitchStmt with no condition.");
			condExpr = convFact.ConvertExpr( *cond );
		}
		assert(condExpr && "Couldn't convert 'condition' expression of the SwitchStmt");

		// Handle the cases of the SwitchStmt
		if( Stmt* body = switchStmt->getBody() ) {
			// this SwitchStmt has a body, i.e.:
			// switch(e) {
			// 	 { body }
			// 	 case x:...
			// ...
			// As the IR doens't allow a body to be represented inside the switch stmt we bring this code outside
			// after the declaration of the eventual conditional variable.
			// TODO: a problem could arise when the body depends on the evaluation of the condition expression, i.e.:
			//		 switch ( a = f() ) {
			//		    b = a+1;
			//			case 1: ...
			//       In this case the a=f() must be assigned to a new variable and replace the occurences of a with the new variable inside
			//		 the switch body

			if(CompoundStmt* compStmt = dyn_cast<CompoundStmt>(body)) {
				std::for_each(compStmt->body_begin(), compStmt->body_end(), [ &retStmt, this ](Stmt* curr){
					if(!isa<SwitchCase>(curr)) {
						StmtWrapper&& visitedStmt = this->Visit(curr);
						std::copy(visitedStmt.begin(), visitedStmt.end(), back_inserter(retStmt));
					}
				});
			}
		}
		vector<core::SwitchStmt::Case> cases;
		// initialize the default case with an empty compoundstmt
		core::StatementPtr defStmt = builder.compoundStmt();

		// the cases can be handled now
		SwitchCase* switchCaseStmt = switchStmt->getSwitchCaseList();
		while(switchCaseStmt) {
			if( CaseStmt* caseStmt = dyn_cast<CaseStmt>(switchCaseStmt) ) {
				core::StatementPtr subStmt(NULL);
				if( Expr* rhs = caseStmt->getRHS() ) {
					assert(!caseStmt->getSubStmt() && "Case stmt cannot have both a RHS and and sub statement.");
					subStmt = convFact.ConvertExpr( *rhs );
				} else if( Stmt* sub = caseStmt->getSubStmt() ) {
					subStmt = tryAggregateStmts( builder, Visit(sub) );
				}
				cases.push_back( std::make_pair(convFact.ConvertExpr( *caseStmt->getLHS() ), subStmt) );
			} else {
				// default case
				DefaultStmt* defCase = dyn_cast<DefaultStmt>(switchCaseStmt);
				assert(defCase && "Case is not the 'default:'.");
				defStmt = tryAggregateStmts( builder, Visit(defCase->getSubStmt()) );
			}
			// next case
			switchCaseStmt = switchCaseStmt->getNextSwitchCase();
		}

		// Appends the switchstmt to the current list of stmt
		retStmt.push_back( builder.switchStmt(condExpr, cases, defStmt) );

		// if the SwitchStmt results in a single IR stmt, return it
		if( retStmt.isSingleStmt() ) return retStmt;
		// otherwise build a CompoundStmt around it and return it
		return StmtWrapper( builder.compoundStmt(retStmt) );
	}

	// as a CaseStmt or DefaultStmt cannot be converted into any IR statements, we generate an error in the case
	// the visitor visits one of these nodes, the VisitSwitchStmt has to make sure the visitor is not called on his subnodes
	StmtWrapper VisitSwitchCase(SwitchCase* caseStmt) { assert(false && "Visitor is visiting a 'case' stmt (cannot compute)"); }

	StmtWrapper VisitBreakStmt(BreakStmt* breakStmt) {
		return StmtWrapper( convFact.builder.breakStmt() );
	}

	StmtWrapper VisitContinueStmt(ContinueStmt* contStmt) {
		return StmtWrapper( convFact.builder.continueStmt() );
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
		return StmtWrapper( convFact.builder.compoundStmt(stmtList) );
	}

	FORWARD_VISITOR_CALL(IntegerLiteral)
	FORWARD_VISITOR_CALL(FloatingLiteral)
	FORWARD_VISITOR_CALL(CharacterLiteral)
	FORWARD_VISITOR_CALL(StringLiteral)

	FORWARD_VISITOR_CALL(BinaryOperator)
	FORWARD_VISITOR_CALL(UnaryOperator)

	FORWARD_VISITOR_CALL(CastExpr)
	FORWARD_VISITOR_CALL(DeclRefExpr)
	FORWARD_VISITOR_CALL(ArraySubscriptExpr)
	FORWARD_VISITOR_CALL(CallExpr)

	StmtWrapper VisitStmt(Stmt* stmt) {
		std::for_each( stmt->child_begin(), stmt->child_end(), [ this ](Stmt* stmt){ this->Visit(stmt); } );
		return StmtWrapper();
	}
};

#define MAKE_SIZE(n)	toVector(core::IntTypeParam::getConcreteIntParam(n))
#define EMPTY_TYPE_LIST	vector<core::TypePtr>()

class ClangTypeConverter: public TypeVisitor<ClangTypeConverter, TypeWrapper> {
	const ConversionFactory& convFact;

	typedef std::vector<Type*> TypeStack;
	TypeStack typeStack;

	typedef std::map<core::TypeVariablePtr, const Type*> RecTypeVarMap;
	typedef std::map<const Type*, core::TypeVariablePtr> TypeRecVarMap;
	TypeRecVarMap mapping;
	map<const Type*, RecTypeVarMap> recTypeVarList;
	map<const Type*, TypeWrapper> recTypeMap;

public:
	ClangTypeConverter(const ConversionFactory& fact): convFact( fact ) { }

	// -------------------- BUILTIN ------------------------------------------------------------------------------
	/**
	 * This method handles buildin types (void,int,long,float,...).
	 */
	TypeWrapper VisitBuiltinType(BuiltinType* buldInTy) {
		const core::ASTBuilder& builder = convFact.builder;

		switch(buldInTy->getKind()) {
		case BuiltinType::Void:
			return TypeWrapper( builder.getUnitType() );
		case BuiltinType::Bool:
			return TypeWrapper( builder.getBoolType() );

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
			return TypeWrapper( builder.getUIntType( SHORT_LENGTH ) );
		case BuiltinType::Short:
			return TypeWrapper( builder.getIntType( SHORT_LENGTH ) );
		case BuiltinType::UInt:
			return TypeWrapper( builder.getUIntType( INT_LENGTH ) );
		case BuiltinType::Int:
			return TypeWrapper( builder.getIntType( INT_LENGTH ) );
		case BuiltinType::UInt128:
			return TypeWrapper( builder.getUIntType( 16 ) );
		case BuiltinType::Int128:
			return TypeWrapper( builder.getIntType( 16 ) );
		case BuiltinType::ULong:
			return TypeWrapper( builder.getUIntType( LONG_LENGTH ) );
		case BuiltinType::ULongLong:
			return TypeWrapper( builder.getUIntType( LONG_LONG_LENGTH ) );
		case BuiltinType::Long:
			return TypeWrapper( builder.getIntType( LONG_LENGTH ) );
		case BuiltinType::LongLong:
			return TypeWrapper( builder.getIntType( LONG_LONG_LENGTH ) );

		// real types
		case BuiltinType::Float:
			return TypeWrapper( builder.getRealType( FLOAT_LENGTH ) );
		case BuiltinType::Double:
			return TypeWrapper( builder.getRealType( DOUBLE_LENGTH ) );
		case BuiltinType::LongDouble:
			return TypeWrapper( builder.getRealType( LONG_DOUBLE_LENGTH ) );

		// not supported types
		case BuiltinType::NullPtr:
		case BuiltinType::Overload:
		case BuiltinType::Dependent:
		case BuiltinType::UndeducedAuto:
		default:
			throw "type not supported"; //todo introduce exception class
		}
		assert(false && "Built-in type conversion not supported!");
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
		return TypeWrapper( convFact.builder.vectorType( convFact.builder.refType(elemTy.ref), core::IntTypeParam::getConcreteIntParam(arrSize) ) );
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

		const core::ASTBuilder& builder = convFact.builder;
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

		const core::ASTBuilder& builder = convFact.builder;
		TypeWrapper elemTy = Visit( arrTy->getElementType().getTypePtr() );
		assert(elemTy.ref && "Conversion of array element type failed.");
		return TypeWrapper( builder.arrayType( builder.refType(elemTy.ref) ) );
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
		const core::ASTBuilder& builder = convFact.builder;
		core::TypePtr retTy = Visit( funcTy->getResultType().getTypePtr() ).ref;
		assert(retTy && "Function has no return type!");

		core::TupleType::ElementTypeList argTypes;
		std::for_each(funcTy->arg_type_begin(), funcTy->arg_type_end(), [ &argTypes, this ](const QualType& currArgType){
			argTypes.push_back( this->Visit( currArgType.getTypePtr() ).ref );
		} );

		if( argTypes.size() == 1 && *argTypes.front() == core::lang::TYPE_UNIT_VAL) {
			// we have only 1 argument, and it is a unit type (void), remove it from the list
			argTypes.clear();
		}

		if( funcTy->isVariadic() )
			argTypes.push_back( core::lang::TYPE_VAR_LIST );

		return TypeWrapper( builder.functionType( builder.tupleType(argTypes), retTy) );
	}

	/**
	 *  Represents a K&R-style 'int foo()' function, which has no information available about its arguments.
	 */
	TypeWrapper VisitFunctionNoProtoType(FunctionNoProtoType* funcTy) {
		core::TypePtr retTy = Visit( funcTy->getResultType().getTypePtr() ).ref;
		assert(retTy && "Function has no return type!");

		return TypeWrapper( convFact.builder.functionType( convFact.builder.tupleType(), retTy) );
	}

	TypeWrapper VisitTypedefType(TypedefType* elabType) {
		assert(false && "TypedefType not yet handled!");
	}

	TypeWrapper VisitTagType(TagType* tagType) {
		// lookup the type map to see if this type has been already converted
		TypeStack::iterator fit = std::find(typeStack.begin(), typeStack.end(), tagType);
		if(fit != typeStack.end())
			throw RecursiveTypeException(tagType);

		// we add the type to the list of traversed types
		typeStack.push_back(tagType);

		TypeWrapper retTy;

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
				assert(false && "Enum types not supported yet");
			} else {
				// handle struct/union/class
				RecordDecl* recDecl = dyn_cast<RecordDecl>(tagDecl);
				assert(recDecl && "TagType decl is not of a RecordDecl type!");

				TypeDependencyGraph typeGraph;
				typeGraph(recDecl);
				typeGraph.print(std::cout);

				core::NamedCompositeType::Entries structElements;
				unsigned short typeVarId=0;
				RecTypeVarMap definitions;
				bool usesPartialType = false;

				for(RecordDecl::field_iterator it=recDecl->field_begin(), end=recDecl->field_end(); it != end; ++it) {
					RecordDecl::field_iterator::value_type curr = *it;
					Type* fieldType = curr->getType().getTypePtr();
					map<const Type*, TypeWrapper>::const_iterator fit = recTypeMap.find(fieldType);
					if(fit != recTypeMap.end()) {
						// the current element uses a type recursively
						// we introduce a TypeVariable as the type of the current field
						structElements.push_back(core::NamedCompositeType::Entry(core::Identifier(curr->getNameAsString()), mapping.find(fieldType)->second) );
					} else {
						try {
							structElements.push_back(
									core::NamedCompositeType::Entry(core::Identifier(curr->getNameAsString()), Visit( fieldType ).ref)
							);
						} catch(const RecursiveTypeException& e) {

							core::TypeVariablePtr recVar(NULL);
							bool newVar = false;
							if(mapping.find(e.getRecType()) == mapping.end()) {
								recVar = core::TypeVariable::get(*this->convFact.mgr, "X" + utils::numeric_cast<std::string>(typeVarId++));
								newVar = true;
							} else
								recVar = mapping.find(e.getRecType())->second;
							// the current element uses a type recursively
							// we introduce a TypeVariable as the type of the current field
							structElements.push_back(core::NamedCompositeType::Entry(core::Identifier(curr->getNameAsString()), recVar) );

							definitions.insert( std::make_pair(recVar,e.getRecType()) );
							if ( newVar ) {
								// add to the var map
								mapping.insert( std::make_pair(e.getRecType(),recVar) );
							}
						}
					}

					// the type of one of the fields is referring to a type which is partially specified
					usesPartialType = (this->recTypeMap.find(fieldType) != this->recTypeMap.end());
				}

				// class and struct are handled in the same way
				if( tagDecl->getTagKind() == TagDecl::TK_struct || tagDecl->getTagKind() ==  TagDecl::TK_class ) {
					if ( usesPartialType || !definitions.empty() ) {
						// this type is using a pratially specified type, before building this type we try to see if we
						// have enough information to build a recursive definition for the sub-type
						recTypeMap.insert( std::make_pair(tagType, retTy) );
					}
					retTy = TypeWrapper( convFact.builder.structType( structElements ) );
					DLOG(INFO) << retTy.ref;
					if( !definitions.empty() ) {
						// we are dealing with a recursive type
						recTypeVarList.insert( std::make_pair(tagType, definitions) );
					}

					// if the type uses recursive types try to build a recursive type using the current knowledge

				} else if( tagDecl->getTagKind() == TagDecl::TK_union )
					retTy = TypeWrapper( convFact.builder.unionType( structElements ) );
				else
					assert(false);
			}
		} else {
			// We didn't find any definition for this type, so we use a name and define it as a generic type
			DLOG(INFO) << convFact.builder.genericType( tagDecl->getNameAsString() )->toString();
			retTy = TypeWrapper( convFact.builder.genericType( tagDecl->getNameAsString() ) );
		}
		typeStack.pop_back();

		if( !recTypeVarList.empty()) {
			// we are exiting from a potentially recursive definition
			// for each temporary recursive type created and stored in recTypeVarList a proper recursive definition has to be created,
			// as at this point we are sure we have all the temporary type necessary to build the recursive definition, we can do it in any order

			// identifying the cycle



//			std::for_each(recTypeVarList.begin(), recTypeVarList.end(), [ this, &convFact ](map<const Type*,RecTypeVarMap>::value_type& curr) {
//
//				core::RecTypeDefinition::RecTypeDefs definitions;
//				std::for_each(curr.second.begin(), curr.second.end(), [this, &definitions](RecTypeVarMap::value_type& curr){
//					map<const Type*, TypeWrapper>::iterator fit = this->recTypeMap.find(curr.second);
//					assert(fit != this->recTypeMap.end() && "Type is not in the map");
//					definitions.insert( std::make_pair(curr.first, fit->second.ref) );
//				});
//				convFact.builder.recTypeDefinition(definitions);
//			});

		}
		return retTy;
	}

	TypeWrapper VisitElaboratedType(ElaboratedType* elabType) {
		assert(false && "ElaboratedType not yet handled!");
	}

	TypeWrapper VisitPointerType(PointerType* pointerTy) {
		return TypeWrapper( convFact.builder.refType( Visit(pointerTy->getPointeeType().getTypePtr()).ref ) );
	}

	TypeWrapper VisitReferenceType(ReferenceType* refTy) {
		return TypeWrapper( convFact.builder.refType( Visit(refTy->getPointeeType().getTypePtr()).ref ) );
	}

};

// ------------------------------------ ConversionFactory ---------------------------

ConversionFactory::ConversionFactory(core::SharedNodeManager mgr): mgr(mgr), builder(mgr),
		typeConv(new ClangTypeConverter(*this)),
		exprConv(new ClangExprConverter(*this)),
		stmtConv(new ClangStmtConverter(*this)) { }

core::TypePtr ConversionFactory::ConvertType(const clang::Type& type) {
	DLOG(INFO) << "Converting type of class:" << type.getTypeClassName();
//	type.dump();
	return typeConv->Visit(const_cast<Type*>(&type)).ref;
}

core::StatementPtr ConversionFactory::ConvertStmt(const clang::Stmt& stmt) {
	DLOG(INFO) << "Converting stmt:";
//	stmt.dump();
	return stmtConv->Visit(const_cast<Stmt*>(&stmt)).getSingleStmt();
}

core::ExpressionPtr ConversionFactory::ConvertExpr(const clang::Expr& expr) {
	DLOG(INFO) << "Converting expression:";
//	expr.dump();
	return exprConv->Visit(const_cast<Expr*>(&expr)).ref;
}

ConversionFactory::~ConversionFactory() {
	delete typeConv;
	delete stmtConv;
	delete exprConv;
}

// ------------------------------------ ClangTypeConverter ---------------------------

void IRConsumer::HandleTopLevelDecl (DeclGroupRef D) {
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
				funcParamList.push_back(
						fact.getASTBuilder().paramExpr( fact.ConvertType( *currParam->getType().getTypePtr() ), currParam->getNameAsString())
				);
			});
			// this is a function decl
			core::StatementPtr funcBody(NULL);
			if(funcDecl->getBody()) {
				funcBody = fact.ConvertStmt( *funcDecl->getBody() );

				core::ExpressionPtr lambaExpr = fact.getASTBuilder().lambdaExpr(funcType, funcParamList, funcBody);

				// annotate name of function
				lambaExpr.addAnnotation(std::make_shared<insieme::c_info::CNameAnnotation>(funcDecl->getName()));

				if(funcDecl->isMain()) {
					program = program->addEntryPoint(lambaExpr);
					//assert((*program->getEntryPoints().begin()).contains(insieme::c_info::CNameAnnotation::KEY) && "Key lost!");
				}
			}

		}else if(VarDecl* varDecl = dyn_cast<VarDecl>(decl)) {
			LOG(INFO) << "Converted into: " << fact.ConvertType( *varDecl->getType().getTypePtr() )->toString();
		}
	}
}

void IRConsumer::HandleTranslationUnit (ASTContext &Ctx) { }

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
