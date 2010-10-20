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
#include <memory>
#include <functional>

#include "logging.h"
#include "conversion.h"

#include "utils/types_lenght.h"
#include "utils/source_locations.h"
#include "utils/dep_graph.h"

#include "analysis/loop_analyzer.h"
#include "analysis/expr_analysis.h"

#include "program.h"
#include "ast_node.h"
#include "types.h"
#include "statements.h"
#include "container_utils.h"
#include "lang_basic.h"
#include "numeric_cast.h"
#include "naming.h"
#include "ocl/ocl_annotations.h"
#include "ast_visitor.h"
#include "container_utils.h"

#include "omp/omp_pragma.h"
#include "transform/node_replacer.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include "clang/Frontend/TextDiagnosticPrinter.h"
#include <boost/algorithm/string.hpp>

using namespace boost;

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

#define GET_TYPE_PTR(type) (type)->getType().getTypePtr()

namespace {

//TODO put it into a class
void printErrorMsg(std::ostringstream& errMsg, const frontend::ClangCompiler& clangComp, const clang::Decl* decl) {
    Diagnostic& diag = clangComp.getDiagnostics();
    TextDiagnosticPrinter* tdc = (TextDiagnosticPrinter*) diag.getClient();
    SourceManager& manager = clangComp.getSourceManager();
    clang::SourceLocation errLoc = decl->getLocStart();
    errMsg << " at location (" << frontend::utils::Line(errLoc, manager) << ":" <<
            frontend::utils::Column(errLoc, manager) << ").\n";


    /*Crashes
    DiagnosticInfo di(&diag);
    tdc->HandleDiagnostic(Diagnostic::Level::Warning, di);*/

    llvm::errs() << errMsg.str();
    tdc->EmitCaretDiagnostic(errLoc, NULL, 0, manager, 0, 0, 80, 0, 0, 0);
}

struct StmtWrapper: public std::vector<core::StatementPtr>{
	StmtWrapper(): std::vector<core::StatementPtr>() { }
	StmtWrapper(const core::StatementPtr& stmt):  std::vector<core::StatementPtr>() { push_back(stmt); }

	core::StatementPtr getSingleStmt() const {
		assert(size() == 1 && "More than 1 statement present");
		return front();
	}

	bool isSingleStmt() const { return size() == 1; }
};

// Returns a string of the text within the source range of the input stream
std::string GetStringFromStream(const SourceManager& srcMgr, const SourceLocation& start) {
	// we use the getDecomposedSpellingLoc() method because in case we read macros values
	// we have to read the expanded value
	std::pair<FileID, unsigned> startLocInfo = srcMgr.getDecomposedSpellingLoc(start);
	llvm::StringRef startBuffer = srcMgr.getBufferData(startLocInfo.first);
	const char *strDataStart = startBuffer.begin() + startLocInfo.second;

	return string(strDataStart, clang::Lexer::MeasureTokenLength(srcMgr.getSpellingLoc(start), srcMgr, clang::LangOptions()));
}

// Tried to aggregate statements into a compound statement (if more than 1 statement is present)
core::StatementPtr tryAggregateStmts(const core::ASTBuilder& builder, const vector<core::StatementPtr>& stmtVect) {
	if( stmtVect.size() == 1 )
		return stmtVect.front();
	return builder.compoundStmt(stmtVect);
}

// in case the the last argument of the function is a var_arg, we try pack the exceeding arguments with the pack
// operation provided by the IR.
vector<core::ExpressionPtr> tryPack(const core::ASTBuilder& builder, core::FunctionTypePtr funcTy, const vector<core::ExpressionPtr>& args) {

	// check if the function type ends with a VAR_LIST type
	core::TupleTypePtr argTy = core::dynamic_pointer_cast<const core::TupleType>(funcTy->getArgumentType());
	assert(argTy && "Function argument is of not type TupleType");

	const core::TupleType::ElementTypeList& elements = argTy->getElementTypes();
	// if the tuple type is empty it means we cannot pack any of the arguments
	if(elements.empty() || elements.size() == args.size())
		return args;

	vector<core::ExpressionPtr> ret;
	if(elements.back() == core::lang::TYPE_VAR_LIST) {
		assert(args.size() >= elements.size()-1 && "Function called with fewer arguments than necessary");
		// last type is a var_list, we have to do the packing of arguments

		// we copy the first N-1 arguments, the remaining will be unpacked
		std::copy(args.begin(), args.begin()+elements.size()-1, std::back_inserter(ret));

		vector<core::ExpressionPtr> toPack;
		std::copy(args.begin()+elements.size()-1, args.end(), std::back_inserter(toPack));

		ret.push_back( builder.callExpr(core::lang::TYPE_UNIT_PTR, core::lang::OP_VAR_LIST_PACK_PTR, toPack) ); //fixme
		return ret;
	}
	return args;
}

std::string getOperationType(const core::TypePtr& type) {
	using namespace core::lang;
	DVLOG(2) << type;
	if(isUIntType(*type))	return "uint";
	if(isIntType(*type)) 	return "int";
	if(isBoolType(*type))	return "bool";
	if(isRealType(*type))	return "real";
    if(isVectorType(*type)) {
        const core::VectorType* vt = dynamic_cast<const core::VectorType*>(&*type);

        const core::TypePtr ref = vt->getElementType();
        std::ostringstream ss;

        if(const core::RefType* subtype = dynamic_cast<const core::RefType*>(&*ref))
            ss << "vector<" << getOperationType(subtype->getElementType()) << ">";
        else
            ss << "vector<" << getOperationType(ref) << ">";

        ss << "vector<" << getOperationType(vt->getElementType()) << ">";
        return ss.str();
    }
    // FIXME
    return "unit";
	// assert(false && "Type not supported");
}

// creates a function call from a list of expressions,
// usefull for implementing the semantics of ++ or -- or comma separated expressions in the IR
core::CallExprPtr createCallExpr(const core::ASTBuilder& builder, const std::vector<core::StatementPtr>& body, core::TypePtr retTy) {

	core::CompoundStmtPtr&& bodyStmt = builder.compoundStmt( body );
	// keeps the list variables used in the body
	insieme::frontend::analysis::VarRefFinder args(bodyStmt);

	core::TupleType::ElementTypeList elemTy;
	core::LambdaExpr::ParamList params;
	std::for_each(args.begin(), args.end(),
		[ &params, &elemTy, &builder, &bodyStmt] (const core::ExpressionPtr& curr) {
			const core::VariablePtr& bodyVar = core::dynamic_pointer_cast<const core::Variable>(curr);
			core::VariablePtr parmVar = builder.variable( bodyVar->getType() );
			params.push_back( parmVar );
			elemTy.push_back( parmVar->getType() );

			// we have to replace the variable of the body with the newly created parmVar
			bodyStmt = core::dynamic_pointer_cast<const core::CompoundStmt>( core::transform::replaceNode(builder, bodyStmt, bodyVar, parmVar) );
			assert(bodyStmt);
		}
	);

	// build the type of the function
	core::FunctionTypePtr funcTy = builder.functionType( builder.tupleType(elemTy), retTy);

	// build the expression body
	core::LambdaExprPtr retExpr = builder.lambdaExpr( funcTy, params, bodyStmt );
	return builder.callExpr( retTy, retExpr, std::vector<core::ExpressionPtr>(args.begin(), args.end()) );
}

// build lambda expression for post/pre increment/decrement unary operators
core::ExpressionPtr encloseIncrementOperator(const core::ASTBuilder& builder, core::ExpressionPtr subExpr, bool post, bool additive) {
	core::RefTypePtr expTy = core::dynamic_pointer_cast<const core::RefType>(subExpr->getType());
	assert( expTy && "LHS operand must of type ref<a'>." );
	const core::TypePtr& subTy = expTy->getElementType();

	core::VariablePtr tmpVar;
	std::vector<core::StatementPtr> stmts;
	if(post) {
		tmpVar = builder.variable(subTy);
		// ref<a'> __tmp = subexpr
		stmts.push_back(builder.declarationStmt(tmpVar,
				builder.callExpr( subTy, core::lang::OP_REF_DEREF_PTR, toVector<core::ExpressionPtr>(subExpr) ) ));
	}
	// subexpr op= 1
	stmts.push_back(
		builder.callExpr(
			core::lang::TYPE_UNIT_PTR,
			core::lang::OP_REF_ASSIGN_PTR,
			toVector<core::ExpressionPtr>(
				subExpr, // ref<a'> a
				builder.callExpr(
					subTy,
					( additive ? core::lang::OP_INT_ADD_PTR : core::lang::OP_INT_SUB_PTR ),
						toVector<core::ExpressionPtr>(
							builder.callExpr( subTy, core::lang::OP_REF_DEREF_PTR, toVector<core::ExpressionPtr>(subExpr) ),
							core::lang::CONST_UINT_ONE_PTR
						)
					) // a - 1
			)
		)
	);
	if(post) {
		assert(tmpVar);
		// return __tmp
		stmts.push_back( builder.returnStmt( tmpVar ) );
	} else {
		// return the variable
		stmts.push_back( builder.callExpr( subTy, core::lang::OP_REF_DEREF_PTR, toVector<core::ExpressionPtr>(subExpr) ) );
	}
	return createCallExpr(builder, std::vector<core::StatementPtr>(stmts), subTy);
}

} // End empty namespace


#define START_LOG_EXPR_CONVERSION(expr) \
	DVLOG(1) << "\n****************************************************************************************\n" \
			 << "Converting expression [class: '" << expr->getStmtClassName() << "']\n" \
			 << "-> at location: (" << utils::location(expr->getLocStart(), convFact.clangComp.getSourceManager()) << "): "; \
	if( VLOG_IS_ON(2) ) { \
		DVLOG(2) << "Dump of clang expression: \n" \
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
		expr->dump(); \
	}

#define END_LOG_EXPR_CONVERSION(expr) \
	DVLOG(1) << "Converted into IR expression: "; \
	DVLOG(1) << "\t" << *expr;

namespace insieme {
namespace frontend {
namespace conversion {

//#############################################################################
//
//							CLANG EXPRESSION CONVERTER
//
//############################################################################
class ConversionFactory::ClangExprConverter: public StmtVisitor<ClangExprConverter, core::ExpressionPtr> {
	ConversionFactory& convFact;

	// Map for resolved lambda functions
	typedef std::map<const FunctionDecl*, core::ExpressionPtr> LambdaExprMap;
	LambdaExprMap lambdaExprCache;

	// this map keeps naming map between the functiondecl and the variable introduced to represent it in the
	// recursive definition
	typedef std::map<const FunctionDecl*, core::VariablePtr> VarMap;
	VarMap	varMap;

	utils::DependencyGraph<const FunctionDecl*> funcDepGraph;

	typedef std::map<const FunctionDecl*, core::VariablePtr> RecVarExprMap;
	RecVarExprMap recVarExprMap;
	bool isRecSubType;
	core::VariablePtr currVar;

public:
	ClangExprConverter(ConversionFactory& convFact): convFact(convFact), isRecSubType(false) { }

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								INTEGER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitIntegerLiteral(clang::IntegerLiteral* intLit) {
		START_LOG_EXPR_CONVERSION(intLit);
		core::ExpressionPtr&& retExpr =
			convFact.builder.literal(
				// retrieve the string representation from the source code
				GetStringFromStream( convFact.clangComp.getSourceManager(), intLit->getExprLoc()),
				convFact.convertType( *GET_TYPE_PTR(intLit) )
			);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								FLOATING LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitFloatingLiteral(clang::FloatingLiteral* floatLit) {
		START_LOG_EXPR_CONVERSION(floatLit);
		core::ExpressionPtr&& retExpr =
			// retrieve the string representation from the source code
			convFact.builder.literal(
				GetStringFromStream( convFact.clangComp.getSourceManager(), floatLit->getExprLoc()),
				convFact.convertType( *GET_TYPE_PTR(floatLit) )
			);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CHARACTER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCharacterLiteral(CharacterLiteral* charLit) {
		START_LOG_EXPR_CONVERSION(charLit);
		core::ExpressionPtr&& retExpr =
			convFact.builder.literal(
				// retrieve the string representation from the source code
				GetStringFromStream(convFact.clangComp.getSourceManager(), charLit->getExprLoc()),
					(charLit->isWide() ? convFact.builder.genericType("wchar") : convFact.builder.genericType("char"))
			);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								STRING LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitStringLiteral(clang::StringLiteral* stringLit) {
		START_LOG_EXPR_CONVERSION(stringLit);
		core::ExpressionPtr&& retExpr =
			convFact.builder.literal(
				GetStringFromStream( convFact.clangComp.getSourceManager(), stringLit->getExprLoc()),
				convFact.builder.genericType(core::Identifier("string"))
			);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CXX BOOLEAN LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXBoolLiteralExpr(CXXBoolLiteralExpr* boolLit) {
		START_LOG_EXPR_CONVERSION(boolLit);
		core::ExpressionPtr&& retExpr =
			// retrieve the string representation from the source code
			convFact.builder.literal(
				GetStringFromStream(convFact.clangComp.getSourceManager(), boolLit->getExprLoc()), core::lang::TYPE_BOOL_PTR
			);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PARENTESIS EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitParenExpr(clang::ParenExpr* parExpr) {
		return Visit( parExpr->getSubExpr() );
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							IMPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitImplicitCastExpr(clang::ImplicitCastExpr* implCastExpr) {
		// we do not convert implicit casts
		START_LOG_EXPR_CONVERSION(implCastExpr);
		return Visit(implCastExpr->getSubExpr());
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCastExpr(clang::CastExpr* castExpr) {
		START_LOG_EXPR_CONVERSION(castExpr);
		const core::TypePtr& type = convFact.convertType( *GET_TYPE_PTR(castExpr) );
		core::ExpressionPtr&& subExpr = Visit(castExpr->getSubExpr());
		return convFact.builder.castExpr( type, subExpr );
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							FUNCTION DECLARATION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitFunctionDecl(const clang::FunctionDecl* funcDecl) {
		// the function is not extern, a lambdaExpr has to be created
		assert(funcDecl->hasBody() && "Function has no body!");

		DVLOG(1) << "\nVisiting Function Declaration for: " << funcDecl->getNameAsString() << std::endl
				 << "\tIsRecSubType: " << isRecSubType << std::endl
				 << "\tEmpty map: "    << recVarExprMap.size();

		if(!isRecSubType) {
			// add this type to the type graph (if not present)
			funcDepGraph.addNode(funcDecl);
			if( VLOG_IS_ON(2) ) {
				funcDepGraph.print( std::cout );
			}
		}

		// retrieve the strongly connected components for this type
		std::set<const FunctionDecl*>&& components = funcDepGraph.getStronglyConnectedComponents( funcDecl );

		if( !components.empty() ) {
			// we are dealing with a recursive type
			DVLOG(1) << "Analyzing FuncDecl: " << funcDecl->getNameAsString() << std::endl
					 << "Number of components in the cycle: " << components.size();
			std::for_each(components.begin(), components.end(),
				[ ] (std::set<const FunctionDecl*>::value_type c) {
					DVLOG(2) << "\t" << c->getNameAsString( ) << "(" << c->param_size() << ")";
				}
			);

			if(!isRecSubType) {

				// we create a TypeVar for each type in the mutual dependence
				recVarExprMap.insert(std::make_pair(funcDecl, convFact.builder.varExpr(
						convFact.convertType( *GET_TYPE_PTR(funcDecl) )
						/*core::Identifier( boost::to_upper_copy(funcDecl->getNameAsString()) )*/))
				);
			} else {
				// we expect the var name to be in currVar
				recVarExprMap.insert(std::make_pair(funcDecl, currVar));
			}

			// when a subtype is resolved we expect to already have these variables in the map
			if(!isRecSubType) {
				std::for_each(components.begin(), components.end(),
					[ this ] (std::set<const FunctionDecl*>::value_type fd) {

						// we count how many variables in the map refers to overloaded versions of the same function
						// this can happen when a function get overloaded and the cycle of recursion can happen between
						// the overloaded version, we need unique variable for each version of the function
//						size_t num_of_overloads = std::count_if(this->recVarExprMap.begin(), this->recVarExprMap.end(),
//							[ &fd ] (RecVarExprMap::value_type curr) {
//								return fd->getName() == curr.first->getName();
//							} );
//
//						std::stringstream recVarName( boost::to_upper_copy(fd->getNameAsString()) );
//						if(num_of_overloads)
//							recVarName << num_of_overloads;

						this->recVarExprMap.insert( std::make_pair(fd, this->convFact.builder.variable(convFact.convertType(*GET_TYPE_PTR(fd)))) );
					}
				);
			}

//			DLOG(INFO) << "MAP: ";
//			std::for_each(recVarExprMap.begin(), recVarExprMap.end(),
//				[] (RecVarExprMap::value_type c) {
//					DLOG(INFO) << "\t" << c.first->getNameAsString() << "[" << (size_t) c.first << "]";
//				}
//			);
		}

		core::ExpressionPtr retLambdaExpr( NULL );
		// this lambda is not yet in the map, we need to create it and add it to the cache
		core::StatementPtr body = convFact.convertStmt( *funcDecl->getBody() );

		vector<core::ParamExprPtr> params;
		std::for_each(funcDecl->param_begin(), funcDecl->param_end(),
			[ &params, this ] (ParmVarDecl* currParam) {
				core::TypePtr paramTy = this->convFact.convertType( *currParam->getOriginalType().getTypePtr() );
				params.push_back( this->convFact.builder.paramExpr( paramTy, core::Identifier(currParam->getName()) ) );
			}
		);

		const core::ASTBuilder& builder = convFact.builder;
		retLambdaExpr = builder.lambdaExpr( convFact.convertType( *GET_TYPE_PTR(funcDecl) ), params, body);

		if( !components.empty() ) {
			// this is a recurive function call
			if(isRecSubType) {
				// if we are visiting a nested recursive type it means someone else will take care
				// of building the rectype node, we just return an intermediate type
				return retLambdaExpr;
			}

			// we have to create a recursive type
			RecVarExprMap::const_iterator tit = recVarExprMap.find(funcDecl);
			assert(tit != recVarExprMap.end() && "Recursive function has not VarExpr associated to himself");
			core::VarExprPtr recVarRef = tit->second;

			core::RecLambdaDefinition::RecFunDefs definitions;
			definitions.insert( std::make_pair(recVarRef, core::dynamic_pointer_cast<const core::LambdaExpr>(retLambdaExpr)) );

			// We start building the recursive type. In order to avoid loop the visitor
			// we have to change its behaviour and let him returns temporarely types
			// when a sub recursive type is visited.
			isRecSubType = true;

			std::for_each(components.begin(), components.end(),
				[ this, &definitions ] (std::set<const FunctionDecl*>::value_type fd) {

					RecVarExprMap::const_iterator tit = recVarExprMap.find(fd);
					assert(tit != recVarExprMap.end() && "Recursive function has no TypeVar associated");
					currVar = tit->second;

					// we remove the variable from the list in order to fool the solver,
					// in this way it will create a descriptor for this type (and he will not return the TypeVar
					// associated with this recursive type). This behaviour is enabled only when the isRecSubType
					// flag is true
					recVarExprMap.erase(fd);
					definitions.insert( std::make_pair(currVar, core::dynamic_pointer_cast<const core::LambdaExpr>(this->VisitFunctionDecl(fd)) ) );

					// reinsert the TypeVar in the map in order to solve the other recursive types
					recVarExprMap.insert( std::make_pair(fd, currVar) );
					currVar = NULL;
				}
			);
			// we reset the behavior of the solver
			isRecSubType = false;
			// the map is also erased so visiting a second type of the mutual cycle will yield a correct result
			recVarExprMap.clear();

			core::RecLambdaDefinitionPtr definition = builder.recLambdaDefinition(definitions);
			retLambdaExpr = builder.recLambdaExpr(recVarRef, definition);
		}

		return retLambdaExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							FUNCTION CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCallExpr(clang::CallExpr* callExpr) {
		START_LOG_EXPR_CONVERSION(callExpr);
		if( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(callExpr->getDirectCallee()) ) {
			const core::ASTBuilder& builder = convFact.builder;

			// collects the type of each argument of the expression
			vector< core::ExpressionPtr > args;
			std::for_each(callExpr->arg_begin(), callExpr->arg_end(),
				[ &args, this ] (Expr* currArg) { args.push_back( this->Visit(currArg) ); }
			);

			core::FunctionTypePtr funcTy = core::dynamic_pointer_cast<const core::FunctionType>( convFact.convertType( *GET_TYPE_PTR(funcDecl) ) );

			vector< core::ExpressionPtr >&& packedArgs = tryPack(convFact.builder, funcTy, args);

			const FunctionDecl* definition = NULL;
			if( !funcDecl->hasBody(definition) ) {
				// in the case the function is extern, a literal is build

				core::ExpressionPtr irNode = convFact.builder.callExpr(	builder.literal(funcDecl->getNameAsString(), funcTy), packedArgs );
				// handle eventual pragmas attached to the Clang node
				frontend::omp::attachOmpAnnotation(irNode, callExpr, convFact);

				return irNode;
			}

			if(!recVarExprMap.empty()) {
				// check if this type has a typevar already associated, in such case return it
				RecVarExprMap::const_iterator fit = recVarExprMap.find(definition);
				if( fit != recVarExprMap.end() ) {
					// we are resolving a parent recursive type, so we shouldn't
					return builder.callExpr(fit->second, packedArgs);
				}
			}

			if(!isRecSubType) {
				LambdaExprMap::const_iterator fit = lambdaExprCache.find(definition);
				if(fit != lambdaExprCache.end()) {
					core::ExpressionPtr irNode = builder.callExpr(fit->second, packedArgs);
					// handle eventual pragmas attached to the Clang node
					frontend::omp::attachOmpAnnotation(irNode, callExpr, convFact);

					return irNode;
				}
			}

			assert(definition && "No definition found for function");
			core::ExpressionPtr lambdaExpr = VisitFunctionDecl(definition);

			// Adding the C function name as annotation
			lambdaExpr.addAnnotation(std::make_shared<insieme::c_info::CNameAnnotation>(definition->getName()));

			// Adding the lambda function to the list of converted functions
			lambdaExprCache.insert( std::make_pair(definition, lambdaExpr) );

			core::ExpressionPtr irNode = builder.callExpr(lambdaExpr, packedArgs);
			// handle eventual pragmas attached to the Clang node
			frontend::omp::attachOmpAnnotation(irNode, callExpr, convFact);
			return irNode;
		}
		assert(false && "Call expression not referring a function");
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX MEMBER CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* callExpr) {
		//todo: CXX extensions
		assert(false && "CXXMemberCallExpr not yet handled");
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						CXX OPERATOR CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCXXOperatorCallExprr(clang::CXXOperatorCallExpr* callExpr) {
		//todo: CXX extensions
		assert(false && "CXXOperatorCallExpr not yet handled");
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							BINARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitBinaryOperator(clang::BinaryOperator* binOp)  {
		START_LOG_EXPR_CONVERSION(binOp);
		const core::ASTBuilder& builder = convFact.builder;

 		core::ExpressionPtr&& rhs = Visit(binOp->getRHS());
		core::ExpressionPtr&& lhs = Visit(binOp->getLHS());

		// if the binary operator is a comma separated expression, we convert it into a function call
		// which returns the value of the last expression
		if( binOp->getOpcode() == BO_Comma) {
			return createCallExpr(builder, toVector<core::StatementPtr>(lhs, builder.returnStmt(rhs)), rhs->getType());
		}

		core::TypePtr exprTy = convFact.convertType( *GET_TYPE_PTR(binOp) );

		// create Pair type
		core::TupleTypePtr tupleTy = builder.tupleType( { exprTy, exprTy } );
		std::string opType = getOperationType(exprTy);

		// we take care of compound operators first,
		// we rewrite the RHS expression in a normal form, i.e.:
		// a op= b  ---->  a = a op b
		std::string op;
		clang::BinaryOperatorKind baseOp;
		switch( binOp->getOpcode() ) {
		// a *= b
		case BO_MulAssign: op = "mul"; baseOp = BO_Mul; break;
		// a /= b
		case BO_DivAssign: op = "div"; baseOp = BO_Div; break;
		// a %= b
		case BO_RemAssign: op = "mod"; baseOp = BO_Rem; break;
		// a += b
		case BO_AddAssign: op = "add"; baseOp = BO_Add; break;
		// a -= b
		case BO_SubAssign: op = "sub"; baseOp = BO_Sub; break;
		// a <<= b
		case BO_ShlAssign: op = "shl"; baseOp = BO_Shl; break;
		// a >>= b
		case BO_ShrAssign: op = "shr"; baseOp = BO_Shr; break;
		// a &= b
		case BO_AndAssign: op = "and"; baseOp = BO_And; break;
		// a |= b
		case BO_OrAssign: op = "or"; baseOp = BO_Or; break;
		// a ^= b
		case BO_XorAssign: op = "xor"; baseOp = BO_Xor; break;
		default:
			break;
		}

		if( !op.empty() ) {
			// The operator is a compound operator, we substitute the RHS expression with the expanded one
			assert( core::dynamic_pointer_cast<const core::RefType>(lhs->getType()) && "LHS operand must of type ref<a'>." );
			core::lang::OperatorPtr&& opFunc = builder.literal( opType + "." + op, builder.functionType(tupleTy, exprTy));

			// we check if the RHS is a ref, in that case we use the deref operator
			if( core::dynamic_pointer_cast<const core::RefType>(rhs->getType()) )
				rhs = builder.callExpr( core::lang::OP_REF_DEREF_PTR, toVector(rhs) );
			rhs = builder.callExpr(opFunc,
				toVector<core::ExpressionPtr>(builder.callExpr( core::lang::OP_REF_DEREF_PTR, toVector(lhs) ), rhs) );
			// add an annotation to the subexpression
			opFunc->addAnnotation( std::make_shared<c_info::COpAnnotation>( BinaryOperator::getOpcodeStr(baseOp)) );
		}

		bool isAssignment = false;
		baseOp = binOp->getOpcode();

		switch( binOp->getOpcode() ) {
		case BO_PtrMemD:
		case BO_PtrMemI:
			assert(false && "Operator not yet supported!");

		// a * b
		case BO_Mul: 	op = "mul";  break;
		// a / b
		case BO_Div: 	op = "div";  break;
		// a % b
		case BO_Rem: 	op = "mod";  break;
		// a + b
		case BO_Add: 	op = "add";  break;
		// a - b
		case BO_Sub: 	op = "sub";  break;
		// a << b
		case BO_Shl: 	op = "shl";  break;
		// a >> b
		case BO_Shr: 	op = "shr";  break;
		// a & b
		case BO_And: 	op = "and";  break;
		// a ^ b
		case BO_Xor: 	op = "xor";  break;
		// a | b
		case BO_Or:  	op = "or"; 	 break;

		// Logic operators

		// a && b
		case BO_LAnd: 	op = "land"; break;
		// a || b
		case BO_LOr:  	op = "lor";  break;
		// a < b
		case BO_LT:	 	op = "lt";   break;
		// a > b
		case BO_GT:  	op = "gt";   break;
		// a <= b
		case BO_LE:  	op = "le";   break;
		// a >= b
		case BO_GE:  	op = "ge";   break;
		// a == b
		case BO_EQ:  	op = "eq";   break;
		// a != b
		case BO_NE:	 	op = "ne";   break;

		case BO_MulAssign: case BO_DivAssign: case BO_RemAssign: case BO_AddAssign: case BO_SubAssign:
		case BO_ShlAssign: case BO_ShrAssign: case BO_AndAssign: case BO_XorAssign: case BO_OrAssign:
		case BO_Assign:
			baseOp = BO_Assign;
			DLOG(INFO) << *lhs;
			DLOG(INFO) << *lhs->getType();
			// This is an assignment, we have to make sure the LHS operation is of type ref<a'>
			assert( core::dynamic_pointer_cast<const core::RefType>(lhs->getType()) && "LHS operand must of type ref<a'>." );
			exprTy = lhs->getType();
			opType = "ref";
			isAssignment = true;
			op = "assign"; break;

		default:
			assert(false && "Operator not supported");
		}

		const core::lang::OperatorPtr& opFunc = builder.literal( opType + "." + op, builder.functionType(tupleTy, exprTy));

		// build a callExpr with the 2 arguments
		if( core::dynamic_pointer_cast<const core::RefType>(rhs->getType()) )
			rhs = builder.callExpr( core::lang::OP_REF_DEREF_PTR, toVector(rhs) );
		if( !isAssignment && core::dynamic_pointer_cast<const core::RefType>(lhs->getType()) )
			lhs = builder.callExpr( core::lang::OP_REF_DEREF_PTR, toVector(lhs) );

		core::ExpressionPtr retExpr = convFact.builder.callExpr( opFunc, toVector(lhs, rhs) );

		// add the operator name in order to help the convertion process in the backend
		opFunc->addAnnotation( std::make_shared<c_info::COpAnnotation>( BinaryOperator::getOpcodeStr(baseOp) ) );

		// handle eventual pragmas attached to the Clang node
		frontend::omp::attachOmpAnnotation(retExpr, binOp, convFact);

		END_LOG_EXPR_CONVERSION( retExpr );
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							UNARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitUnaryOperator(clang::UnaryOperator *unOp) {
		START_LOG_EXPR_CONVERSION(unOp);
		const core::ASTBuilder& builder = convFact.builder;
		core::ExpressionPtr&& subExpr = Visit(unOp->getSubExpr());

		bool post = true;
		switch(unOp->getOpcode()) {
		// conversion of post increment/decrement operation is done by creating a tuple expression i.e.:
		// a++ ==> (__tmp = a, a=a+1, __tmp)
		// ++a ==> ( a=a+1, a)
		// --a
		case UO_PreDec:
			post = false;
		// a--
		case UO_PostDec:
			subExpr = encloseIncrementOperator(builder, subExpr, post, false);
			break;
		// a++
		case UO_PreInc:
			post = false;
		// ++a
		case UO_PostInc:
			subExpr = encloseIncrementOperator(builder, subExpr, post, true);
			break;
		// &a
		case UO_AddrOf:
			// assert(false && "Conversion of AddressOf operator '&' not supported");
			break;
		// *a
		case UO_Deref:
			// return ExprWrapper( builder.callExpr( core::lang::OP_REF_DEREF_PTR, {subExpr} ) );
			subExpr = builder.callExpr( core::lang::OP_REF_DEREF_PTR, toVector(subExpr) );
			break;
		// +a
		case UO_Plus:
			// just return the subexpression
			break;
		// -a
		case UO_Minus:
			// TODO:
			// assert(false && "Conversion of unary operator '-' not supported");
		// ~a
		case UO_Not:
			// TODO:
			// assert(false && "Conversion of unary operator '~' not supported");
		// !a
		case UO_LNot:
			// TODO:
			// assert(false && "Conversion of unary operator '!' not supported");

		case UO_Real:
		case UO_Imag:
		case UO_Extension: //TODO:
		default:
			break;
			// assert(false && "Unary operator not supported");
		}

		// handle eventual pragmas attached to the Clang node
		frontend::omp::attachOmpAnnotation(subExpr, unOp, convFact);

		// add the operator name in order to help the convertion process in the backend
		subExpr->addAnnotation( std::make_shared<c_info::COpAnnotation>( UnaryOperator::getOpcodeStr(unOp->getOpcode()) ) );

		return subExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							CONDITIONAL OPERATOR FIXME
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitConditionalOperator(clang::ConditionalOperator* condOp) {
		core::TypePtr&& retTy = convFact.convertType( *GET_TYPE_PTR(condOp) );

		core::ExpressionPtr&& trueExpr = Visit(condOp->getTrueExpr());
		core::ExpressionPtr&& falseExpr = Visit(condOp->getFalseExpr());
		core::ExpressionPtr&& condExpr = Visit( condOp->getCond() );
		core::StatementPtr ifStmt = convFact.builder.ifStmt(condExpr, trueExpr, falseExpr);

		return createCallExpr( convFact.builder, toVector( ifStmt ),  retTy);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						ARRAY SUBSCRIPT EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitArraySubscriptExpr(clang::ArraySubscriptExpr* arraySubExpr) {
		START_LOG_EXPR_CONVERSION(arraySubExpr);
		core::ExpressionPtr&& base = Visit( arraySubExpr->getBase() );

//		if( !core::dynamic_pointer_cast<const core::VectorType>(base->getType()) ) {
//			// CLANG doesn't recognize this as a vector type, but a subscript operator has been called,
//			// so we assume this was originally a vector type
//
//		}

		core::ExpressionPtr&& idx = Visit( arraySubExpr->getIdx() );

//		TODO: we need better checking for vector type
//		assert( (core::dynamic_pointer_cast<const core::VectorType>( base->getType() ) ||
//				core::dynamic_pointer_cast<const core::ArrayType>( base->getType() )) && "Base expression of array subscript is not a vector/array type.");

		core::ExpressionPtr&& retExpr =
			convFact.builder.callExpr(
				convFact.builder.refType( convFact.convertType( *GET_TYPE_PTR(arraySubExpr) ) ),
				core::lang::OP_SUBSCRIPT_PTR,
				toVector<core::ExpressionPtr>(
					convFact.builder.callExpr(core::lang::OP_REF_DEREF_PTR, toVector(base)),
					idx
				)
			);
//		DLOG(INFO) << "EXPR_TY: " << *retExpr->getType();
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						EXT VECTOR ELEMENT EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitExtVectorElementExpr(ExtVectorElementExpr* vecElemExpr){
        START_LOG_EXPR_CONVERSION(vecElemExpr);
        core::ExpressionPtr&& base = Visit( vecElemExpr->getBase() );

        std::string pos;
        llvm::StringRef&& accessor = vecElemExpr->getAccessor().getName();

        //translate OpenCL accessor string to index
        if(accessor == "x") 		pos = "0";
        else if(accessor == "y")    pos = "1";
        else if(accessor == "z")	pos = "2";
        else if(accessor == "w")	pos = "3";
        else {
        	// the input string is in a form sXXX
        	assert(accessor.front() == 's');
        	// we skip the s and return the value to get the number
        	llvm::StringRef numStr = accessor.substr(1,accessor.size()-1);
        	assert(insieme::utils::numeric_cast<unsigned int>(numStr.data()) >= 0 && "String is not a number");
        	pos = numStr;
        }
        core::ExpressionPtr&& idx = convFact.builder.literal(pos, convFact.convertType( *GET_TYPE_PTR(vecElemExpr) ));
        core::ExpressionPtr&& retExpr = convFact.builder.callExpr(core::lang::OP_SUBSCRIPT_PTR, toVector( base, idx ));
        END_LOG_EXPR_CONVERSION(retExpr);
        return retExpr;
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VAR DECLARATION REFERENCE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
		START_LOG_EXPR_CONVERSION(declRef);
		const core::ASTBuilder& builder = convFact.builder;
		// check whether this is a reference to a variable
		if(Decl* varDecl = declRef->getDecl()) {
			core::TypePtr varTy = convFact.convertType( *GET_TYPE_PTR(declRef) );

			// FIXME: is this correct?
			if(!core::dynamic_pointer_cast<const core::RefType>(varTy))
				varTy = builder.refType(varTy);

			core::Identifier id( declRef->getDecl()->getNameAsString() );
			// if this variable is declared in a method signature
			if(isa<ParmVarDecl>(varDecl)) {
				return builder.paramExpr(varTy, id);
			}
			// else it is a
			return builder.varExpr(varTy, id);
		}
		// todo: C++ check whether this is a reference to a class field, or method (function).
		assert(false && "DeclRefExpr not supported!");
	}

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //                       VECTOR INITALIZATION EXPRESSION
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitInitListExpr(clang::InitListExpr* initList) {
        START_LOG_EXPR_CONVERSION(initList);
        std::vector<core::ExpressionPtr> elements;

        // get all values of the init expression
        for(size_t i = 0, end = initList->getNumInits(); i < end; ++i) {
             elements.push_back( convFact.convertExpr(*(initList->getInit(i))) );
        }

        // create vector initializator
        core::ExpressionPtr retExpr = convFact.builder.vectorExpr(elements);

        END_LOG_EXPR_CONVERSION(retExpr);
        return retExpr;
    }
};

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							Printing macros for statements
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#define FORWARD_VISITOR_CALL(StmtTy) \
	StmtWrapper Visit##StmtTy( StmtTy* stmt ) { return StmtWrapper( convFact.convertExpr(*stmt) ); }

#define START_LOG_STMT_CONVERSION(stmt) \
	DVLOG(1) << "\n****************************************************************************************\n" \
			 << "Converting statement [class: '" << stmt->getStmtClassName() << "'] \n" \
			 << "-> at location: (" << utils::location(stmt->getLocStart(), convFact.clangComp.getSourceManager()) << "): "; \
	if( VLOG_IS_ON(2) ) { \
		DVLOG(2) << "Dump of clang statement:\n" \
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
		stmt->dump(convFact.clangComp.getSourceManager()); \
	}

#define END_LOG_STMT_CONVERSION(stmt) \
	DVLOG(1) << "Converted 'statement' into IR stmt: "; \
	DVLOG(1) << "\t" << *stmt;


//#############################################################################
//
//							CLANG STMT CONVERTER
//
//############################################################################
class ConversionFactory::ClangStmtConverter: public StmtVisitor<ClangStmtConverter, StmtWrapper> {
	ConversionFactory& convFact;
private:

	core::ExpressionPtr defaultInitVal(const clang::Type& ty, const core::TypePtr type ) {
        if ( ty.isIntegerType() || ty.isUnsignedIntegerType() ) {
            // initialize integer value
            return convFact.builder.literal("0", type);
        }
        if( ty.isFloatingType() || ty.isRealType() || ty.isRealFloatingType() ) {
            // in case of floating types we initialize with a zero value
            return convFact.builder.literal("0.0", type);
        }
        if ( ty.isAnyPointerType() || ty.isRValueReferenceType() || ty.isLValueReferenceType() ) {
            // initialize pointer/reference types with the null value
            return core::lang::CONST_NULL_PTR_PTR;
        }
        if ( ty.isCharType() || ty.isAnyCharacterType() ) {
            // TODO
            return core::lang::CONST_NULL_PTR_PTR;
        }
        if ( ty.isBooleanType() ) {
            // boolean values are initialized to false
            return convFact.builder.literal("false", core::lang::TYPE_BOOL_PTR);
        }

        //----------------  INTIALIZE VECTORS ---------------------------------
        const Type* elemTy = NULL;
        size_t arraySize = 0;
        if ( ty.isExtVectorType() ) {
        	const TypedefType* typedefType = dyn_cast<const TypedefType>(&ty);
            assert(typedefType && "ExtVectorType has unexpected class");
            const ExtVectorType* vecTy = dyn_cast<const ExtVectorType>( typedefType->getDecl()->getUnderlyingType().getTypePtr() );
            assert(vecTy && "ExtVectorType has unexpected class");

			elemTy = vecTy->getElementType()->getUnqualifiedDesugaredType();
			arraySize = vecTy->getNumElements();
        }
        if ( ty.isConstantArrayType() ) {
        	const ConstantArrayType* arrTy = dyn_cast<const ConstantArrayType>(&ty);
			assert(arrTy && "ConstantArrayType has unexpected class");

			elemTy = arrTy->getElementType()->getUnqualifiedDesugaredType();
			arraySize = *arrTy->getSize().getRawData();
        }
        if( ty.isExtVectorType() || ty.isConstantArrayType() ) {
        	core::ExpressionPtr&& initVal = defaultInitVal(*elemTy, convFact.convertType( *elemTy ) );
        	return convFact.builder.vectorExpr( std::vector<core::ExpressionPtr>(arraySize, initVal) );
        }

        assert(false && "default initialization type not defined");
        return core::lang::CONST_NULL_PTR_PTR;
	}

public:

	ClangStmtConverter(ConversionFactory& convFact): convFact(convFact) { }

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						VARIABLE DECLARATION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::DeclarationStmtPtr VisitVarDecl(clang::VarDecl* varDecl) {

		// logging
		DVLOG(1) << "\n****************************************************************************************\n"
				 << "Converting VarDecl [class: '" << varDecl->getDeclKindName() << "']\n"
				 << "-> at location: (" << utils::location(varDecl->getLocation(), convFact.clangComp.getSourceManager()) << "): ";
		if( VLOG_IS_ON(2) ) { \
			DVLOG(2) << "Dump of clang VarDecl: \n"
					 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
			varDecl->dump();
		}

		clang::QualType clangType = varDecl->getType();
		if(!clangType.isCanonical())
			clangType = clangType->getCanonicalTypeInternal();

		// we cannot analyze if the variable will be modified or not, so we make it of type ref<a'> if
        // it is not declared as const, successive dataflow analysis could be used to restrict the access
		// to this variable
        core::TypePtr type = clangType.isConstQualified() ?
            convFact.convertType( *GET_TYPE_PTR(varDecl) ) :
            convFact.builder.refType( convFact.convertType( *GET_TYPE_PTR(varDecl) ) );
		// todo: initialization for declarations with no initialization value

		// initialization value
		core::ExpressionPtr initExpr;
		if( varDecl->getInit() ) {
			initExpr = convFact.convertExpr( *varDecl->getInit() );
		} else {
		    initExpr = defaultInitVal(* GET_TYPE_PTR(varDecl), type);
		}

        core::DeclarationStmtPtr&& retStmt = convFact.builder.declarationStmt( type, varDecl->getNameAsString(), initExpr );

        /*-------------------------><-----------------------*/
        core::AnnotationPtr&& attr = convFact.convertClangAttributes(varDecl);
        if(attr)
        	retStmt->addAnnotation(attr);

        // logging
        DVLOG(1) << "Converted into IR stmt: "; \
    	DVLOG(1) << "\t" << *retStmt;

    	return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							DECLARATION STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// In clang a declstmt is represented as a list of VarDecl
	StmtWrapper VisitDeclStmt(clang::DeclStmt* declStmt) {
		// if there is only one declaration in the DeclStmt we return it
		if( declStmt->isSingleDecl() && isa<clang::VarDecl>(declStmt->getSingleDecl()) )
			return StmtWrapper( VisitVarDecl( dyn_cast<clang::VarDecl>(declStmt->getSingleDecl()) ) );

		// otherwise we create an an expression list which contains the multiple declaration inside the statement
		StmtWrapper retList;
		for(clang::DeclStmt::decl_iterator it = declStmt->decl_begin(), e = declStmt->decl_end(); it != e; ++it)
			if( clang::VarDecl* varDecl = dyn_cast<clang::VarDecl>(*it) )
				retList.push_back( VisitVarDecl(varDecl) );
		return retList;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							RETURN STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitReturnStmt(ReturnStmt* retStmt) {
		START_LOG_STMT_CONVERSION(retStmt);
		assert(retStmt->getRetValue() && "ReturnStmt has an empty expression");

		core::StatementPtr ret = convFact.builder.returnStmt( convFact.convertExpr( *retStmt->getRetValue() ) );
		// handle eventual OpenMP pragmas attached to the Clang node
		frontend::omp::attachOmpAnnotation(ret, retStmt, convFact);

		END_LOG_STMT_CONVERSION( ret );
		return StmtWrapper( ret );
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								FOR STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitForStmt(ForStmt* forStmt) {
		START_LOG_STMT_CONVERSION(forStmt);
		const core::ASTBuilder& builder = convFact.builder;
		VLOG(2) << "{ Visit ForStmt }";

		StmtWrapper retStmt;
		StmtWrapper&& body = Visit(forStmt->getBody());

		try {
			// Analyze loop for induction variable
			analysis::LoopAnalyzer loopAnalysis(forStmt, convFact);

			core::ExpressionPtr&& incExpr = loopAnalysis.getIncrExpr();
			core::ExpressionPtr&& condExpr = loopAnalysis.getCondExpr();

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
				whileBody.push_back( convFact.convertExpr( *forStmt->getInc() ) );

				core::StatementPtr&& whileStmt =  builder.whileStmt( convFact.convertExpr( *forStmt->getCond() ), builder.compoundStmt(whileBody) );

				// handle eventual pragmas attached to the Clang node
				frontend::omp::attachOmpAnnotation(whileStmt, forStmt, convFact);

				END_LOG_STMT_CONVERSION( whileStmt );
				return StmtWrapper( whileStmt );
			}

			StmtWrapper&& initExpr = Visit( initStmt );
			if( !initExpr.isSingleStmt() ) {
				assert(core::dynamic_pointer_cast<const core::DeclarationStmt>(initExpr[0]) && "Not a declaration statement");
				// we have a multiple declaration in the initialization part of the stmt
				// e.g.
				// for(int a,b=0; ...)
				//
				// to handle this situation we have to create an outer block in order to declare the variables which are
				// not used as induction variable
				const VarDecl* inductionVar = loopAnalysis.getInductionVar();

				std::function<bool (const core::StatementPtr&, bool)> inductionVarFilter =
					[inductionVar](const core::StatementPtr& curr, bool negate) {
						core::DeclarationStmtPtr&& declStmt = core::dynamic_pointer_cast<const core::DeclarationStmt>(curr);
						assert(declStmt && "Not a declaration statement");
						bool ret = declStmt->getVarExpression()->getIdentifier() == inductionVar->getNameAsString();
						return negate ? !ret : ret;
					};

				// we insert all the variable declarations (excluded the induction variable) before the body of the for loop
				std::copy_if(initExpr.begin(), initExpr.end(), std::back_inserter(retStmt), std::bind( inductionVarFilter, std::placeholders::_1, true ) );
				//
				std::vector<core::StatementPtr>::const_iterator fit =
						std::find_if(initExpr.begin(), initExpr.end(), std::bind( inductionVarFilter, std::placeholders::_1, false ));
				assert(fit != initExpr.end() && "Induction variable not declared in the loop initialization expression");
				initExpr = *fit;
			}

			// We are in the case where we are sure there is exactly 1 element in the initialization expression
			core::DeclarationStmtPtr declStmt = core::dynamic_pointer_cast<const core::DeclarationStmt>( initExpr.getSingleStmt() );
			bool iteratorChanged = false;
			core::VarExprPtr newIndVar;
			if( !declStmt ) {
				// the init expression is not a declaration stmt, it could be a situation where it is an assignment operation:
				// for( i=0; ...)
				core::ExpressionPtr&& init = core::dynamic_pointer_cast<const core::Expression>( initExpr.getSingleStmt() );
				assert(init);

				// we have to define a new induction variable for the loop and replace every instance in the loop with the new variable
				std::string varName = std::string("__") + loopAnalysis.getInductionVar()->getNameAsString();
				DVLOG(2) << "Substituting loop induction variable: " << loopAnalysis.getInductionVar()->getNameAsString()
						<< " with variable: " << varName;

				core::TypePtr varTy = convFact.convertType( *GET_TYPE_PTR(loopAnalysis.getInductionVar()) );
				newIndVar = builder.varExpr(varTy, core::Identifier(varName));

				declStmt = builder.declarationStmt( builder.refType(varTy), core::Identifier(varName), core::lang::CONST_UINT_ZERO_PTR );

				DVLOG(2) << "Printing body: " << body;
				core::NodePtr ret = core::transform::replaceNode(convFact.builder, body.getSingleStmt(),
						builder.varExpr(builder.refType(varTy), core::Identifier(loopAnalysis.getInductionVar()->getNameAsString())),
						newIndVar);

				// replace the body with the newly modified one
				body = StmtWrapper( core::dynamic_pointer_cast<const core::Statement>(ret) );

				// we have to remember that the iterator has been changed for this loop
				iteratorChanged = true;
			}

			assert(declStmt && "Falied loop init expression conversion");
			// We finally create the IR ForStmt
			core::ForStmtPtr irFor = builder.forStmt(declStmt, body.getSingleStmt(), condExpr, incExpr);
			assert(irFor && "Created for statement is not valid");

			// handle eventual pragmas attached to the Clang node
			frontend::omp::attachOmpAnnotation(irFor, forStmt, convFact);

			retStmt.push_back( irFor );

			if(iteratorChanged) {
				// in the case we replace the loop iterator with a temporary variable, we have to assign the final value of the
				// iterator to the old variable so we don't change the semantics of the code
				core::TypePtr varTy = convFact.convertType( *GET_TYPE_PTR(loopAnalysis.getInductionVar()) );
				const core::lang::OperatorPtr& refAssign = builder.literal( "ref.assign", core::lang::TYPE_OP_ASSIGN_PTR);

				retStmt.push_back( builder.callExpr( refAssign,
					toVector<core::ExpressionPtr>(
						builder.varExpr(varTy, core::Identifier(loopAnalysis.getInductionVar()->getNameAsString())), // ref<a'> a
						loopAnalysis.getCondExpr()
					)
				));
				refAssign->addAnnotation( std::make_shared<c_info::COpAnnotation>("=") ); // FIXME
			}

		} catch(const analysis::LoopNormalizationError& e) {

			if( VarDecl* condVarDecl = forStmt->getConditionVariable() ) {
				assert(forStmt->getCond() == NULL && "ForLoop condition cannot be a variable declaration and an expression");
				// the for loop has a variable declared in the condition part, e.g.
				// for(...; int a = f(); ...)
				//
				// to handle this kind of situation we have to move the declaration outside the loop body
				// inside a new context
				Expr* expr = condVarDecl->getInit();
				condVarDecl->setInit(NULL); // set the expression to null temporarily
				core::DeclarationStmtPtr&& declStmt = VisitVarDecl(condVarDecl);
				condVarDecl->setInit(expr);

				retStmt.push_back( declStmt );
			}

			// analysis of loop structure failed, we have to build a while statement
			retStmt.push_back( Visit( forStmt->getInit() ).getSingleStmt() );
			retStmt.push_back( builder.whileStmt(
					convFact.convertExpr( *forStmt->getCond() ),
					builder.compoundStmt(toVector<core::StatementPtr>(body.getSingleStmt(), convFact.convertExpr( *forStmt->getInc() ))))
			);
		}

		retStmt = tryAggregateStmts(builder, retStmt);

		END_LOG_STMT_CONVERSION( retStmt.getSingleStmt() );
		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								IF STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitIfStmt(IfStmt* ifStmt) {
		START_LOG_STMT_CONVERSION(ifStmt);
		const core::ASTBuilder& builder = convFact.builder;
		StmtWrapper retStmt;

		VLOG(2) << "{ IfStmt }";
		core::StatementPtr thenBody = tryAggregateStmts( builder, Visit( ifStmt->getThen() ) );
		assert(thenBody && "Couldn't convert 'then' body of the IfStmt");

		VLOG(2) << "IfStmt 'then' body: " << *thenBody;
		core::ExpressionPtr condExpr(NULL);
		if( VarDecl* condVarDecl = ifStmt->getConditionVariable() ) {
			assert(ifStmt->getCond() == NULL && "IfStmt condition cannot contains both a variable declaration and an expression");

			// we are in the situation where a variable is declared in the if condition, i.e.:
			// if(int a = ...) { }
			//
			// this will be converted into the following IR representation:
			// { int a = ...; if(a){ } }
			core::DeclarationStmtPtr&& declStmt = VisitVarDecl(condVarDecl);
			retStmt.push_back( declStmt );

			// the expression will be a reference to the declared variable
			condExpr = declStmt->getVarExpression();
		} else {
			Expr* cond = ifStmt->getCond();
			assert(cond && "If statement with no condition.");
			condExpr = convFact.convertExpr( *cond );
		}
		assert(condExpr && "Couldn't convert 'condition' expression of the IfStmt");
		VLOG(2) << "IfStmt 'condition' expression: " << *condExpr;

		core::StatementPtr elseBody(NULL);
		// check for else statement
		if(Stmt* elseStmt = ifStmt->getElse()) {
			elseBody = tryAggregateStmts( builder, Visit( elseStmt ) );
		} else {
			// create an empty compound statement in the case there is no else stmt
			elseBody = builder.compoundStmt();
		}
		assert(elseBody && "Couldn't convert 'else' body of the IfStmt");
		VLOG(2) << "IfStmt 'else' body: " << *elseBody;

		core::StatementPtr irNode = builder.ifStmt(condExpr, thenBody, elseBody);

		// handle eventual OpenMP pragmas attached to the Clang node
		frontend::omp::attachOmpAnnotation(irNode, ifStmt, convFact);

		// adding the ifstmt to the list of returned stmts
		retStmt.push_back( irNode );

		// try to aggregate statements into a CompoundStmt if more than 1 statement has been created
		// from this IfStmt
		retStmt = tryAggregateStmts(builder, retStmt);

		END_LOG_STMT_CONVERSION( retStmt.getSingleStmt() );
		// otherwise we introduce an outer CompoundStmt
		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							WHILE STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitWhileStmt(WhileStmt* whileStmt) {
		START_LOG_STMT_CONVERSION(whileStmt);
		const core::ASTBuilder& builder = convFact.builder;
		StmtWrapper retStmt;

		VLOG(2) << "{ WhileStmt }";
		core::StatementPtr body = tryAggregateStmts( builder, Visit( whileStmt->getBody() ) );
		assert(body && "Couldn't convert body of the WhileStmt");

		VLOG(2) << "WhileStmt body: " << body;
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
			core::DeclarationStmtPtr&& declStmt = VisitVarDecl(condVarDecl);
			condVarDecl->setInit(expr);

			retStmt.push_back( declStmt );

			// the expression will be an a = expr
			// condExpr = declStmt->getVarExpression();
			assert(false && "WhileStmt with a declaration of a condition variable not supported");
		} else {
			Expr* cond = whileStmt->getCond();
			assert(cond && "WhileStmt with no condition.");
			condExpr = convFact.convertExpr( *cond );
		}
		assert(condExpr && "Couldn't convert 'condition' expression of the WhileStmt");
		VLOG(2) << "WhileStmt 'condition' expression: " << condExpr;

		core::StatementPtr irNode = builder.whileStmt(condExpr, body);

		// handle eventual OpenMP pragmas attached to the Clang node
		frontend::omp::attachOmpAnnotation(irNode, whileStmt, convFact);

		// adding the WhileStmt to the list of returned stmts
		retStmt.push_back( irNode );
		retStmt = tryAggregateStmts(builder, retStmt);

		END_LOG_STMT_CONVERSION( retStmt.getSingleStmt() );
		// otherwise we introduce an outer CompoundStmt
		return retStmt;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							SWITCH STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitSwitchStmt(SwitchStmt* switchStmt) {
		START_LOG_STMT_CONVERSION(switchStmt);
		const core::ASTBuilder& builder = convFact.builder;
		StmtWrapper retStmt;

		VLOG(2) << "{ SwitchStmt }";
		core::ExpressionPtr condExpr(NULL);
		if( VarDecl* condVarDecl = switchStmt->getConditionVariable() ) {
			assert(switchStmt->getCond() == NULL && "SwitchStmt condition cannot contains both a variable declaration and an expression");

			core::DeclarationStmtPtr&& declStmt = VisitVarDecl(condVarDecl);
			retStmt.push_back( declStmt );

			// the expression will be a reference to the declared variable
			condExpr = declStmt->getVarExpression();
		} else {
			Expr* cond = switchStmt->getCond();
			assert(cond && "SwitchStmt with no condition.");
			condExpr = convFact.convertExpr( *cond );
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
				std::for_each(compStmt->body_begin(), compStmt->body_end(),
					[ &retStmt, this ] (Stmt* curr) {
						if(!isa<SwitchCase>(curr)) {
							StmtWrapper&& visitedStmt = this->Visit(curr);
							std::copy(visitedStmt.begin(), visitedStmt.end(), back_inserter(retStmt));
						}
					}
				);
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
					subStmt = convFact.convertExpr( *rhs );
				} else if( Stmt* sub = caseStmt->getSubStmt() ) {
					subStmt = tryAggregateStmts( builder, Visit(sub) );
				}
				cases.push_back( std::make_pair(convFact.convertExpr( *caseStmt->getLHS() ), subStmt) );
			} else {
				// default case
				DefaultStmt* defCase = dyn_cast<DefaultStmt>(switchCaseStmt);
				assert(defCase && "Case is not the 'default:'.");
				defStmt = tryAggregateStmts( builder, Visit(defCase->getSubStmt()) );
			}
			// next case
			switchCaseStmt = switchCaseStmt->getNextSwitchCase();
		}

		core::StatementPtr irNode = builder.switchStmt(condExpr, cases, defStmt);
		// handle eventual OpenMP pragmas attached to the Clang node
		frontend::omp::attachOmpAnnotation(irNode, switchStmt, convFact);

		// Appends the switchstmt to the current list of stmt
		retStmt.push_back( irNode );
		retStmt = tryAggregateStmts(builder, retStmt);

		END_LOG_STMT_CONVERSION( retStmt.getSingleStmt() );
		return retStmt;
	}

	// as a CaseStmt or DefaultStmt cannot be converted into any IR statements, we generate an error in the case
	// the visitor visits one of these nodes, the VisitSwitchStmt has to make sure the visitor is not called on his subnodes
	StmtWrapper VisitSwitchCase(SwitchCase* caseStmt) { assert(false && "Visitor is visiting a 'case' stmt (cannot compute)"); }

	StmtWrapper VisitBreakStmt(BreakStmt* breakStmt) { return StmtWrapper( convFact.builder.breakStmt() ); }
	StmtWrapper VisitContinueStmt(ContinueStmt* contStmt) { return StmtWrapper( convFact.builder.continueStmt() ); }

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							COMPOUND STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitCompoundStmt(CompoundStmt* compStmt) {
		START_LOG_STMT_CONVERSION(compStmt);
		vector<core::StatementPtr> stmtList;
		std::for_each( compStmt->body_begin(), compStmt->body_end(),
			[ &stmtList, this ] (Stmt* stmt) {
				// A compoundstmt can contain declaration statements.This means that a clang DeclStmt can be converted in multiple
				// StatementPtr because an initialization list such as: int a,b=1; is converted into the following sequence of statements:
				// int<a> a = 0; int<4> b = 1;
				StmtWrapper&& convertedStmt = this->Visit(stmt);
				std::copy(convertedStmt.begin(), convertedStmt.end(), std::back_inserter(stmtList));
			}
		);
		core::StatementPtr retStmt = convFact.builder.compoundStmt(stmtList);

		// handle eventual OpenMP pragmas attached to the Clang node
		frontend::omp::attachOmpAnnotation(retStmt, compStmt, convFact);

		END_LOG_STMT_CONVERSION(retStmt);
		return StmtWrapper( retStmt );
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							NULL STATEMENT
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	StmtWrapper VisitNullStmt(NullStmt* nullStmt) {
		core::StatementPtr retStmt = core::lang::STMT_NO_OP_PTR;

		// handle eventual OpenMP pragmas attached to the Clang node
		frontend::omp::attachOmpAnnotation(retStmt, nullStmt, convFact);

		return StmtWrapper( retStmt );
	}

	FORWARD_VISITOR_CALL(IntegerLiteral)
	FORWARD_VISITOR_CALL(FloatingLiteral)
	FORWARD_VISITOR_CALL(CharacterLiteral)
	FORWARD_VISITOR_CALL(StringLiteral)

	FORWARD_VISITOR_CALL(BinaryOperator)
	FORWARD_VISITOR_CALL(UnaryOperator)
	FORWARD_VISITOR_CALL(ConditionalOperator)

	FORWARD_VISITOR_CALL(CastExpr)
	FORWARD_VISITOR_CALL(ImplicitCastExpr)
	FORWARD_VISITOR_CALL(DeclRefExpr)
	FORWARD_VISITOR_CALL(ArraySubscriptExpr)
	FORWARD_VISITOR_CALL(CallExpr)
	FORWARD_VISITOR_CALL(ParenExpr)

	StmtWrapper VisitStmt(Stmt* stmt) {
		std::for_each( stmt->child_begin(), stmt->child_end(), [ this ] (Stmt* stmt) { this->Visit(stmt); });
		return StmtWrapper();
	}
};

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// 							Printing macros for statements
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#define MAKE_SIZE(n)	toVector(core::IntTypeParam::getConcreteIntParam(n))
#define EMPTY_TYPE_LIST	vector<core::TypePtr>()

#define START_LOG_TYPE_CONVERSION(type) \
	DVLOG(1) << "\n****************************************************************************************\n" \
			 << "Converting type [class: '" << (type)->getTypeClassName() << "']"; \
	if( VLOG_IS_ON(2) ) { \
		DVLOG(2) << "Dump of clang type: \n" \
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
		type->dump(); \
	}

#define END_LOG_TYPE_CONVERSION(type) \
	DVLOG(1) << "Converted 'type' into IR type: "; \
	DVLOG(1) << "\t" << *type;


//#############################################################################
//
//							CLANG TYPE CONVERTER
//
//############################################################################
class ConversionFactory::ClangTypeConverter: public TypeVisitor<ClangTypeConverter, core::TypePtr> {
	const ConversionFactory& convFact;

	utils::DependencyGraph<const Type*> typeGraph;

	typedef std::map<const Type*, core::TypeVariablePtr> TypeRecVarMap;
	TypeRecVarMap recVarMap;
	bool isRecSubType;

	typedef std::map<const Type*, core::TypePtr> RecTypeMap;
	RecTypeMap recTypeCache;

public:
	ClangTypeConverter(const ConversionFactory& fact): convFact( fact ), isRecSubType(false) { }

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								BUILTIN TYPES
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitBuiltinType(BuiltinType* buldInTy) {
		START_LOG_TYPE_CONVERSION( buldInTy );
		const core::ASTBuilder& builder = convFact.builder;

		switch(buldInTy->getKind()) {
		case BuiltinType::Void:
			return builder.getUnitType();
		case BuiltinType::Bool:
			return builder.getBoolType();

		// char types
		case BuiltinType::Char_U:
		case BuiltinType::UChar:
			return builder.genericType("uchar");
		case BuiltinType::Char16:
			return builder.genericType("char", EMPTY_TYPE_LIST, MAKE_SIZE(2));
		case BuiltinType::Char32:
			return builder.genericType("char", EMPTY_TYPE_LIST, MAKE_SIZE(4));
		case BuiltinType::Char_S:
		case BuiltinType::SChar:
			return builder.genericType("char");
		case BuiltinType::WChar:
			return builder.genericType("wchar");

		// integer types
		case BuiltinType::UShort:
			return builder.getUIntType( SHORT_LENGTH );
		case BuiltinType::Short:
			return builder.getIntType( SHORT_LENGTH );
		case BuiltinType::UInt:
			return builder.getUIntType( INT_LENGTH );
		case BuiltinType::Int:
			return builder.getIntType( INT_LENGTH );
		case BuiltinType::UInt128:
			return builder.getUIntType( 16 );
		case BuiltinType::Int128:
			return builder.getIntType( 16 );
		case BuiltinType::ULong:
			return builder.getUIntType( LONG_LENGTH );
		case BuiltinType::ULongLong:
			return builder.getUIntType( LONG_LONG_LENGTH );
		case BuiltinType::Long:
			return builder.getIntType( LONG_LENGTH );
		case BuiltinType::LongLong:
			return builder.getIntType( LONG_LONG_LENGTH );

		// real types
		case BuiltinType::Float:
			return builder.getRealType( FLOAT_LENGTH );
		case BuiltinType::Double:
			return builder.getRealType( DOUBLE_LENGTH );
		case BuiltinType::LongDouble:
			return builder.getRealType( LONG_DOUBLE_LENGTH );

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

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								COMPLEX TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitComplexType(ComplexType* bulinTy) {
		assert(false && "ComplexType not yet handled!");
	}

	// ------------------------   ARRAYS  -------------------------------------
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 					CONSTANT ARRAY TYPE
	//
	// This method handles the canonical version of C arrays with a specified
	// constant size. For example, the canonical type for 'int A[4 + 4*100]' is
	// a ConstantArrayType where the element type is 'int' and the size is 404
	//
	// The IR representation for such array will be: vector<ref<int<4>>,404>
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitConstantArrayType(ConstantArrayType* arrTy) {
		START_LOG_TYPE_CONVERSION( arrTy );
		if(arrTy->isSugared())
			// if the type is sugared, we Visit the desugared type
			return Visit( arrTy->desugar().getTypePtr() );

		size_t arrSize = *arrTy->getSize().getRawData();
		core::TypePtr&& elemTy = Visit( arrTy->getElementType().getTypePtr() );
		assert(elemTy && "Conversion of array element type failed.");

		core::TypePtr&& retTy = convFact.builder.vectorType( convFact.builder.refType(elemTy), core::IntTypeParam::getConcreteIntParam(arrSize) );
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						INCOMPLETE ARRAT TYPE
	// This method handles C arrays with an unspecified size. For example
	// 'int A[]' has an IncompleteArrayType where the element type is 'int'
	// and the size is unspecified.
	//
	// The representation for such array will be: ref<array<ref<int<4>>>>
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitIncompleteArrayType(IncompleteArrayType* arrTy) {
		START_LOG_TYPE_CONVERSION( arrTy );
		if(arrTy->isSugared())
			// if the type is sugared, we Visit the desugared type
			return Visit( arrTy->desugar().getTypePtr() );

		const core::ASTBuilder& builder = convFact.builder;
		core::TypePtr&& elemTy = Visit( arrTy->getElementType().getTypePtr() );
		assert(elemTy && "Conversion of array element type failed.");

		core::TypePtr&& retTy = builder.arrayType(builder.refType(elemTy));
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VARIABLE ARRAT TYPE
	// This class represents C arrays with a specified size which is not an
	// integer-constant-expression. For example, 'int s[x+foo()]'. Since the
	// size expression is an arbitrary expression, we store it as such.
	// Note: VariableArrayType's aren't uniqued (since the expressions aren't)
	// and should not be: two lexically equivalent variable array types could
	// mean different things, for example, these variables do not have the same
	// type dynamically:
	//				void foo(int x) { int Y[x]; ++x; int Z[x]; }
	//
	// he representation for such array will be: array<ref<int<4>>>( expr() )
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitVariableArrayType(VariableArrayType* arrTy) {
		START_LOG_TYPE_CONVERSION( arrTy );
		if(arrTy->isSugared())
			// if the type is sugared, we Visit the desugared type
			return Visit( arrTy->desugar().getTypePtr() );

		const core::ASTBuilder& builder = convFact.builder;
		core::TypePtr&& elemTy = Visit( arrTy->getElementType().getTypePtr() );
		assert(elemTy && "Conversion of array element type failed.");

		core::TypePtr retTy = builder.arrayType( builder.refType(elemTy) );
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 						DEPENDENT SIZED ARRAY TYPE
	// This type represents an array type in C++ whose size is a value-dependent
	// expression. For example:
	//
	//  template<typename T, int Size>
	//  class array {
	//     T data[Size];
	//  };
	//
	// For these types, we won't actually know what the array bound is until
	// template instantiation occurs, at which point this will become either
	// a ConstantArrayType or a VariableArrayType.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitDependentSizedArrayType(DependentSizedArrayType* arrTy) {
		assert(false && "DependentSizedArrayType not yet handled!");
	}

	// --------------------  FUNCTIONS  ---------------------------------------
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//				FUNCTION PROTO TYPE
	// Represents a prototype with argument type info, e.g. 'int foo(int)' or
	// 'int foo(void)'. 'void' is represented as having no arguments, not as
	// having a single void argument. Such a type can have an exception
	// specification, but this specification is not part of the canonical type.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitFunctionProtoType(FunctionProtoType* funcTy) {
		START_LOG_TYPE_CONVERSION(funcTy);

		const core::ASTBuilder& builder = convFact.builder;
		core::TypePtr&& retTy = Visit( funcTy->getResultType().getTypePtr() );
		assert(retTy && "Function has no return type!");

		core::TupleType::ElementTypeList argTypes;
		std::for_each(funcTy->arg_type_begin(), funcTy->arg_type_end(),
			[ &argTypes, this ] (const QualType& currArgType) {
				// we add a ref type for function parameters
				argTypes.push_back( this->convFact.builder.refType( this->Visit( currArgType.getTypePtr() ) ) );
			}
		);

		if( argTypes.size() == 1 && *argTypes.front() == core::lang::TYPE_UNIT_VAL) {
			// we have only 1 argument, and it is a unit type (void), remove it from the list
			argTypes.clear();
		}

		if( funcTy->isVariadic() )
			argTypes.push_back( core::lang::TYPE_VAR_LIST );

		retTy = builder.functionType( builder.tupleType(argTypes), retTy);
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					FUNCTION NO PROTO TYPE
	// Represents a K&R-style 'int foo()' function, which has no information
	// available about its arguments.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitFunctionNoProtoType(FunctionNoProtoType* funcTy) {
		START_LOG_TYPE_CONVERSION( funcTy );
		core::TypePtr&& retTy = Visit( funcTy->getResultType().getTypePtr() );
		assert(retTy && "Function has no return type!");

		retTy = convFact.builder.functionType( convFact.builder.tupleType(), retTy);
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	// TBD
//	TypeWrapper VisitVectorType(VectorType* vecTy) {	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 							EXTENDEND VECTOR TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitExtVectorType(ExtVectorType* vecTy) {
       // get vector datatype
        const QualType qt = vecTy->getElementType();
        const BuiltinType* buildInTy = dyn_cast<const BuiltinType>( qt->getUnqualifiedDesugaredType() );
        core::TypePtr&& subType = Visit(const_cast<BuiltinType*>(buildInTy));

        // get the number of elements
        size_t num = vecTy->getNumElements();
        core::IntTypeParam numElem = core::IntTypeParam::getConcreteIntParam(num);

        //note: members of OpenCL vectors are always modifiable
        return convFact.builder.vectorType( convFact.builder.refType(subType), numElem);
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 								TYPEDEF TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitTypedefType(TypedefType* typedefType) {
		START_LOG_TYPE_CONVERSION( typedefType );

        core::TypePtr&& subType = Visit( typedefType->getDecl()->getUnderlyingType().getTypePtr() );
        // Adding the name of the typedef as annotation
        subType.addAnnotation(std::make_shared<insieme::c_info::CNameAnnotation>(typedefType->getDecl()->getNameAsString()));

        END_LOG_TYPE_CONVERSION( subType );
        return  subType;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 								TYPE OF TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitTypeOfType(TypeOfType* typeOfType) {
		START_LOG_TYPE_CONVERSION(typeOfType);
		core::TypePtr retTy = convFact.builder.getUnitType();
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// 							TYPE OF EXPRESSION TYPE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitTypeOfExprType(TypeOfExprType* typeOfType) {
		START_LOG_TYPE_CONVERSION( typeOfType );
		core::TypePtr&& retTy = Visit( GET_TYPE_PTR(typeOfType->getUnderlyingExpr()) );
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//					TAG TYPE: STRUCT | UNION | CLASS | ENUM
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitTagType(TagType* tagType) {
		if(!recVarMap.empty()) {
			// check if this type has a typevar already associated, in such case return it
			TypeRecVarMap::const_iterator fit = recVarMap.find(tagType);
			if( fit != recVarMap.end() ) {
				// we are resolving a parent recursive type, so we shouldn't
				return fit->second;
			}
		}

		// check if the type is in the cache of already solved recursive types
		// this is done only if we are not resolving a recursive sub type
		if(!isRecSubType) {
			RecTypeMap::const_iterator rit = recTypeCache.find(tagType);
			if(rit != recTypeCache.end())
				return rit->second;
		}

		START_LOG_TYPE_CONVERSION(tagType);

		// will store the converted type
		core::TypePtr retTy(NULL);
		DVLOG(2) << "Converting TagType: " << tagType->getDecl()->getName().str();

		TagDecl* tagDecl = tagType->getDecl()->getCanonicalDecl();
		// iterate through all the re-declarations to see if one of them provides a definition
		TagDecl::redecl_iterator i,e = tagDecl->redecls_end();
		for(i = tagDecl->redecls_begin(); i != e && !i->isDefinition(); ++i) ;
		if(i != e) {
			tagDecl = tagDecl->getDefinition();
			// we found a definition for this declaration, use it
			assert(tagDecl->isDefinition() && "TagType is not a definition");

			if(tagDecl->getTagKind() == clang::TTK_Enum) {
				assert(false && "Enum types not supported yet");
			} else {
				// handle struct/union/class
				RecordDecl* recDecl = dyn_cast<RecordDecl>(tagDecl);
				assert(recDecl && "TagType decl is not of a RecordDecl type!");

				if(!isRecSubType) {
					// add this type to the type graph (if not present)
					typeGraph.addNode(tagDecl->getTypeForDecl());
				}

				// retrieve the strongly connected componenets for this type
				std::set<const Type*>&& components = typeGraph.getStronglyConnectedComponents(tagDecl->getTypeForDecl());

				if( !components.empty() ) {
					if(VLOG_IS_ON(2)) {
						// we are dealing with a recursive type
						VLOG(2) << "Analyzing RecordDecl: " << recDecl->getNameAsString() << std::endl
								<< "Number of components in the cycle: " << components.size();
						std::for_each(components.begin(), components.end(),
							[] (std::set<const Type*>::value_type c) {
								assert(isa<const TagType>(c));
								VLOG(2) << "\t" << dyn_cast<const TagType>(c)->getDecl()->getNameAsString();
							}
						);
						typeGraph.print(std::cout);
					}

					// we create a TypeVar for each type in the mutual dependence
					recVarMap.insert( std::make_pair(tagType, convFact.builder.typeVariable(recDecl->getName())) );

					// when a subtype is resolved we aspect to already have these variables in the map
					if(!isRecSubType) {
						std::for_each(components.begin(), components.end(),
							[ this ] (std::set<const Type*>::value_type ty) {
								const TagType* tagTy = dyn_cast<const TagType>(ty);
								assert(tagTy && "Type is not of TagType type");

								this->recVarMap.insert( std::make_pair(ty, convFact.builder.typeVariable(tagTy->getDecl()->getName())) );
							}
						);
					}
				}

				// Visit the type of the fields recursively
				// Note: if a field is referring one of the type in the cyclic dependency, a reference
				//       to the TypeVar will be returned.
				core::NamedCompositeType::Entries structElements;
				for(RecordDecl::field_iterator it=recDecl->field_begin(), end=recDecl->field_end(); it != end; ++it) {
					RecordDecl::field_iterator::value_type curr = *it;
					Type* fieldType = curr->getType().getTypePtr();
					structElements.push_back(
							core::NamedCompositeType::Entry(core::Identifier(curr->getNameAsString()), Visit( fieldType ))
					);
				}

				// build a struct or union IR type
				retTy = handleTagType(tagDecl, structElements);

				if( !components.empty() ) {
					// if we are visiting a nested recursive type it means someone else will take care
					// of building the rectype node, we just return an intermediate type
					if(isRecSubType)
						return retTy;

					// we have to create a recursive type
					TypeRecVarMap::const_iterator tit = recVarMap.find(tagType);
					assert(tit != recVarMap.end() && "Recursive type has not TypeVar associated to himself");
					core::TypeVariablePtr recTypeVar = tit->second;

					core::RecTypeDefinition::RecTypeDefs definitions;
					definitions.insert( std::make_pair(recTypeVar, handleTagType(tagDecl, structElements) ) );

					// We start building the recursive type. In order to avoid loop the visitor
					// we have to change its behaviour and let him returns temporarely types
					// when a sub recursive type is visited.
					isRecSubType = true;

					std::for_each(components.begin(), components.end(),
						[ this, &definitions ] (std::set<const Type*>::value_type ty) {
							const TagType* tagTy = dyn_cast<const TagType>(ty);
							assert(tagTy && "Type is not of TagType type");

							TypeRecVarMap::const_iterator tit = recVarMap.find(ty);
							assert(tit != recVarMap.end() && "Recursive type has no TypeVar associated");
							core::TypeVariablePtr var = tit->second;

							// we remove the variable from the list in order to fool the solver,
							// in this way it will create a descriptor for this type (and he will not return the TypeVar
							// associated with this recursive type). This behaviour is enabled only when the isRecSubType
							// flag is true
							recVarMap.erase(ty);

							definitions.insert( std::make_pair(var, this->Visit(const_cast<Type*>(ty))) );

							// reinsert the TypeVar in the map in order to solve the other recursive types
							recVarMap.insert( std::make_pair(tagTy, var) );
						}
					);
					// we reset the behavior of the solver
					isRecSubType = false;
					// the map is also erased so visiting a second type of the mutual cycle will yield a correct result
					recVarMap.clear();

					core::RecTypeDefinitionPtr definition = convFact.builder.recTypeDefinition(definitions);

					retTy = convFact.builder.recType(recTypeVar, definition);

					// Once we solved this recursive type, we add to a cache of recursive types
					// so next time we encounter it, we don't need to compute the graph
					recTypeCache.insert(std::make_pair(tagType, retTy));
				}

				// Adding the name of the C struct as annotation
				retTy.addAnnotation( std::make_shared<insieme::c_info::CNameAnnotation>(recDecl->getName()) );
			}
		} else {
			// We didn't find any definition for this type, so we use a name and define it as a generic type
			retTy = convFact.builder.genericType( tagDecl->getNameAsString() );
		}
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							ELABORATED TYPE (TODO)
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitElaboratedType(ElaboratedType* elabType) {
		assert(false && "ElaboratedType not yet handled!");
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							POINTER TYPE (FIXME)
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitPointerType(PointerType* pointerTy) {
		START_LOG_TYPE_CONVERSION(pointerTy);
		core::TypePtr&& retTy = convFact.builder.refType( Visit(pointerTy->getPointeeType().getTypePtr()) );
		END_LOG_TYPE_CONVERSION( retTy );
		return retTy;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						REFERENCE TYPE (FIXME)
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::TypePtr VisitReferenceType(ReferenceType* refTy) {
		return convFact.builder.refType( Visit( refTy->getPointeeType().getTypePtr()) );
	}

private:

	core::TypePtr handleTagType(const TagDecl* tagDecl, const core::NamedCompositeType::Entries& structElements) {
		if( tagDecl->getTagKind() == clang::TTK_Struct || tagDecl->getTagKind() ==  clang::TTK_Class ) {
			return convFact.builder.structType( structElements );
		} else if( tagDecl->getTagKind() == clang::TTK_Union ) {
			return convFact.builder.unionType( structElements );
		}
		assert(false && "TagType not supported");
	}
};

// ------------------------------------ ConversionFactory ---------------------------

ConversionFactory::ConversionFactory(core::SharedNodeManager mgr, const ClangCompiler& clang, const PragmaList& pragmaList):
	mgr(mgr),  builder(mgr), clangComp(clang), pragmaMap(pragmaList),
	typeConv( new ClangTypeConverter(*this) ),
	exprConv( new ClangExprConverter(*this) ),
	stmtConv( new ClangStmtConverter(*this) ) { }

core::TypePtr ConversionFactory::convertType(const clang::Type& type) const {
	return typeConv->Visit( const_cast<Type*>(&type) );
}

core::StatementPtr ConversionFactory::convertStmt(const clang::Stmt& stmt) const {
	return stmtConv->Visit( const_cast<Stmt*>(&stmt) ).getSingleStmt();
}

core::ExpressionPtr ConversionFactory::convertExpr(const clang::Expr& expr) const {
	return exprConv->Visit( const_cast<Expr*>(&expr) );
}

/* Function to convert Clang attributes of declarations to IR annotations (local version)
 * currently used for:
 * 	* OpenCL address spaces
 */
core::AnnotationPtr ConversionFactory::convertClangAttributes(const clang::VarDecl* varDecl) {
    if(!varDecl->hasAttrs())
    	return core::AnnotationPtr();

	const AttrVec attrVec = varDecl->getAttrs();
	std::ostringstream ss;
	ocl::BaseAnnotation::AnnotationList declAnnotation;
	try {
	for(AttrVec::const_iterator I = attrVec.begin(), E = attrVec.end(); I != E; ++I) {
		if(AnnotateAttr* attr = dyn_cast<AnnotateAttr>(*I)) {
			std::string sr = attr->getAnnotation().str();

			//check if the declaration has attribute __private
			if(sr == "__private") {
				DVLOG(2) << "           OpenCL address space __private";
				declAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>(
						ocl::AddressSpaceAnnotation::addressSpace::PRIVATE));
				continue;
			}

			//check if the declaration has attribute __local
			if(sr == "__local") {
				DVLOG(2) << "           OpenCL address space __local";
				declAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>(
						ocl::AddressSpaceAnnotation::addressSpace::LOCAL));
				continue;
			}

			//check if the declaration has attribute __global
			if(sr == "__global") {
				ss << "Address space __global not allowed for local variable";
				throw &ss;
			}

			//check if the declaration has attribute __constant
			if(sr == "__constant") {
				ss << "Address space __constant not allowed for local variable";
				throw &ss;
			}

			ss << "Unexpected annotation " << sr;

			throw &ss;
		}
		else
			ss << "Unexpected attribute";
			throw &ss;
	}}
	catch(std::ostringstream *errMsg) {
        //show errors if unexpected patterns were found
        printErrorMsg(*errMsg, clangComp, varDecl);
	}
	return std::make_shared<ocl::BaseAnnotation>(declAnnotation);
}

/* Function to convert Clang attributes of declarations to IR annotations (arguments version)
 * currently used for:
 * OpenCL address spaces
 */
core::AnnotationPtr ConversionFactory::convertClangAttributes(const clang::ParmVarDecl* varDecl) {
    if(!varDecl->hasAttrs())
    	return core::AnnotationPtr();

	const AttrVec attrVec = varDecl->getAttrs();
	std::ostringstream ss;
	ocl::BaseAnnotation::AnnotationList paramAnnotation;

	try {
	for(AttrVec::const_iterator I = attrVec.begin(), E = attrVec.end(); I != E; ++I) {
		if(AnnotateAttr* attr = dyn_cast<AnnotateAttr>(*I)) {
			std::string sr = attr->getAnnotation().str();

			//check if the declaration has attribute __private
			if(sr == "__private") {
				DVLOG(2) << "           OpenCL address space __private";
				paramAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>(
						ocl::AddressSpaceAnnotation::addressSpace::PRIVATE));
				continue;
			}

			//check if the declaration has attribute __local
			if(sr == "__local") {
				DVLOG(2) << "           OpenCL address space __local";
				paramAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>(
						ocl::AddressSpaceAnnotation::addressSpace::LOCAL));
				continue;
			}

			//check if the declaration has attribute __global
			if(sr == "__global") {
				DVLOG(2) << "           OpenCL address space __global";
				paramAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>(
						ocl::AddressSpaceAnnotation::addressSpace::GLOBAL));
				continue;
			}

			//check if the declaration has attribute __constant
			if(sr == "__constant") {
				DVLOG(2) << "           OpenCL address space __constant";
				paramAnnotation.push_back(std::make_shared<ocl::AddressSpaceAnnotation>(
						ocl::AddressSpaceAnnotation::addressSpace::CONSTANT));
				continue;
			}
			ss << "Unexpected annotation " << sr;

			throw &ss;
		}
		else
			ss << "Unexpected attribute";
			throw &ss;
	}}
	catch(std::ostringstream *errMsg) {
        //show errors if unexpected patterns were found
        printErrorMsg(*errMsg, clangComp, varDecl);
	}
	return std::make_shared<ocl::BaseAnnotation>(paramAnnotation);
}

ConversionFactory::~ConversionFactory() {
	delete typeConv;
	delete stmtConv;
	delete exprConv;
}

// ------------------------------------ ClangTypeConverter ---------------------------

void IRConverter::handleTopLevelDecl(clang::DeclContext* declCtx) {

	for(DeclContext::decl_iterator it = declCtx->decls_begin(), end = declCtx->decls_end(); it != end; ++it) {
		Decl* decl = *it;
		if(FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(decl)) {

			// finds a definition of this function if any
			const FunctionDecl* definition = NULL;
			// if this function is just a declaration, and it has no definition, we just skip it
			if(!funcDecl->hasBody(definition))
				continue;

			core::LambdaExprPtr lambdaExpr = handleFunctionDecl(definition);
			if(definition->isMain())
				mProgram = core::Program::addEntryPoint(*mFact.getNodeManager(), mProgram, lambdaExpr);
		}
//		else if(VarDecl* varDecl = dyn_cast<VarDecl>(decl)) {
//			fact.convertType( *varDecl->getType().getTypePtr() );
//		}
	}

}

core::LambdaExprPtr IRConverter::handleFunctionDecl(const clang::FunctionDecl* funcDecl) {
	DVLOG(1) << "**************************************************************************";
	DVLOG(1) << "Encountered function declaration '" << funcDecl->getNameAsString() << "': "
			 << frontend::utils::location( funcDecl->getLocStart(), mClangComp.getSourceManager() );
	DVLOG(1) << "\t* Converting body";

	core::TypePtr funcType = mFact.convertType( *GET_TYPE_PTR(funcDecl) );
	// paramlist
	core::LambdaExpr::ParamList funcParamList;
	std::for_each(funcDecl->param_begin(), funcDecl->param_end(),
		[&funcParamList, &mFact] (ParmVarDecl* currParam) {
			funcParamList.push_back(
				mFact.getASTBuilder().paramExpr( mFact.convertType( *GET_TYPE_PTR(currParam) ), currParam->getNameAsString()) );

			//port clang attributes to IR annotations
			core::AnnotationPtr attr = mFact.convertClangAttributes(currParam);
			if(attr)
				 funcParamList.back()->addAnnotation(attr);
		}
	);

	//check Attributes of the function definition
	ocl::BaseAnnotation::AnnotationList kernelAnnotation;
	if(funcDecl->hasAttrs()) {
		const clang::AttrVec attrVec = funcDecl->getAttrs();

		for(AttrVec::const_iterator I = attrVec.begin(), E = attrVec.end(); I != E; ++I) {
			if(AnnotateAttr* attr = dyn_cast<AnnotateAttr>(*I)) {
				//get annotate string
				llvm::StringRef sr = attr->getAnnotation();

				//check if it is an OpenCL kernel function
				if(sr == "__kernel") {
					DVLOG(1) << "is OpenCL kernel function";
					kernelAnnotation.push_back( std::make_shared<ocl::KernelFctAnnotation>() );
				}
			}
			if(ReqdWorkGroupSizeAttr* attr = dyn_cast<ReqdWorkGroupSizeAttr>(*I)) {
				kernelAnnotation.push_back(std::make_shared<ocl::WorkGroupSizeAnnotation>(
						attr->getXDim(), attr->getYDim(), attr->getZDim())
				);
			}
		}
	}

	core::StatementPtr funcBody(NULL);
	assert(funcDecl->getBody() && "Function Definition has no body");

	funcBody = mFact.convertStmt( *funcDecl->getBody() );
	core::LambdaExprPtr&& lambdaExpr = mFact.getASTBuilder().lambdaExpr(funcType, funcParamList, funcBody);
    // if OpenCL related annotations have been found, create OclBaseAnnotation and
    // add it to the funciton's attribute
    if(kernelAnnotation.size() > 0)
        lambdaExpr.addAnnotation( std::make_shared<ocl::BaseAnnotation>(kernelAnnotation) );
	// annotate name of function
	lambdaExpr.addAnnotation(std::make_shared<insieme::c_info::CNameAnnotation>(funcDecl->getName()));
	return lambdaExpr;
}

core::LambdaExprPtr IRConverter::handleBody(const clang::Stmt* body) {
	core::StatementPtr&& bodyStmt = mFact.convertStmt( *body );
	core::CallExprPtr&& callExpr = createCallExpr(mFact.getASTBuilder(), std::vector<core::StatementPtr>( {bodyStmt} ), core::lang::TYPE_UNIT);

	return core::dynamic_pointer_cast<const core::LambdaExpr>(callExpr->getFunctionExpr());
}


} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
