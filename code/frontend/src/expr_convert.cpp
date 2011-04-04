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

#include "insieme/frontend/convert.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/dep_graph.h"
#include "insieme/frontend/utils/clang_utils.h"
#include "insieme/frontend/analysis/expr_analysis.h"
#include "insieme/frontend/omp/omp_pragma.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"

#include "insieme/c_info/naming.h"

#include "clang/AST/StmtVisitor.h"

#include "clang/Index/Entity.h"
#include "clang/Index/Indexer.h"

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

namespace std {
std::ostream& operator<<(std::ostream& out, const clang::FunctionDecl* funcDecl) {
	return out << funcDecl->getNameAsString() << "(" << funcDecl->param_size() << ")";
}
} // end std namespace

namespace {
// Returns a string of the text within the source range of the input stream
std::string GetStringFromStream(const SourceManager& srcMgr, const SourceLocation& start) {
	/*
	 *  we use the getDecomposedSpellingLoc() method because in case we read macros values we have to read the expanded
	 *  value
	 */
	std::pair<FileID, unsigned>&& startLocInfo = srcMgr.getDecomposedSpellingLoc(start);
	llvm::StringRef&& startBuffer = srcMgr.getBufferData(startLocInfo.first);
	const char *strDataStart = startBuffer.begin() + startLocInfo.second;

	return string(strDataStart,
			clang::Lexer::MeasureTokenLength(srcMgr.getSpellingLoc(start), srcMgr, clang::LangOptions())
		);
}

/*
 * In case the the last argument of the function is a var_arg, we try pack the exceeding arguments with the pack
 * operation provided by the IR.
 */
vector<core::ExpressionPtr>
tryPack(const core::ASTBuilder& builder, core::FunctionTypePtr funcTy, const ExpressionList& args) {

	// check if the function type ends with a VAR_LIST type
	const core::TypeList& argsTy = funcTy->getArgumentTypes();
	// assert(argsTy && "Function argument is of not type TupleType");

	// if the tuple type is empty it means we cannot pack any of the arguments
	if( argsTy.empty() ) {
		return args;
	}

	const core::lang::BasicGenerator& gen = builder.getBasicGenerator();
	if ( gen.isVarList(argsTy.back()) ) {
		ExpressionList ret;
		assert(args.size() >= argsTy.size()-1 && "Function called with fewer arguments than necessary");
		// last type is a var_list, we have to do the packing of arguments

		// we copy the first N-1 arguments, the remaining will be unpacked
		std::copy(args.begin(), args.begin()+argsTy.size()-1, std::back_inserter(ret));

		ExpressionList toPack;
		if ( args.size() > argsTy.size()-1 ) {
			std::copy(args.begin()+argsTy.size()-1, args.end(), std::back_inserter(toPack));
		}

		// arguments has to be packed into a tuple expression, and then inserted into a pack expression
		ret.push_back(
			builder.callExpr(gen.getVarList(), gen.getVarlistPack(), builder.tupleExpr(toPack))
		);
		return ret;
	}
	return args;
}

core::CallExprPtr getSizeOfType(const core::ASTBuilder& builder, const core::TypePtr& type) {
	core::LiteralPtr size;

	const core::lang::BasicGenerator& gen = builder.getBasicGenerator();
	if ( core::VectorTypePtr&& vecTy = core::dynamic_pointer_cast<const core::VectorType>(type) ) {
		return builder.callExpr(
			gen.getUnsignedIntMul(),
			builder.literal( gen.getUInt8(), toString(*(vecTy->getSize())) ),
			getSizeOfType( builder, vecTy->getElementType() )
		);
	}
	// in case of ref<'a>, recurr on 'a
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type) ) {
		return getSizeOfType( builder, refTy->getElementType() );
	}

	return builder.callExpr( gen.getSizeof(), gen.getTypeLiteral(type) );
}

core::ExpressionPtr
handleMemAlloc(const core::ASTBuilder& builder, const core::TypePtr& type, const core::ExpressionPtr& subExpr) {

	if( core::CallExprPtr&& callExpr = core::dynamic_pointer_cast<const core::CallExpr>(subExpr) ) {

		if ( core::LiteralPtr&& lit = core::dynamic_pointer_cast<const core::Literal>(callExpr->getFunctionExpr()) ) {

			if ( lit->getValue() == "malloc" || lit->getValue() == "calloc" ) {
				assert(callExpr->getArguments().size() == 1 && "malloc() takes only 1 argument");

				const core::lang::BasicGenerator& gen = builder.getBasicGenerator();
				// The type of the cast should be ref<array<'a>>, and the sizeof('a) need to be derived
				assert(type->getNodeType() == core::NT_ArrayType);
				const core::TypePtr& elemType = core::static_pointer_cast<const core::ArrayType>(type)->getElementType();

				/*
				 * The number of elements to be allocated of type 'targetType' is:
				 * 		-> 	expr / sizeof(targetType)
				 */
				core::CallExprPtr&& size = builder.callExpr(
					gen.getUnsignedIntDiv(), callExpr->getArguments().front(), getSizeOfType(builder, elemType)
				);

				assert(elemType->getNodeType() != core::NT_RefType);
				//elemType = core::static_pointer_cast<const core::RefType>(elemType)->getElementType();

				return builder.refNew(builder.callExpr(type, gen.getArrayCreate1D(),
						builder.callExpr(elemType, gen.getUndefined(), gen.getTypeLiteral(elemType)), size)
					);
			}
		}
	}
	return core::ExpressionPtr();

}

} // end anonymous namespace

namespace insieme {
namespace frontend {

namespace utils {

struct CallExprVisitor: public clang::StmtVisitor<CallExprVisitor> {

	clang::idx::Indexer& indexer;
	typedef std::set<const clang::FunctionDecl*> CallGraph;
	CallGraph callGraph;

	CallExprVisitor (clang::idx::Indexer& indexer): indexer(indexer) { }

	CallGraph getCallGraph (const clang::FunctionDecl* func) {
		assert(func->hasBody() && "Function in the dependency graph has no body");

		Visit(func->getBody());
		return callGraph;
	}

	void VisitCallExpr (clang::CallExpr* callExpr) {

		if ( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(callExpr->getDirectCallee()) ) {
			const clang::FunctionDecl* def = NULL;
			/*
			 * this will find function definitions if they are declared in  the same translation unit
			 * (also defined as static)
			 */
			if ( !funcDecl->hasBody(def) ) {
				/*
				 * if the function is not defined in this translation unit, maybe it is defined in another we already
				 * loaded use the clang indexer to lookup the definition for this function declarations
				 */
				clang::idx::Entity&& funcEntity = clang::idx::Entity::get( funcDecl, indexer.getProgram() );
				conversion::ConversionFactory::TranslationUnitPair&& ret = indexer.getDefinitionFor(funcEntity);
				if ( ret.first ) {
					def = ret.first;
				}
			}

			if ( def ) { callGraph.insert(def); }
		}
		VisitStmt(callExpr);
	}

	void VisitStmt (clang::Stmt* stmt) {
		std::for_each(stmt->child_begin(), stmt->child_end(),
			[ this ](clang::Stmt* curr) { if(curr) this->Visit(curr); });
	}
};

/**
 * In order for DepGraph to build the dependency graph for functions the clang indexer is needed,
 * FunctionDependencyGraph adds the indexer to member functions of DependencyGraph
 */
class FunctionDepenencyGraph : public DependencyGraph<const clang::FunctionDecl*> {
	clang::idx::Indexer& idx;
public:
	FunctionDepenencyGraph(clang::idx::Indexer& idx) : DependencyGraph<const clang::FunctionDecl*>(), idx(idx) { }
	clang::idx::Indexer& getIndexer() const { return idx; }
};

template <>
void DependencyGraph<const clang::FunctionDecl*>::Handle(const clang::FunctionDecl* func,
							const DependencyGraph<const clang::FunctionDecl*>::VertexTy& v) {
	// This is potentially dangerous
	FunctionDepenencyGraph& funcDepGraph = static_cast<FunctionDepenencyGraph&>(*this);

	CallExprVisitor callExprVis(funcDepGraph.getIndexer());
	CallExprVisitor::CallGraph&& graph = callExprVis.getCallGraph(func);

	std::for_each(graph.begin(), graph.end(),
			[ this, v ](const clang::FunctionDecl* currFunc) { assert(currFunc); this->addNode(currFunc, &v); }
	);
}

} // end namespace utils

namespace conversion {

#define START_LOG_EXPR_CONVERSION(expr) \
	assert(convFact.currTU && "Translation unit not correctly set"); \
	VLOG(1) << "\n****************************************************************************************\n" \
			 << "Converting expression [class: '" << expr->getStmtClassName() << "']\n" \
			 << "-> at location: (" <<	\
				utils::location(expr->getLocStart(), convFact.currTU->getCompiler().getSourceManager()) << "): "; \
	if( VLOG_IS_ON(2) ) { \
		VLOG(2) << "Dump of clang expression: \n" \
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
		expr->dump(); \
	}

#define END_LOG_EXPR_CONVERSION(expr) \
	VLOG(1) << "Converted into IR expression: "; \
	VLOG(1) << "\t" << *expr << " type:( " << *expr->getType() << " )";


//---------------------------------------------------------------------------------------------------------------------
//										CLANG EXPRESSION CONVERTER
//---------------------------------------------------------------------------------------------------------------------
class ConversionFactory::ClangExprConverter: public StmtVisitor<ClangExprConverter, core::ExpressionPtr> {
	ConversionFactory& convFact;
	ConversionContext& ctx;

	core::ExpressionPtr wrapVariable(clang::Expr* expr) {
		const DeclRefExpr* ref = utils::skipSugar<const DeclRefExpr>(expr);
		if ( ref && isa<const ParmVarDecl>(ref->getDecl()) ) {
			const core::VariablePtr& parmVar =
				core::static_pointer_cast<const core::Variable>( convFact.convertExpr(ref) );

			auto fit = ctx.wrapRefMap.find(parmVar);
			if ( fit == ctx.wrapRefMap.end() ) {
				fit = ctx.wrapRefMap.insert(
					std::make_pair( parmVar, convFact.builder.variable(convFact.builder.refType(parmVar->getType())) )
				).first;
			}
			return fit->second;
		}
		return convFact.convertExpr(expr);
	}

public:

	// CallGraph for functions, used to resolved eventual recursive functions
	utils::FunctionDepenencyGraph funcDepGraph;

	ClangExprConverter(ConversionFactory& convFact, Program& program): convFact(convFact), ctx(convFact.ctx),
			funcDepGraph(program.getClangIndexer()) { }

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								INTEGER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitIntegerLiteral(clang::IntegerLiteral* intLit) {
		START_LOG_EXPR_CONVERSION(intLit);
		core::ExpressionPtr&& retExpr =
			convFact.builder.literal(
				// retrieve the string representation from the source code
				GetStringFromStream( convFact.currTU->getCompiler().getSourceManager(), intLit->getExprLoc() ),
				convFact.convertType( GET_TYPE_PTR(intLit) )
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
				GetStringFromStream( convFact.currTU->getCompiler().getSourceManager(), floatLit->getExprLoc()),
				convFact.convertType( GET_TYPE_PTR(floatLit) )
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
				GetStringFromStream(convFact.currTU->getCompiler().getSourceManager(), charLit->getExprLoc()),
					(charLit->isWide() ? convFact.mgr.basic.getWChar() : convFact.mgr.basic.getChar())
			);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								STRING LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitStringLiteral(clang::StringLiteral* stringLit) {
		START_LOG_EXPR_CONVERSION(stringLit);
		std::string&& strValue = GetStringFromStream(
				convFact.currTU->getCompiler().getSourceManager(), stringLit->getExprLoc()
			);
		core::ExpressionPtr&& retExpr =	convFact.builder.literal( strValue,	convFact.mgr.basic.getString() );
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
				GetStringFromStream(convFact.currTU->getCompiler().getSourceManager(),
						boolLit->getExprLoc()), convFact.mgr.basic.getBool()
			);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PARENTESIS EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitParenExpr(clang::ParenExpr* parExpr) {
		core::ExpressionPtr&& retExpr = Visit( parExpr->getSubExpr() );
		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, parExpr, convFact);
		return annotatedNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						   IMPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitImplicitCastExpr(clang::ImplicitCastExpr* implCastExpr) {
		START_LOG_EXPR_CONVERSION(implCastExpr);
		core::TypePtr&& type = convFact.convertType( GET_TYPE_PTR(implCastExpr) );
		core::ExpressionPtr&& subExpr = Visit(implCastExpr->getSubExpr());
		core::ExpressionPtr&& nonRefExpr = convFact.tryDeref(subExpr);

		// if the cast is to a aa pointer type and the subexpr is a 0 it should be replaced with a null literal
		if ( ( type->getNodeType() == core::NT_ArrayType ) &&
				*subExpr == *convFact.builder.literal(subExpr->getType(),"0") ) {
			return convFact.builder.castExpr( type, convFact.builder.getNodeManager().basic.getNull() );
		}

		// Mallocs/Allocs are replaced with ref.new expression
		if (core::ExpressionPtr&& retExpr = handleMemAlloc(convFact.getASTBuilder(), type, subExpr) ) {
			return retExpr;
		}

		// If the subexpression is a string, remove the implicit casts
		if ( convFact.mgr.basic.isString(subExpr->getType()) ) {
			return subExpr;
		}

		// if the subexpression is an array or a vector, remove all the C implicit casts
		if ( nonRefExpr->getType()->getNodeType() == core::NT_ArrayType ||
				nonRefExpr->getType()->getNodeType() == core::NT_VectorType ) {
			return subExpr;
		}

		// In the case the target type of the cast is not a reftype we deref the subexpression
		if ( !convFact.builder.getBasicGenerator().isNull(nonRefExpr) && type->getNodeType() != core::NT_RefType ) {
			subExpr = nonRefExpr;
		}

		// LOG(DEBUG) << *subExpr << "(" << *subExpr->getType() << ") -> " << *type;
		// Convert casts form scalars to vectors to vector init exrpessions
		subExpr = convFact.mgr.basic.scalarToVector(type, subExpr);

        core::ExpressionPtr&& retExpr =
        		(type != subExpr->getType() ? convFact.builder.castExpr( type, subExpr ) : subExpr);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCastExpr(clang::CastExpr* castExpr) {
		START_LOG_EXPR_CONVERSION(castExpr);
		const core::TypePtr& type = convFact.convertType( GET_TYPE_PTR(castExpr) );
		core::ExpressionPtr&& subExpr = Visit(castExpr->getSubExpr());

		// if the cast is to a 'void*' type and the subexpr is a 0 it should be
		// replaced with a null literal
		if (( type->getNodeType() == core::NT_ArrayType ) &&
				*subExpr == *convFact.builder.literal(subExpr->getType(),"0") ) {
			return convFact.builder.getNodeManager().basic.getNull();
		}

		// Mallocs/Allocs are replaced with ref.new expression
		if(core::ExpressionPtr&& retExpr = handleMemAlloc(convFact.getASTBuilder(), type, subExpr))
			return retExpr;

		// In the case the target type of the cast is not a reftype we deref the subexpression
		if(!convFact.builder.getBasicGenerator().isNull(subExpr) && type->getNodeType() != core::NT_RefType) {
			subExpr = convFact.tryDeref(subExpr);
		}

		// LOG(DEBUG) << *subExpr << " -> " << *type;
		// Convert casts form scalars to vectors to vector init exrpessions
		subExpr = convFact.mgr.basic.scalarToVector(type, subExpr);

		core::ExpressionPtr&& retExpr =
				( type != subExpr->getType() ? convFact.builder.castExpr( type, subExpr ) : subExpr );
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							FUNCTION CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCallExpr(clang::CallExpr* callExpr) {
		START_LOG_EXPR_CONVERSION(callExpr);
		const core::ASTBuilder& builder = convFact.builder;

		if ( callExpr->getDirectCallee() ) {

			FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(callExpr->getDirectCallee());

			core::FunctionTypePtr funcTy =
				core::static_pointer_cast<const core::FunctionType>( convFact.convertType( GET_TYPE_PTR(funcDecl) ) );

			// collects the type of each argument of the expression
			ExpressionList args;
			for ( size_t argId = 0, end = callExpr->getNumArgs(); argId < end; ++argId ) {
				core::ExpressionPtr&& arg = Visit( callExpr->getArg(argId) );
				if ( argId < funcTy->getArgumentTypes().size() &&
						funcTy->getArgumentTypes()[argId]->getNodeType() == core::NT_RefType ) {
					if ( arg->getType()->getNodeType() != core::NT_RefType ) {
						if ( builder.getBasicGenerator().isString(arg->getType()) ) {
							arg = builder.callExpr( builder.getBasicGenerator().getStringToCharPointer(), arg );
						} else {
							arg = builder.refVar(arg);
						}
					}
				} else {
					arg = convFact.tryDeref(arg);
				}
				args.push_back( arg );
			}

			const TranslationUnit* oldTU = convFact.currTU;
			const FunctionDecl* definition = NULL;
			/*
			 * this will find function definitions if they are declared in  the same translation unit
			 * (also defined as static)
			 */
			if( !funcDecl->hasBody(definition) ) {
				/*
				 * if the function is not defined in this translation unit, maybe it is defined in another we already
				 * loaded use the clang indexer to lookup the definition for this function declarations
				 */
				clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, convFact.program.getClangProgram());
				ConversionFactory::TranslationUnitPair&& ret =
						convFact.program.getClangIndexer().getDefinitionFor(funcEntity);

				if ( ret.first ) {
					definition = ret.first;
					assert(ret.second && "Error while loading translation unit for function definition");
					convFact.currTU = &Program::getTranslationUnit(ret.second);
				}

			}

			if ( !definition ) {
				//-----------------------------------------------------------------------------------------------------
				//     						Handle of 'special' built-in functions
				//-----------------------------------------------------------------------------------------------------
				// free(): check whether this is a call to the free() function
				if ( funcDecl->getNameAsString() == "free" && callExpr->getNumArgs() == 1 ) {
					// in the case the free uses an input parameter
					if ( args[0]->getType()->getNodeType() == core::NT_RefType ) {
						return builder.callExpr( builder.getBasicGenerator().getRefDelete(), args[0] );
					}
					// otherwise this is not a L-Value so it needs to be wrapped into a variable
					return builder.callExpr( builder.getBasicGenerator().getRefDelete(),
							wrapVariable(callExpr->getArg(0))
						);
				}
			}

			ExpressionList&& packedArgs = tryPack(convFact.builder, funcTy, args);

			if ( !definition ) {
				// No definition has been found in any of the translation units, we mark this function as extern!
				core::ExpressionPtr&& irNode = convFact.builder.callExpr(
						funcTy->getReturnType(), builder.literal(funcDecl->getNameAsString(), funcTy), packedArgs
					);
				// handle eventual pragmas attached to the Clang node
				core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(irNode, callExpr, convFact);
				return annotatedNode;
			}

			/*
			 * We find a definition, we lookup if this variable needs to access the globals, in that case the capture
			 * list needs to be initialized with the value of global variable in the current scope
			 */
			if ( ctx.globalFuncMap.find(definition) != ctx.globalFuncMap.end() ) {
				/*
				 * we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
				 * current context
				 */
				assert(ctx.globalVar && "No global definitions forwarded to this point");
				packedArgs.insert(packedArgs.begin(), ctx.globalVar);
			}

			/*
			 * If we are resolving the body of a recursive function we have to return the associated variable every
			 * time a function in the strongly connected graph of function calls is encountered.
			 */
			if ( ctx.isResolvingRecFuncBody ) {
				// check if this type has a typevar already associated, in such case return it
				ConversionContext::RecVarExprMap::const_iterator fit = ctx.recVarExprMap.find(definition);
				if( fit != ctx.recVarExprMap.end() ) {
					/*
					 * we are resolving a parent recursive type, so when one of the recursive functions in the
					 * connected components are called, the introduced mu variable has to be used instead.
					 */
					convFact.currTU = oldTU;
					return builder.callExpr(
							funcTy->getReturnType(), static_cast<core::ExpressionPtr>(fit->second), packedArgs
						);
				}
			}

			if ( !ctx.isResolvingRecFuncBody ) {
				ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(definition);
				if ( fit != ctx.lambdaExprCache.end() ) {
					convFact.currTU = oldTU;
					core::ExpressionPtr&& irNode =
							builder.callExpr(funcTy->getReturnType(),
									static_cast<core::ExpressionPtr>(fit->second), packedArgs
								);
					// handle eventual pragmas attached to the Clang node
					core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(irNode, callExpr, convFact);
					return annotatedNode;
				}
			}

			assert(definition && "No definition found for function");
			core::ExpressionPtr lambdaExpr =
					core::static_pointer_cast<const core::LambdaExpr>( convFact.convertFunctionDecl(definition) );
			convFact.currTU = oldTU;

			core::ExpressionPtr&& irNode = builder.callExpr(funcTy->getReturnType(), lambdaExpr, packedArgs);
			// handle eventual pragmas attached to the Clang node
			core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(irNode, callExpr, convFact);
			return annotatedNode;
		}
		if ( callExpr->getCallee() ) {
			core::ExpressionPtr funcPtr = convFact.tryDeref( Visit( callExpr->getCallee() ) );
			const core::TypePtr& subTy = funcPtr->getType();
			if ( subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType ) {
				const core::TypePtr& subVecTy =
						core::static_pointer_cast<const core::SingleElementType>(subTy)->getElementType();

				funcPtr = builder.callExpr( subVecTy, builder.getBasicGenerator().getArraySubscript1D(), funcPtr, builder.uintLit(0) );
			}

			ExpressionList args;
			core::FunctionTypePtr funcTy = core::static_pointer_cast<const core::FunctionType>( funcPtr->getType() );
			for ( size_t argId = 0, end = callExpr->getNumArgs(); argId < end; ++argId ) {
				core::ExpressionPtr&& arg = Visit( callExpr->getArg(argId) );
				if ( argId < funcTy->getArgumentTypes().size() &&
						funcTy->getArgumentTypes()[argId]->getNodeType() == core::NT_RefType ) {
					if ( arg->getType()->getNodeType() != core::NT_RefType ) {
						if ( builder.getBasicGenerator().isString(arg->getType()) ) {
							arg = builder.callExpr( builder.getBasicGenerator().getStringToCharPointer(), arg );
						} else {
							arg = builder.refVar(arg);
						}
					}
				} else {
					arg = convFact.tryDeref(arg);
				}
				args.push_back( arg );
			}
			return  builder.callExpr( funcPtr, args );
		}
		assert(false && "Call expression not referring a function");
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							PREDEFINED EXPRESSION
	//
	// [C99 6.4.2.2] - A predefined identifier such as __func__.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitPredefinedExpr(clang::PredefinedExpr* preExpr) {
		return convFact.builder.getBasicGenerator().getNull(); // FIXME
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						SIZEOF ALIGNOF EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitSizeOfAlignOfExpr(clang::SizeOfAlignOfExpr* expr) {
		START_LOG_EXPR_CONVERSION(expr);
		if ( expr->isSizeOf() ) {
			core::TypePtr&& type = expr->isArgumentType() ?
				convFact.convertType( expr->getArgumentType().getTypePtr() ) :
				convFact.convertType( expr->getArgumentExpr()->getType().getTypePtr() );
			return getSizeOfType(convFact.getASTBuilder(), type);
		}
		assert(false && "SizeOfAlignOfExpr not yet supported");
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
	//
	// [C99 6.5.2.3] Structure and Union Members. X->F and X.F.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitMemberExpr(clang::MemberExpr* membExpr)  {
		START_LOG_EXPR_CONVERSION(membExpr);
		const core::ASTBuilder& builder = convFact.builder;

		core::ExpressionPtr&& base = Visit(membExpr->getBase());
		const core::lang::BasicGenerator& gen = builder.getBasicGenerator();
		if(membExpr->isArrow()) {
			/*
			 * we have to check whether we currently have a ref or probably an array (which is used to represent
			 * C pointers)
			 */
			assert( base->getType()->getNodeType() == core::NT_RefType);

			const core::TypePtr& subTy =
					core::static_pointer_cast<const core::RefType>(base->getType())->getElementType();

			if(subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType ) {
				const core::SingleElementTypePtr& vecTy =
						core::static_pointer_cast<const core::SingleElementType>(subTy);
				base = builder.callExpr(
						builder.refType(vecTy->getElementType()), gen.getArrayRefElem1D(), base, builder.uintLit(0)
					);
			}
		}

		core::IdentifierPtr ident = builder.identifier(membExpr->getMemberDecl()->getNameAsString());
		core::ExpressionPtr retExpr;

		// we have a ref type we should use the struct.ref member access
		if ( base->getType()->getNodeType() == core::NT_RefType ) {
			// There are 2 basic cases which need to be handled: Struct/Unions and Recursive Types
			core::TypePtr structTy = core::static_pointer_cast<const core::RefType>(base->getType())->getElementType();
			assert((structTy->getNodeType() == core::NT_StructType || structTy->getNodeType() == core::NT_UnionType  ||
					structTy->getNodeType() == core::NT_RecType) &&
					"Using a member access operation on a non struct/union type"
				);

			// if the inner type is a RecType then we need to unroll it to get the contained composite type
			if ( structTy->getNodeType() == core::NT_RecType ) {
				structTy = core::static_pointer_cast<const core::RecType>(structTy)->unroll(convFact.mgr);
			}

			const core::TypePtr& memberTy =
					core::static_pointer_cast<const core::NamedCompositeType>(structTy)->getTypeOfMember(ident);

			retExpr = builder.callExpr( builder.refType(memberTy),
						  gen.getCompositeRefElem(),
						  toVector<core::ExpressionPtr>(base, gen.getIdentifierLiteral(ident),
								  gen.getTypeLiteral(memberTy)
						  )
					);

		} else {
			retExpr = builder.memberAccessExpr(base, ident);
		}
		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, membExpr, convFact);

		return annotatedNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							BINARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitBinaryOperator(clang::BinaryOperator* binOp)  {
		START_LOG_EXPR_CONVERSION(binOp);
		const core::ASTBuilder& builder = convFact.builder;
		const core::lang::BasicGenerator& gen = builder.getBasicGenerator();

 		core::ExpressionPtr&& rhs = Visit(binOp->getRHS());
		core::ExpressionPtr&& lhs = Visit(binOp->getLHS());

		/*
		 * if the binary operator is a comma separated expression, we convert it into a function call which returns the
		 * value of the last expression
		 */
		if ( binOp->getOpcode() == BO_Comma ) {

			core::CompoundStmtPtr&& body = builder.compoundStmt(toVector<core::StatementPtr>(lhs,
					(gen.isUnit(rhs->getType()) ? static_cast<core::StatementPtr>(rhs) : builder.returnStmt(rhs)) )
				);
			return builder.createCallExprFromBody(body, rhs->getType());
		}

		// the type of this expression is the type of the LHS expression
		core::TypePtr exprTy = lhs->getType()->getNodeType() == core::NT_RefType ?
				core::static_pointer_cast<const core::RefType>(lhs->getType())->getElementType() : lhs->getType();

		// get basic element type
        core::ExpressionPtr&& subExprLHS = convFact.tryDeref(lhs);

        /*
         * we take care of compound operators first, we rewrite the RHS expression in a normal form, i.e.:
         * 		->		a op= b  ---->  a = a op b
         */
		clang::BinaryOperatorKind baseOp;
		core::lang::BasicGenerator::Operator op;
		bool isCompound = true;

		switch ( binOp->getOpcode() ) {
		// a *= b
		case BO_MulAssign: 	op = core::lang::BasicGenerator::Mul; baseOp = BO_Mul; break;
		// a /= b
		case BO_DivAssign: 	op = core::lang::BasicGenerator::Div; baseOp = BO_Div; break;
		// a %= b
		case BO_RemAssign:	op = core::lang::BasicGenerator::Mod; baseOp = BO_Rem; break;
		// a += b
		case BO_AddAssign: 	op = core::lang::BasicGenerator::Add; baseOp = BO_Add; break;
		// a -= b
		case BO_SubAssign:	op = core::lang::BasicGenerator::Sub; baseOp = BO_Sub; break;
		// a <<= b
		case BO_ShlAssign: 	op = core::lang::BasicGenerator::LShift; baseOp = BO_Shl; break;
		// a >>= b
		case BO_ShrAssign: 	op = core::lang::BasicGenerator::RShift; baseOp = BO_Shr; break;
		// a &= b
		case BO_AndAssign: 	op = core::lang::BasicGenerator::And; baseOp = BO_And; break;
		// a |= b
		case BO_OrAssign: 	op = core::lang::BasicGenerator::Or; baseOp = BO_Or; break;
		// a ^= b
		case BO_XorAssign: 	op = core::lang::BasicGenerator::Xor; baseOp = BO_Xor; break;
		default:
			isCompound = false;
		}

		if ( isCompound ) {
			// we check if the RHS is a ref, in that case we use the deref operator
			rhs = convFact.tryDeref(rhs);
			core::ExpressionPtr&& opFunc = gen.getOperator(exprTy, op);
			rhs = builder.callExpr(exprTy, opFunc, subExprLHS, rhs);
		}

		bool isAssignment = false;
		bool isLogical = false;

		baseOp = binOp->getOpcode();

		core::ExpressionPtr opFunc;
		switch ( binOp->getOpcode() ) {
		case BO_PtrMemD:
		case BO_PtrMemI:
			assert(false && "Operator not yet supported!");

		// a * b
		case BO_Mul: 	op = core::lang::BasicGenerator::Mul;  break;
		// a / b
		case BO_Div: 	op = core::lang::BasicGenerator::Div;  break;
		// a % b
		case BO_Rem: 	op = core::lang::BasicGenerator::Mod;  break;
		// a + b
		case BO_Add: 	op = core::lang::BasicGenerator::Add;  break;
		// a - b
		case BO_Sub: 	op = core::lang::BasicGenerator::Sub;  break;
		// a << b
		case BO_Shl: 	op = core::lang::BasicGenerator::LShift;  break;
		// a >> b
		case BO_Shr: 	op = core::lang::BasicGenerator::RShift;  break;
		// a & b
		case BO_And: 	op = core::lang::BasicGenerator::And;  break;
		// a ^ b
		case BO_Xor: 	op = core::lang::BasicGenerator::Xor;  break;
		// a | b
		case BO_Or:  	op = core::lang::BasicGenerator::Or; 	 break;

		// Logic operators

		// a && b
		case BO_LAnd: 	op = core::lang::BasicGenerator::LAnd; isLogical=true; break;
		// a || b
		case BO_LOr:  	op = core::lang::BasicGenerator::LOr;  isLogical=true; break;
		// a < b
		case BO_LT:	 	op = core::lang::BasicGenerator::Lt;   isLogical=true; break;
		// a > b
		case BO_GT:  	op = core::lang::BasicGenerator::Gt;   isLogical=true; break;
		// a <= b
		case BO_LE:  	op = core::lang::BasicGenerator::Le;   isLogical=true; break;
		// a >= b
		case BO_GE:  	op = core::lang::BasicGenerator::Ge;   isLogical=true; break;
		// a == b
		case BO_EQ:  	op = core::lang::BasicGenerator::Eq;   isLogical=true; break;
		// a != b
		case BO_NE:	 	op = core::lang::BasicGenerator::Ne;   isLogical=true; break;

		case BO_MulAssign: case BO_DivAssign: case BO_RemAssign: case BO_AddAssign: case BO_SubAssign:
		case BO_ShlAssign: case BO_ShrAssign: case BO_AndAssign: case BO_XorAssign: case BO_OrAssign:
		case BO_Assign:
		{
			baseOp = BO_Assign;
			/*
			 * poor C codes assign value to function parameters, this is not allowed here as input parameters are of
			 * non REF type. What we need to do is introduce a declaration for these variables and use the created
			 * variable on the stack instead of the input parameters
			 */
			lhs = wrapVariable(binOp->getLHS());

			// This is an assignment, we have to make sure the LHS operation is of type ref<a'>
			assert( core::dynamic_pointer_cast<const core::RefType>(lhs->getType()) &&
					"LHS operand must be of type ref<a'>."
				);
			isAssignment = true;
			opFunc = gen.getRefAssign();
			exprTy = gen.getUnit();
			break;
		}
		default:
			assert(false && "Operator not supported");
		}
		rhs = convFact.tryDeref(rhs);

		// Operators && and || introduce short circuit operations, this has to be directly supported in the IR.
		if ( baseOp == BO_LAnd || baseOp == BO_LOr ) {
			if( !gen.isBool(lhs->getType()) ) {
				lhs = builder.castExpr(gen.getBool(), lhs);
			}
			if( !convFact.mgr.basic.isBool(rhs->getType()) ) {
				rhs = builder.castExpr(gen.getBool(), rhs);
			}
			// lazy evaluation of RHS
			exprTy = gen.getBool();
			rhs = builder.createCallExprFromBody(builder.returnStmt(rhs), gen.getBool(), true);
		}

		if( !isAssignment ) {
			lhs = convFact.tryDeref(lhs);

			// Handle pointers arithmetic
			VLOG(2) << "Lookup for operation: " << op << ", for type: " << *exprTy;
			opFunc = gen.getOperator(exprTy, op);

			if ( DeclRefExpr* declRefExpr = utils::skipSugar<DeclRefExpr>(binOp->getLHS()) ) {
				if ( isa<ArrayType>(declRefExpr->getDecl()->getType().getTypePtr()) )
					assert(false && "Pointer arithmetic not yet supported");
			}

			if(isLogical)
				exprTy = gen.getBool();
		}
		assert(opFunc);

        core::ExpressionPtr&& retExpr = convFact.builder.callExpr( exprTy, opFunc, lhs, rhs );
		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, binOp, convFact);

		END_LOG_EXPR_CONVERSION( retExpr );
		return annotatedNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							UNARY OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitUnaryOperator(clang::UnaryOperator *unOp) {
		START_LOG_EXPR_CONVERSION(unOp);
		const core::ASTBuilder& builder = convFact.builder;
		const core::lang::BasicGenerator& gen = builder.getBasicGenerator();
		core::ExpressionPtr&& subExpr = Visit(unOp->getSubExpr());

		// build lambda expression for post/pre increment/decrement unary operators
		auto encloseIncrementOperator =
			[ this, &builder, &gen ]
			(core::ExpressionPtr subExpr, core::lang::BasicGenerator::Operator op) -> core::ExpressionPtr {
				core::TypePtr type = subExpr->getType();
				assert( type->getNodeType() == core::NT_RefType &&
						"Illegal increment/decrement operand - not a ref type" );
				core::TypePtr elementType = core::static_pointer_cast<const core::RefType>(type)->getElementType();

				core::TypePtr genType;
				if ( gen.isSignedInt(elementType) ) {
					genType = gen.getIntGen();
				} else if ( gen.isUnsignedInt(elementType) ) {
					genType = gen.getUIntGen();
				} else {
					assert(false && "Illegal operand type for increment/decrement operator.");
				}
				return convFact.builder.callExpr(elementType, convFact.mgr.basic.getOperator(genType, op), subExpr);
			};

		switch ( unOp->getOpcode() ) {
		// conversion of post increment/decrement operation is done by creating a tuple expression i.e.:
		// a++ ==> (__tmp = a, a=a+1, __tmp)
		// ++a ==> ( a=a+1, a)
		// --a
		case UO_PreDec:
			subExpr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PreDec);
			break;
		// a--
		case UO_PostDec:
			subExpr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PostDec);
			break;
		// a++
		case UO_PreInc:
			subExpr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PreInc);
			break;
		// ++a
		case UO_PostInc:
			subExpr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PostInc);
			break;
		// &a
		case UO_AddrOf:
		{
			/*
			 * We need to be careful paramvars are not dereferenced and the address passed around. If this happens
			 * we have to declare a variable holding the memory location for that value and replace every use of
			 * the paramvar with the newly generated variable: the structure needRef in the ctx is used for this
			 */
			core::ExpressionPtr&& expr = wrapVariable(unOp->getSubExpr());
			assert(expr->getType()->getNodeType() == core::NT_RefType);

			subExpr = builder.callExpr( builder.getBasicGenerator().getScalarToArray(),  expr );
			break;
		}
		// *a
		case UO_Deref: {
			assert(subExpr->getType()->getNodeType() == core::NT_RefType &&
					"Impossible to apply * operator to an R-Value");
			const core::TypePtr& subTy =
					core::static_pointer_cast<const core::RefType>(subExpr->getType())->getElementType();
			if ( subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType ) {
				const core::TypePtr& subVecTy =
						core::static_pointer_cast<const core::SingleElementType>(subTy)->getElementType();

				subExpr = builder.callExpr(
					builder.refType(subVecTy), gen.getArrayRefElem1D(), subExpr, builder.literal("0", gen.getUInt4())
				);
			} else {
				subExpr = convFact.tryDeref(subExpr);
			}
			break;
		}
		// +a
		case UO_Plus:
			// just return the subexpression
			break;
		// -a
		case UO_Minus:
			subExpr = builder.invertSign( convFact.tryDeref(subExpr) );
			break;
		// ~a
		case UO_Not:
			subExpr = convFact.tryDeref(subExpr);
			subExpr = builder.callExpr(
					subExpr->getType(), gen.getOperator(subExpr->getType(), core::lang::BasicGenerator::Not), subExpr
				);
			break;
		// !a
		case UO_LNot:
			subExpr = convFact.tryDeref(subExpr);
			if( !gen.isBool(subExpr->getType()) ) {
				// for now add a cast expression to bool FIXME
				subExpr = convFact.getASTBuilder().castExpr(gen.getBool(), subExpr);
			}
			assert( gen.isBool(subExpr->getType()) );
			subExpr = builder.callExpr( subExpr->getType(), gen.getBoolLNot(), subExpr );
			break;
		case UO_Real:
		case UO_Imag:
		case UO_Extension: //TODO:
		default:
			assert(false && "Unary operator not supported");
		}

		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(subExpr, unOp, convFact);

		// add the operator name in order to help the convertion process in the backend
		subExpr->addAnnotation(
				std::make_shared<c_info::COpAnnotation>( UnaryOperator::getOpcodeStr(unOp->getOpcode()) )
			);

		return annotatedNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							CONDITIONAL OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitConditionalOperator(clang::ConditionalOperator* condOp) {
		START_LOG_EXPR_CONVERSION(condOp);
		const core::ASTBuilder& builder = convFact.builder;
		const core::lang::BasicGenerator& gen = builder.getBasicGenerator();

		core::TypePtr&& retTy = convFact.convertType( GET_TYPE_PTR(condOp) );
		core::ExpressionPtr&& trueExpr = Visit(condOp->getTrueExpr());
		core::ExpressionPtr&& falseExpr = Visit(condOp->getFalseExpr());
		core::ExpressionPtr&& condExpr = Visit( condOp->getCond() );

		// add ref.deref if needed
		condExpr = convFact.tryDeref(condExpr);

		if ( !gen.isBool(condExpr->getType()) ) {
			// the return type of the condition is not a boolean, we add a cast expression
			condExpr = builder.castExpr(gen.getBool(), condExpr); // FIXME
		}

		// Dereference eventual references
		if ( retTy->getNodeType() == core::NT_RefType ) {
			retTy= core::static_pointer_cast<const core::RefType>(retTy)->getElementType();
		}

		core::ExpressionPtr&& retExpr = builder.callExpr(retTy, gen.getIfThenElse(),
				condExpr,	// Condition
				builder.createCallExprFromBody( builder.returnStmt(convFact.tryDeref(trueExpr)),  retTy, true ), // True
				builder.createCallExprFromBody( builder.returnStmt(convFact.tryDeref(falseExpr)),  retTy, true ) // False
		);

		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, condOp, convFact);

		END_LOG_EXPR_CONVERSION(retExpr);
		return annotatedNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						ARRAY SUBSCRIPT EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitArraySubscriptExpr(clang::ArraySubscriptExpr* arraySubExpr) {
		START_LOG_EXPR_CONVERSION(arraySubExpr);

		const core::lang::BasicGenerator& gen = convFact.builder.getBasicGenerator();
		/*
		 * CLANG introduces implicit cast for the base expression of array subscripts which cast the array type into a
		 * simple pointer. As insieme supports subscripts only for array or vector types, we skip eventual implicit
		 * cast operations.
		 */
		Expr* baseExpr = arraySubExpr->getBase();
		while ( ImplicitCastExpr *castExpr = dyn_cast<ImplicitCastExpr>(baseExpr) ) {
			baseExpr = castExpr->getSubExpr();
		}

		// IDX
		core::ExpressionPtr&& idx = convFact.tryDeref( Visit( arraySubExpr->getIdx() ) );

		// BASE
		core::ExpressionPtr&& base = Visit( baseExpr );

		core::TypePtr opType;
		core::LiteralPtr op;

		if ( base->getType()->getNodeType() == core::NT_RefType ) {
			// The vector/array is an L-Value so we use the array.ref.elem
			// operator to return a reference to the addressed memory location
			const core::RefTypePtr& refSubTy = core::static_pointer_cast<const core::RefType>(base->getType());

			// TODO: we need better checking for vector type
			assert( (refSubTy->getElementType()->getNodeType() == core::NT_VectorType ||
					 refSubTy->getElementType()->getNodeType() == core::NT_ArrayType) &&
					"Base expression of array subscript is not a vector/array type.");

			op = gen.getArrayRefElem1D();
			opType = convFact.builder.refType(
				core::static_pointer_cast<const core::SingleElementType>(refSubTy->getElementType())->getElementType()
			);

		} else {
			/*
			 * The vector/array is an R-value (e.g. (int[2]){0,1}[1] ) in this case the subscript returns an R-value so
			 * the array.subscript operator must be used
			 */
			// TODO: we need better checking for vector type
			assert( (base->getType()->getNodeType() == core::NT_VectorType ||
					 base->getType()->getNodeType() == core::NT_ArrayType) &&
					"Base expression of array subscript is not a vector/array type.");

			op = gen.getArraySubscript1D();
			opType = core::static_pointer_cast<const core::SingleElementType>(base->getType())->getElementType();
		}

		core::ExpressionPtr&& retExpr =
				convFact.builder.callExpr( opType, op, base, convFact.builder.castExpr(gen.getUInt4(), idx) );

		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, arraySubExpr, convFact);
		END_LOG_EXPR_CONVERSION(retExpr);
		return annotatedNode;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						EXT VECTOR ELEMENT EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitExtVectorElementExpr(ExtVectorElementExpr* vecElemExpr){
        START_LOG_EXPR_CONVERSION(vecElemExpr);
        core::ExpressionPtr&& base = Visit( vecElemExpr->getBase() );
        const core::lang::BasicGenerator& gen = convFact.builder.getBasicGenerator();

        std::string pos;
        llvm::StringRef&& accessor = vecElemExpr->getAccessor().getName();

        core::TypePtr&& exprTy = convFact.convertType( GET_TYPE_PTR(vecElemExpr) );

        //translate OpenCL accessor string to index
        if ( accessor == "x" ) 		pos = "0";
        else if ( accessor == "y" ) pos = "1";
        else if ( accessor == "z" )	pos = "2";
        else if ( accessor == "w" )	pos = "3";
	    else if ( accessor.front() == 's' || accessor.front() == 'S' ){
        	// the input string is in a form sXXX
        	// we skip the s and return the value to get the number
        	llvm::StringRef numStr = accessor.substr(1,accessor.size()-1);
        	assert( insieme::utils::numeric_cast<unsigned int>(numStr.data()) >= 0 &&
        			"Vector accessing string is not a number" );
        	pos = numStr;
	    } else if ( accessor.size() <= 4 ){ // opencl vector permutation
            vector<core::ExpressionPtr> args;

            for ( auto I = accessor.begin(), E = accessor.end(); I != E; ++I ) {
                args.push_back(convFact.builder.intLit(*I == 'w' ? 3 : (*I)-'x')); //convert x, y, z, w to 0, 1, 2, 3
            }
            return convFact.builder.callExpr(
            		gen.getVectorPermute(), convFact.tryDeref(base), convFact.builder.vectorExpr(args)
            	);
        } else {
            assert(accessor.size() <= 4 && "ExtVectorElementExpr has unknown format");
        }

        // The type of the indes is always uint<4>
        core::ExpressionPtr&& idx = convFact.builder.literal(pos, gen.getUInt4());
        // if the type of the vector is a refType, we deref it
        base = convFact.tryDeref(base);

        core::ExpressionPtr&& retExpr = convFact.builder.callExpr(exprTy, gen.getVectorSubscript(), base, idx);

        END_LOG_EXPR_CONVERSION(retExpr);
        return retExpr;
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VAR DECLARATION REFERENCE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
		START_LOG_EXPR_CONVERSION(declRef);
		// check whether this is a reference to a variable
		core::ExpressionPtr retExpr;
		if ( VarDecl* varDecl = dyn_cast<VarDecl>(declRef->getDecl()) ) {
			retExpr = convFact.lookUpVariable( varDecl );
		} else if( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(declRef->getDecl()) ) {
			retExpr = core::static_pointer_cast<const core::Expression>( convFact.convertFunctionDecl(funcDecl) );
		} else {
			// todo: C++ check whether this is a reference to a class field, or method (function).
			assert(false && "DeclRefExpr not supported!");
		}

		// handle eventual pragmas attached to the Clang node
		core::ExpressionPtr&& annotatedNode = omp::attachOmpAnnotation(retExpr, declRef, convFact);
		END_LOG_EXPR_CONVERSION(retExpr);

		return annotatedNode;
	}

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //                  VECTOR/STRUCT INITALIZATION EXPRESSION
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitInitListExpr(clang::InitListExpr* initList) {
		assert(false && "Visiting of initializer list is not allowed!");
    }

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//                  	COMPOUND LITERAL EXPRESSION
	// Introduced in C99 6.5.2.5, used to initialize structures or arrays with
	// the { } expression, example:
	// 		strcut A a;
	// 		a = (struct A) { 10, 20, 30 };
	//
	//	or:
	//		((int [3]){1,2,3})[2]  -> 2
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCompoundLiteralExpr(clang::CompoundLiteralExpr* compLitExpr) {

		if ( clang::InitListExpr* initList = dyn_cast<clang::InitListExpr>(compLitExpr->getInitializer()) ) {
			return convFact.convertInitExpr(initList, convFact.convertType(compLitExpr->getType().getTypePtr()), false);
		}
		return Visit(compLitExpr->getInitializer());
	}
};

ConversionFactory::ClangExprConverter* ConversionFactory::makeExprConvert(ConversionFactory& fact, Program& program) {
	return new ClangExprConverter(fact, program);
}

void ConversionFactory::cleanExprConvert(ConversionFactory::ClangExprConverter* exprConv) {
	delete exprConv;
}

core::ExpressionPtr ConversionFactory::convertExpr(const clang::Expr* expr) const {
	assert(expr && "Calling convertExpr with a NULL pointer");
	return exprConv->Visit( const_cast<Expr*>(expr) );
}

/**
 * InitListExpr describes an initializer list, which can be used to initialize objects of different types,
 * InitListExpr including struct/class/union types, arrays, and vectors. For example:
 *
 * struct foo x = { 1, { 2, 3 } };
 *
 * In insieme this statement has to tranformed into a StructExpr, or VectorExpr depending on the type of the
 * LHS expression.
 */
core::ExpressionPtr
ConversionFactory::convertInitializerList(const clang::InitListExpr* initList, const core::TypePtr& type) const {
	bool isRef = false;
	core::TypePtr currType = type;
	if ( core::RefTypePtr&& refType = core::dynamic_pointer_cast<const core::RefType>(type) ) {
		isRef = true;
		currType = refType->getElementType();
	}

	core::ExpressionPtr retExpr;
	if ( currType->getNodeType() == core::NT_VectorType || currType->getNodeType() == core::NT_ArrayType ) {
		const core::TypePtr& elemTy =
				core::static_pointer_cast<const core::SingleElementType>(currType)->getElementType();
		ExpressionList elements;
		// get all values of the init expression
		for ( size_t i = 0, end = initList->getNumInits(); i < end; ++i ) {
			const clang::Expr* subExpr = initList->getInit(i);
			core::ExpressionPtr convExpr = convertInitExpr(subExpr, elemTy, false);
			// If the type is a refType we have to add a VAR.REF operation
			if ( !core::analysis::isCallOf(convExpr, mgr.basic.getRefVar()) ) {
				convExpr = builder.refVar(convExpr);
			}
			elements.push_back( convExpr );
		}
		retExpr = builder.vectorExpr(elements);
	}

	/*
	 * in the case the initexpr is used to initialize a struct/class we need to create a structExpr to initialize the
	 * structure
	 */
	if ( core::StructTypePtr&& structTy = core::dynamic_pointer_cast<const core::StructType>(currType) ) {
		core::StructExpr::Members members;
		for ( size_t i = 0, end = initList->getNumInits(); i < end; ++i ) {
			const core::NamedCompositeType::Entry& curr = structTy->getEntries()[i];
			members.push_back(
				core::StructExpr::Member(curr.first, convertInitExpr(initList->getInit(i), curr.second, false))
			);
		}
		retExpr = builder.structExpr(members);
	}

	assert(retExpr && "Couldn't convert initialization expression");

	if ( isRef ) {
		retExpr = builder.refVar( retExpr );
	}
	// create vector initializator
	return retExpr;
}

namespace {

core::FunctionTypePtr addGlobalsToFunctionType(const core::ASTBuilder& builder,
						 	 	 	 	 	   const core::TypePtr& globals,
						 	 	 	 	 	   const core::FunctionTypePtr& funcType) {

	const std::vector<core::TypePtr>& oldArgs = funcType->getArgumentTypes();

	std::vector<core::TypePtr> argTypes(oldArgs.size()+1);

	std::copy(oldArgs.begin(), oldArgs.end(), argTypes.begin()+1);
	// function is receiving a reference to the global struct as the first argument
	argTypes[0] = builder.refType(globals);
	return builder.functionType( argTypes, funcType->getReturnType() );

}

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						CONVERT FUNCTION DECLARATION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::NodePtr ConversionFactory::convertFunctionDecl(const clang::FunctionDecl* funcDecl, bool isEntryPoint) {
	// the function is not extern, a lambdaExpr has to be created
	assert(funcDecl->hasBody() && "Function has no body!");
	assert(currTU);
	VLOG(1) << "~ Converting function: '" << funcDecl->getNameAsString() << "' isRec?: " << ctx.isRecSubFunc;

	VLOG(1) << "#----------------------------------------------------------------------------------#";
	VLOG(1) << "\nVisiting Function Declaration for: " << funcDecl->getNameAsString() << std::endl
			 << "-> at location: ("
			 << utils::location(funcDecl->getSourceRange().getBegin(), currTU->getCompiler().getSourceManager())
			 << "): " << std::endl
			 << "\tIsRecSubType: " << ctx.isRecSubFunc << std::endl
			 << "\tEmpty map: "    << ctx.recVarExprMap.size();

	if ( !ctx.isRecSubFunc ) {
		// add this type to the type graph (if not present)
		exprConv->funcDepGraph.addNode(funcDecl);
		if( VLOG_IS_ON(2) ) {
			exprConv->funcDepGraph.print( std::cout );
		}
	}

	// retrieve the strongly connected components for this type
	std::set<const FunctionDecl*>&& components = exprConv->funcDepGraph.getStronglyConnectedComponents( funcDecl );

	if ( !components.empty() ) {
		// we are dealing with a recursive type
		VLOG(1) << "Analyzing FuncDecl: " << funcDecl->getNameAsString() << std::endl
				<< "Number of components in the cycle: " << components.size();
		std::for_each(components.begin(), components.end(),
			[ ] (std::set<const FunctionDecl*>::value_type c) {
				VLOG(2) << "\t" << c->getNameAsString( ) << "(" << c->param_size() << ")";
			}
		);

		if ( !ctx.isRecSubFunc ) {
			if ( ctx.recVarExprMap.find(funcDecl) == ctx.recVarExprMap.end() ) {
				// we create a TypeVar for each type in the mutual dependence
				core::VariablePtr&& var = builder.variable( convertType( GET_TYPE_PTR(funcDecl) ) );
				ctx.recVarExprMap.insert( std::make_pair(funcDecl, var) );
			}
		} else {
			// we expect the var name to be in currVar
			ctx.recVarExprMap.insert(std::make_pair(funcDecl, ctx.currVar));
		}

		// when a subtype is resolved we expect to already have these variables in the map
		if ( !ctx.isRecSubFunc ) {
			std::for_each(components.begin(), components.end(),
				[ this ] (std::set<const FunctionDecl*>::value_type fd) {

					if ( this->ctx.recVarExprMap.find(fd) == this->ctx.recVarExprMap.end() ) {
						core::FunctionTypePtr funcType =
							core::static_pointer_cast<const core::FunctionType>( this->convertType(GET_TYPE_PTR(fd)) );
						// In the case the function is receiving the global variables the signature needs to be
						// modified by allowing the global struct to be passed as an argument
						if ( this->ctx.globalFuncMap.find(fd) != this->ctx.globalFuncMap.end() ) {
							funcType = addGlobalsToFunctionType(this->builder, this->ctx.globalStruct.first, funcType);
						}
						core::VariablePtr&& var = this->builder.variable( funcType );
						this->ctx.recVarExprMap.insert( std::make_pair(fd, var ) );
					}
				}
			);
		}
		if ( VLOG_IS_ON(2) ) {
			VLOG(2) << "MAP: ";
			std::for_each(ctx.recVarExprMap.begin(), ctx.recVarExprMap.end(),
				[] (ConversionContext::RecVarExprMap::value_type c) {
					VLOG(2) << "\t" << c.first->getNameAsString() << "[" << c.first << "]";
				}
			);
		}
	}

	vector<core::VariablePtr> params;
	/*
	 * before resolving the body we have to set the currGlobalVar accordingly depending if this function will use the
	 * global struct or not
	 */
	core::VariablePtr parentGlobalVar = ctx.globalVar;
	if ( !isEntryPoint && ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end() ) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		core::VariablePtr&& var = builder.variable( builder.refType(ctx.globalStruct.first) );
		params.push_back( var );
		ctx.globalVar = var;
	}

	std::for_each(funcDecl->param_begin(), funcDecl->param_end(),
		[ &params, this ] (ParmVarDecl* currParam) {
			params.push_back( core::static_pointer_cast<const core::Variable>( this->lookUpVariable(currParam) ) );
		}
	);

	// this lambda is not yet in the map, we need to create it and add it to the cache
	assert(!ctx.isResolvingRecFuncBody && "~~~ Something odd happened, you are allowed by all means to blame Simone ~~~");
	if ( !components.empty() ) {
		ctx.isResolvingRecFuncBody = true;
	}
	core::StatementPtr&& body = convertStmt( funcDecl->getBody() );
	/*
	 * if any of the parameters of this function has been marked as needRef, we need to add a declaration just before
	 * the body of this function
	 */
	vector<core::StatementPtr> decls;
	std::for_each(params.begin(), params.end(),
		[ &decls, &body, this ] (core::VariablePtr currParam) {
			auto fit = this->ctx.wrapRefMap.find(currParam);

			if ( fit != this->ctx.wrapRefMap.end() ) {
				decls.push_back( this->builder.declarationStmt(fit->second,	this->builder.refVar( fit->first ) ));
				/*
				 * replace this parameter in the body, example:
				 *
				 * int f(int a) {
				 *  for (...) {
				 *   x = a; <- if all the occurencies of a will not be replaced the semantics of
				 *   		   the code will not be preserved
				 *   a = i;
				 *  }
				 * }
				 *
				 *  as the variable can olny appear in the RHS of expression, we have to sobstitute it with its
				 *  dereference
				 */
				body = core::static_pointer_cast<const core::Statement>(
						core::transform::replaceAll( this->builder.getNodeManager(), body, fit->first,
								this->tryDeref(fit->second), true
							)
				);
			}

		}
	);

	// if we introduce new decls we have to introduce them just before the body of the function
	if(!decls.empty()) {
		// push the old body
		decls.push_back(body);
		body = builder.compoundStmt(decls);
	}
	ctx.isResolvingRecFuncBody = false;

	// ADD THE GLOBALS
	if ( isEntryPoint && ctx.globalVar ) {
		const core::CompoundStmtPtr& compStmt = core::static_pointer_cast<const core::CompoundStmt>(body);
		assert(ctx.globalVar && ctx.globalStruct.second);

		const StatementList& oldStmts = compStmt->getStatements();

		std::vector<core::StatementPtr> stmts(oldStmts.size()+1);
		stmts[0] = builder.declarationStmt(ctx.globalVar, builder.refNew( ctx.globalStruct.second ));
		std::copy(compStmt->getStatements().begin(), compStmt->getStatements().end(), stmts.begin()+1);

		body = builder.compoundStmt(stmts);
	}

	core::TypePtr convertedType = convertType( GET_TYPE_PTR(funcDecl) );
	assert(convertedType->getNodeType() == core::NT_FunctionType && "Converted type has to be a function type!");
	core::FunctionTypePtr funcType = core::static_pointer_cast<const core::FunctionType>(convertedType);

	// if this function gets the globals in the capture list we have to create a different type
	if ( !isEntryPoint && ctx.globalFuncMap.find(funcDecl) != ctx.globalFuncMap.end() ) {
		// declare a new variable that will be used to hold a reference to the global data stucture
		funcType = addGlobalsToFunctionType(builder, ctx.globalStruct.first, funcType);
	}

	// reset old global var
	ctx.globalVar = parentGlobalVar;

	if ( components.empty() ) {
		core::LambdaExprPtr&& retLambdaExpr = builder.lambdaExpr( funcType, params, body);
		// attach name annotation to the lambda
		retLambdaExpr->getLambda()->addAnnotation(
			std::make_shared<c_info::CNameAnnotation>( funcDecl->getNameAsString() )
		);

        // Adding the lambda function to the list of converted functions
        ctx.lambdaExprCache.insert( std::make_pair(funcDecl, retLambdaExpr) );

        return attachFuncAnnotations(retLambdaExpr, funcDecl);
	}

	core::LambdaPtr&& retLambdaNode = builder.lambda( funcType, params, body );
	// attach name annotation to the lambda
	retLambdaNode->addAnnotation( std::make_shared<c_info::CNameAnnotation>( funcDecl->getNameAsString() ) );
	// this is a recurive function call
	if ( ctx.isRecSubFunc ) {
		/*
		 * if we are visiting a nested recursive type it means someone else will take care of building the rectype
		 * node, we just return an intermediate type
		 */
		return retLambdaNode;
	}

	// we have to create a recursive type
	ConversionContext::RecVarExprMap::const_iterator tit = ctx.recVarExprMap.find(funcDecl);
	assert(tit != ctx.recVarExprMap.end() && "Recursive function has not VarExpr associated to himself");
	core::VariablePtr recVarRef = tit->second;

	core::LambdaDefinition::Definitions definitions;
	definitions.insert( std::make_pair(recVarRef, retLambdaNode) );

	// We start building the recursive type. In order to avoid loop the visitor
	// we have to change its behaviour and let him returns temporarely types
	// when a sub recursive type is visited.
	ctx.isRecSubFunc = true;

	std::for_each(components.begin(), components.end(),
		[ this, &definitions ] (std::set<const FunctionDecl*>::value_type fd) {

			ConversionContext::RecVarExprMap::const_iterator tit = this->ctx.recVarExprMap.find(fd);
			assert(tit != this->ctx.recVarExprMap.end() && "Recursive function has no TypeVar associated");
			this->ctx.currVar = tit->second;

			/*
			 * we remove the variable from the list in order to fool the solver, in this way it will create a descriptor
			 * for this type (and he will not return the TypeVar associated with this recursive type). This behaviour
			 * is enabled only when the isRecSubType flag is true
			 */
			this->ctx.recVarExprMap.erase(fd);

			/*
			 * if the function is not defined in this translation unit, maybe it is defined in another we already loaded
			 * use the clang indexer to lookup the definition for this function declarations
			 */
			clang::idx::Entity&& funcEntity =
					clang::idx::Entity::get(const_cast<FunctionDecl*>(fd), this->program.getClangProgram());
			ConversionFactory::TranslationUnitPair&& ret = this->program.getClangIndexer().getDefinitionFor(funcEntity);
			const TranslationUnit* oldTU = this->currTU;
			if ( ret.first ) {
				fd = ret.first;
				assert(ret.second && "Error loading translation unit for function definition");
				this->currTU = &Program::getTranslationUnit(ret.second);
			}

			const core::LambdaPtr& lambda =
					core::static_pointer_cast<const core::Lambda>(this->convertFunctionDecl(fd));
			assert(lambda && "Resolution of sub recursive lambda yields a wrong result");
			this->currTU = oldTU;
			// attach name annotation to the lambda
			lambda->addAnnotation( std::make_shared<c_info::CNameAnnotation>( fd->getNameAsString() ) );
			definitions.insert( std::make_pair(this->ctx.currVar, lambda) );

			// reinsert the TypeVar in the map in order to solve the other recursive types
			this->ctx.recVarExprMap.insert( std::make_pair(fd, this->ctx.currVar) );
			this->ctx.currVar = NULL;
		}
	);
	// we reset the behavior of the solver
	ctx.isRecSubFunc = false;

	core::LambdaDefinitionPtr&& definition = builder.lambdaDefinition(definitions);
	core::LambdaExprPtr&& retLambdaExpr = builder.lambdaExpr(recVarRef, definition);

	// Adding the lambda function to the list of converted functions
	ctx.lambdaExprCache.insert( std::make_pair(funcDecl, retLambdaExpr) );
	// we also need to cache all the other recursive definition, so when we will resolve
	// another function in the recursion we will not repeat the process again
	std::for_each(components.begin(), components.end(),
		[ this, &definition ] (std::set<const FunctionDecl*>::value_type fd) {
			auto fit = this->ctx.recVarExprMap.find(fd);
			assert(fit != this->ctx.recVarExprMap.end());
			core::ExpressionPtr&& func = builder.lambdaExpr(fit->second, definition);
			ctx.lambdaExprCache.insert( std::make_pair(fd, func) );

			func = this->attachFuncAnnotations(func, fd);
		}
	);
	return attachFuncAnnotations(retLambdaExpr, funcDecl);
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
