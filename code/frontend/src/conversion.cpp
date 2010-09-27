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

#include "logging.h"
#include "conversion.h"

#include "utils/types_lenght.h"
#include "utils/source_locations.h"
#include "utils/dep_graph.h"

#include "program.h"
#include "ast_node.h"
#include "types.h"
#include "statements.h"
#include "container_utils.h"
#include "lang_basic.h"
#include "numeric_cast.h"
#include "naming.h"
#include "ocl_annotations.h"

#include "clang/AST/ASTConsumer.h"
#include "clang/AST/ASTContext.h"
#include "clang/AST/StmtVisitor.h"
#include "clang/AST/TypeVisitor.h"

#include <boost/algorithm/string.hpp>

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
	// we use the getDecomposedSpellingLoc() method because in case we read macros values
	// we have to read the expanded value
	std::pair<FileID, unsigned> startLocInfo = srcMgr.getDecomposedSpellingLoc(start);
	llvm::StringRef startBuffer = srcMgr.getBufferData(startLocInfo.first);
	const char *strDataStart = startBuffer.begin() + startLocInfo.second;
//	DLOG(INFO) << "VALUE: " << string(strDataStart, clang::Lexer::MeasureTokenLength(start, srcMgr, clang::LangOptions()));
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

		ret.push_back( builder.callExpr(core::lang::OP_VAR_LIST_PACK_PTR, toPack) );
		return ret;
	}
	return args;
}

std::string getOperationType(const core::TypePtr& type) {
	using namespace core::lang;
	if(isUIntType(*type))	return "uint";
	if(isIntType(*type)) 	return "int";
	if(isBoolType(*type))	return "bool";
	if(isRealType(*type))	return "real";
	assert(false && "Type not supported");
}

} // End empty namespace

namespace insieme {
namespace frontend {
namespace conversion {

class ClangExprConverter: public StmtVisitor<ClangExprConverter, ExprWrapper> {
	ConversionFactory& convFact;

	// Map for resolved lambda functions
	typedef std::map<const FunctionDecl*, core::ExpressionPtr> LambdaExprMap;
	LambdaExprMap lambdaExprCache;

	utils::DependencyGraph<const FunctionDecl*> funcDepGraph;

	typedef std::map<const FunctionDecl*, core::VarExprPtr> RecVarExprMap;
	RecVarExprMap recVarExprMap;
	bool isRecSubType;
	core::VarExprPtr currVar;

public:
	ClangExprConverter(ConversionFactory& convFact): convFact(convFact), isRecSubType(false), currVar(NULL) { }

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
		// todo: Handle escape characters
		return ExprWrapper( convFact.builder.literal(
				GetStringFromStream(convFact.clangCtx->getSourceManager(), stringLit->getExprLoc()),
				convFact.builder.genericType(core::Identifier("string")))
		);
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

	ExprWrapper VisitParenExpr(clang::ParenExpr* parExpr) {
		return Visit( parExpr->getSubExpr() );
	}

	ExprWrapper VisitCastExpr(clang::CastExpr* castExpr) {
		const core::TypePtr& type = convFact.ConvertType( *castExpr->getType().getTypePtr() );
		const core::ExpressionPtr& subExpr = Visit(castExpr->getSubExpr()).ref;
		return ExprWrapper( convFact.builder.castExpr( type, subExpr ) );
	}

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
						convFact.ConvertType( *funcDecl->getType().getTypePtr() ),
						core::Identifier( boost::to_upper_copy(funcDecl->getNameAsString()) )))
				);
			} else {
				// we expect the var name to be in currVar
				recVarExprMap.insert(std::make_pair(funcDecl, currVar));
			}

			// when a subtype is resolved we aspect to already have these variables in the map
			if(!isRecSubType) {
				std::for_each(components.begin(), components.end(),
					[ this ] (std::set<const FunctionDecl*>::value_type fd) {

						// we count how many variables in the map refers to overloaded versions of the same function
						// this can happen when a function get overloaded and the cycle of recursion can happen between
						// the overloaded version, we need unique variable for each version of the function
						size_t num_of_overloads = std::count_if(this->recVarExprMap.begin(), this->recVarExprMap.end(),
							[ &fd ] (RecVarExprMap::value_type curr) {
								return fd->getName() == curr.first->getName();
							} );

						std::stringstream recVarName( boost::to_upper_copy(fd->getNameAsString()) );
						if(num_of_overloads)
							recVarName << num_of_overloads;

						this->recVarExprMap.insert( std::make_pair(fd,
								this->convFact.builder.varExpr(convFact.ConvertType(*fd->getType().getTypePtr()),recVarName.str())) );
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
		core::StatementPtr body = convFact.ConvertStmt( *funcDecl->getBody() );

		vector<core::ParamExprPtr> params;
		std::for_each(funcDecl->param_begin(), funcDecl->param_end(),
			[ &params, this ] (ParmVarDecl* currParam) {
				core::TypePtr paramTy = this->convFact.ConvertType( *currParam->getOriginalType().getTypePtr() );
				params.push_back( this->convFact.builder.paramExpr( paramTy, core::Identifier(currParam->getName()) ) );
			}
		);

		const core::ASTBuilder& builder = convFact.builder;
		retLambdaExpr = builder.lambdaExpr( convFact.ConvertType( *funcDecl->getType().getTypePtr() ), params, body);

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

	ExprWrapper VisitCallExpr(clang::CallExpr* callExpr) {
		if( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(callExpr->getDirectCallee()) ) {
			const core::ASTBuilder& builder = convFact.builder;

			// collects the type of each argument of the expression
			vector< core::ExpressionPtr > args;
			std::for_each(callExpr->arg_begin(), callExpr->arg_end(),
				[ &args, this ] (Expr* currArg) { args.push_back( this->Visit(currArg).ref ); }
			);

			core::FunctionTypePtr funcTy =
					core::dynamic_pointer_cast<const core::FunctionType>( convFact.ConvertType( *funcDecl->getType().getTypePtr() ) );

			vector< core::ExpressionPtr >&& packedArgs = tryPack(convFact.builder, funcTy, args);

			const FunctionDecl* definition = NULL;
			if( !funcDecl->hasBody(definition) ) {
				// in the case the function is extern, a literal is build
				return ExprWrapper( convFact.builder.callExpr(
						builder.literal(funcDecl->getNameAsString(), funcTy), packedArgs)
				);
			}

			if(!recVarExprMap.empty()) {
				// check if this type has a typevar already associated, in such case return it
				RecVarExprMap::const_iterator fit = recVarExprMap.find(definition);
				if( fit != recVarExprMap.end() ) {
					// we are resolving a parent recursive type, so we shouldn't
					return ExprWrapper( builder.callExpr(fit->second, packedArgs) );
				}
			}

			if(!isRecSubType) {
				LambdaExprMap::const_iterator fit = lambdaExprCache.find(definition);
				if(fit != lambdaExprCache.end())
					return fit->second;
			}

			assert(definition && "No definition found for function");
			core::ExpressionPtr lambdaExpr = VisitFunctionDecl(definition);

			// Adding the C function name as annotation
			lambdaExpr.addAnnotation(std::make_shared<insieme::c_info::CNameAnnotation>(definition->getName()));

			// Adding the lambda function to the list of converted functions
			lambdaExprCache.insert( std::make_pair(definition, lambdaExpr) );
			return ExprWrapper( builder.callExpr(lambdaExpr, packedArgs)  );
		}
		assert(false && "Call expression not referring a function");
	}

	ExprWrapper VisitCXXMemberCallExpr(clang::CXXMemberCallExpr* callExpr) {
		//todo: CXX extensions
		assert(false && "CXXMemberCallExpr not yet handled");
	}

	ExprWrapper VisitCXXOperatorCallExprr(clang::CXXOperatorCallExpr* callExpr) {
		//todo: CXX extensions
		assert(false && "CXXOperatorCallExpr not yet handled");
	}

	ExprWrapper VisitBinaryOperator(clang::BinaryOperator* binOp)  {
		const core::ASTBuilder& builder = convFact.builder;

		core::ExpressionPtr rhs = Visit(binOp->getRHS()).ref;
		const core::ExpressionPtr& lhs = Visit(binOp->getLHS()).ref;

		// if the binary operator is a comma separated expression, we convert it into
		// a tuple expression and return it
		if( binOp->getOpcode() == BO_Comma)
			return ExprWrapper( builder.tupleExpr({ lhs, rhs }) );

		core::TypePtr exprTy = convFact.ConvertType( *binOp->getType().getTypePtr() );

		// create Pair type
		core::TupleTypePtr tupleTy = builder.tupleType( { exprTy, exprTy } );
		std::string opType = getOperationType(exprTy);

		// we take care of compound operators first,
		// we rewrite the RHS expression in a normal form, i.e.:
		// a op= b  ---->  a = a op b
		std::string op;
		switch( binOp->getOpcode() ) {
		// a *= b
		case BO_MulAssign: op = "mul"; break;
		// a /= b
		case BO_DivAssign: op = "div"; break;
		// a %= b
		case BO_RemAssign: op = "mod"; break;
		// a += b
		case BO_AddAssign: op = "add"; break;
		// a -= b
		case BO_SubAssign: op = "sub"; break;
		// a <<= b
		case BO_ShlAssign: op = "shl"; break;
		// a >>= b
		case BO_ShrAssign: op = "shr"; break;
		// a &= b
		case BO_AndAssign: op = "and"; break;
		// a |= b
		case BO_OrAssign: op = "or"; break;
		// a ^= b
		case BO_XorAssign: op = "xor"; break;
		default:
			break;
		}

		if( !op.empty() ) {
			// The operator is a compound operator, we substitute the RHS expression with the expanded one
			const core::lang::OperatorPtr& opFunc = builder.literal( opType + "." + op, builder.functionType(tupleTy, exprTy));
			rhs = builder.callExpr(opFunc, { lhs, rhs });
		}

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
			// This is an assignment, we have to make sure the LHS operation is of type ref<a'>
			assert( core::dynamic_pointer_cast<const core::RefType>(lhs->getType()) && "LHS operand must of type ref<a'>." );
			exprTy = lhs->getType();
			opType = "ref";
			op = "assign"; break;

		default:
			assert(false && "Operator not supported");
		}

		const core::lang::OperatorPtr& opFunc = builder.literal( opType + "." + op, builder.functionType(tupleTy, exprTy));

		// build a callExpr with the 2 arguments
		return ExprWrapper( convFact.builder.callExpr(opFunc, { lhs, rhs }) );
	}

	ExprWrapper VisitUnaryOperator(clang::UnaryOperator *unOp) {
		const core::ASTBuilder& builder = convFact.builder;
		core::ExpressionPtr subExpr = Visit(unOp->getSubExpr()).ref;

		bool additive = false;
		bool post = false;
		switch(unOp->getOpcode()) {
		// conversion of post increment/decrement operation is done by creating a tuple expression i.e.:
		// a++ ==> (a=a+1, a-1) // FIXME? Does it need to be ATOMIC?
		// a-- ==> (a=a-1, a+1)
		// a++
		case UO_PostInc:
			additive = true;
		// a--
		case UO_PostDec:
			post = true;

		// ++a
		case UO_PreInc:
			additive = true;
		// --a
		case UO_PreDec:
			assert( core::dynamic_pointer_cast<const core::RefType>(subExpr->getType()) && "LHS operand must of type ref<a'>." );
			return ExprWrapper(
				// build a tuple expression
				builder.tupleExpr(
				std::vector<core::ExpressionPtr>( { 	// ref.assign(a int.add(a, 1))
					builder.callExpr( core::lang::OP_REF_ASSIGN_PTR,
						std::vector<core::ExpressionPtr>({
							subExpr, // ref<a'> a
							builder.callExpr(
								( additive ? core::lang::OP_INT_ADD_PTR:core::lang::OP_INT_SUB_PTR ),
									std::vector<core::ExpressionPtr>({ subExpr, core::lang::CONST_UINT_ONE_PTR })
							) // a - 1
						})
					),
					(post ? // if is post increment/decrement
						builder.callExpr(
							( additive ? core::lang::OP_INT_SUB_PTR:core::lang::OP_INT_ADD_PTR ),
							std::vector<core::ExpressionPtr>({
								builder.callExpr( core::lang::OP_REF_DEREF_PTR, {subExpr} ), // ref.deref(a)
								core::lang::CONST_UINT_ONE_PTR // 1
							})
						)
						: // else
						builder.callExpr( core::lang::OP_REF_DEREF_PTR, {subExpr} )
					)
				}))
			) ;
		// &a
		case UO_AddrOf:
		// *a
		case UO_Deref:

		// +a
		case UO_Plus:
		// -a
		case UO_Minus:
		// ~a
		case UO_Not:
		// !a
		case UO_LNot:

		case UO_Real:
		case UO_Imag:
		case UO_Extension:
		default:
			assert(false && "Unary operator not supported");
		}
		return ExprWrapper( subExpr );
	}

	ExprWrapper VisitArraySubscriptExpr(clang::ArraySubscriptExpr* arraySubExpr) {
		// todo:
		return ExprWrapper( EmptyExpr(convFact.builder) );
	}

	ExprWrapper VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
		const core::ASTBuilder& builder = convFact.builder;
		// check whether this is a reference to a variable
		if(Decl* varDecl = declRef->getDecl()) {
			core::TypePtr varTy = convFact.ConvertType( *declRef->getType().getTypePtr() );
			// FIXME: is this correct?
			if(!core::dynamic_pointer_cast<const core::RefType>(varTy))
				varTy = builder.refType(varTy);

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

		// we cannot analyze if the variable will be modified or not, so we make it of type ref<a'>
		// successive dataflow analysis could be used to restrict the access to this variable
		core::TypePtr type = convFact.builder.refType( convFact.ConvertType( *varDecl->getType().getTypePtr() ) );

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
				initExpr = core::lang::CONST_NULL_PTR_PTR;
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
		VLOG(2) << "@ ForStmt";

		StmtWrapper retStmt;
		StmtWrapper&& body = Visit(forStmt->getBody());
		VLOG(2) << "\t-> ForStmt body: " << body;

		ExprWrapper&& incExpr = convFact.ConvertExpr( *forStmt->getInc() );
		// Determine the induction variable
		// analyze the incExpr looking for the induction variable for this loop
		VLOG(2) << "\t-> ForStmt incExpr: " << *incExpr.ref;

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
//				const core::VarExprPtr& varExpr = declStmt->getVarExpression();
//				core::ExpressionPtr&& initExpr = convFact.ConvertExpr( *expr );
			// todo: build a binary expression
			// condExpr = (varExpr = initExpr);
		} else
			condExpr = convFact.ConvertExpr( *forStmt->getCond() );

		VLOG(2) << "\t-> ForStmt condExpr: " << *condExpr.ref;

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
		VLOG(2) << "\t-> ForStmt initExpr: " << initExpr;

		core::DeclarationStmtPtr declStmt = core::dynamic_pointer_cast<const core::DeclarationStmt>( initExpr.getSingleStmt() );
		assert(declStmt && "Falied loop init expression conversion");
		retStmt.push_back( builder.forStmt(declStmt, body.getSingleStmt(), condExpr.ref, incExpr.ref) );

		return StmtWrapper( tryAggregateStmts(builder, retStmt) );
	}

	StmtWrapper VisitIfStmt(IfStmt* ifStmt) {
		const core::ASTBuilder& builder = convFact.builder;
		StmtWrapper retStmt;

		VLOG(2) << "@ IfStmt";
		core::StatementPtr thenBody = tryAggregateStmts( builder, Visit( ifStmt->getThen() ) );
		assert(thenBody && "Couldn't convert 'then' body of the IfStmt");

		VLOG(2) << "\t-> IfStmt 'then' body: " << *thenBody;
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
		VLOG(2) << "\t-> IfStmt 'condition' expression: " << *condExpr;

		core::StatementPtr elseBody(NULL);
		// check for else statement
		if(Stmt* elseStmt = ifStmt->getElse()) {
			elseBody = tryAggregateStmts( builder, Visit( elseStmt ) );
		} else {
			// create an empty compound statement in the case there is no else stmt
			elseBody = builder.compoundStmt();
		}
		assert(elseBody && "Couldn't convert 'else' body of the IfStmt");
		VLOG(2) << "\t-> IfStmt 'else' body: " << *elseBody;

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

		VLOG(2) << "@ WhileStmt";
		core::StatementPtr body = tryAggregateStmts( builder, Visit( whileStmt->getBody() ) );
		assert(body && "Couldn't convert body of the WhileStmt");

		VLOG(2) << "\t-> WhileStmt body: " << body;
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
			core::DeclarationStmtPtr&& declStmt =
					core::dynamic_pointer_cast<const core::DeclarationStmt>( VisitVarDecl(condVarDecl).getSingleStmt() );
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
		VLOG(2) << "\t-> WhileStmt 'condition' expression: " << condExpr;

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

		VLOG(2) << "@ SwitchStmt";
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
			[ &stmtList, this ] (Stmt* stmt) {
				// A compoundstmt can contain declaration statements.This means that a clang DeclStmt can be converted in multiple
				// StatementPtr because an initialization list such as: int a,b=1; is converted into the following sequence of statements:
				// int<a> a = 0; int<4> b = 1;
				StmtWrapper&& convertedStmt = this->Visit(stmt);
				std::copy(convertedStmt.begin(), convertedStmt.end(), std::back_inserter(stmtList));
			}
		);
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
		std::for_each( stmt->child_begin(), stmt->child_end(), [ this ] (Stmt* stmt) { this->Visit(stmt); });
		return StmtWrapper();
	}
};

#define MAKE_SIZE(n)	toVector(core::IntTypeParam::getConcreteIntParam(n))
#define EMPTY_TYPE_LIST	vector<core::TypePtr>()

class ClangTypeConverter: public TypeVisitor<ClangTypeConverter, TypeWrapper> {
	const ConversionFactory& convFact;

	utils::DependencyGraph<const Type*> typeGraph;

	typedef std::map<const Type*, core::TypeVariablePtr> TypeRecVarMap;
	TypeRecVarMap recVarMap;
	bool isRecSubType;

	typedef std::map<const Type*, core::TypePtr> RecTypeMap;
	RecTypeMap recTypeCache;

public:
	ClangTypeConverter(const ConversionFactory& fact): convFact( fact ), isRecSubType(false) { }

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
		std::for_each(funcTy->arg_type_begin(), funcTy->arg_type_end(),
			[ &argTypes, this ] (const QualType& currArgType) {
				argTypes.push_back( this->Visit( currArgType.getTypePtr() ).ref );
			}
		);

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

	TypeWrapper VisitTypedefType(TypedefType* typedefType) {
		DLOG(INFO) << "Converting typedef: " << typedefType->getDecl()->getName().str();
		typedefType->getDecl()->dump();
		return Visit(typedefType->getDecl()->getUnderlyingType().getTypePtr());
		// assert(false && "TypedefType not yet handled!");
	}

	TypeWrapper VisitTypeOfType(TypeOfType* typeOfType) {
		// assert(false && "TypeOfType not yet handled!");
		DLOG(ERROR) << "TypeOfType not yet handled";
		return TypeWrapper( convFact.builder.getUnitType() );
	}

	TypeWrapper VisitTypeOfExprType(TypeOfExprType* typeOfType) {
		// assert(false && "TypeOfExprType not yet handled!");
		// DLOG(ERROR) << "TypeOfExprType not yet handled";
		return Visit( typeOfType->getUnderlyingExpr()->getType().getTypePtr() );
	}

	TypeWrapper VisitTagType(TagType* tagType) {
		if(!recVarMap.empty()) {
			// check if this type has a typevar already associated, in such case return it
			TypeRecVarMap::const_iterator fit = recVarMap.find(tagType);
			if( fit != recVarMap.end() ) {
				// we are resolving a parent recursive type, so we shouldn't
				return TypeWrapper( fit->second );
			}
		}

		// check if the type is in the cache of already solved recursive types
		// this is done only if we are not resolving a recursive sub type
		if(!isRecSubType) {
			RecTypeMap::const_iterator rit = recTypeCache.find(tagType);
			if(rit != recTypeCache.end())
				return TypeWrapper( rit->second );
		}
		// will store the converted type
		core::TypePtr retTy(NULL);
		DLOG(INFO) << "Converting TagType: " << tagType->getDecl()->getName().str();

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
					// we are dealing with a recursive type
					DLOG(INFO) << "Analyzing RecordDecl: " << recDecl->getNameAsString() << std::endl <<
												  "Number of components in the cycle: " << components.size();
					std::for_each(components.begin(), components.end(),
						[] (std::set<const Type*>::value_type c) {
							assert(isa<const TagType>(c));
							DLOG(INFO) << "\t" << dyn_cast<const TagType>(c)->getDecl()->getNameAsString();
						}
					);

//					typeGraph.print(std::cout);

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
							core::NamedCompositeType::Entry(core::Identifier(curr->getNameAsString()), Visit( fieldType ).ref)
					);
				}

				// build a struct or union IR type
				retTy = handleTagType(tagDecl, structElements);

				if( !components.empty() ) {
					// if we are visiting a nested recursive type it means someone else will take care
					// of building the rectype node, we just return an intermediate type
					if(isRecSubType)
						return TypeWrapper( retTy );

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

							definitions.insert( std::make_pair(var, this->Visit(const_cast<Type*>(ty)).ref) );

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
		return TypeWrapper( retTy );
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

ConversionFactory::ConversionFactory(core::SharedNodeManager mgr): mgr(mgr), builder(mgr),
		typeConv( new ClangTypeConverter(*this) ),
		exprConv( new ClangExprConverter(*this) ),
		stmtConv( new ClangStmtConverter(*this) ) { }

core::TypePtr ConversionFactory::ConvertType(const clang::Type& type) {
	DVLOG(1) << "Start converting type [class: '" << type.getTypeClassName() << "']:";
	if( VLOG_IS_ON(2) ) {
		DVLOG(2) << "Dump of clang type: \n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		type.dump();
	}
	core::TypePtr ty = typeConv->Visit(const_cast<Type*>(&type)).ref;
	DVLOG(1) << "Converted into insieme 'type': ";
	DVLOG(1) << "\t" << *ty;
	return ty;
}

core::StatementPtr ConversionFactory::ConvertStmt(const clang::Stmt& stmt) {
	DVLOG(1) << "Start converting statement [class: '" << stmt.getStmtClassName() << "'] {" <<
			utils::location(stmt.getLocStart(), clangCtx->getSourceManager()) << "}: ";
	if( VLOG_IS_ON(2) ) {
		DVLOG(2) << "Dump of clang statement: \n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		stmt.dump();
	}
	core::StatementPtr s = stmtConv->Visit(const_cast<Stmt*>(&stmt)).getSingleStmt();
	DVLOG(1) << "Converted into insieme 'statement': ";
	DVLOG(1) << "\t" << *s;
	return s;
}

core::ExpressionPtr ConversionFactory::ConvertExpr(const clang::Expr& expr) {
	DVLOG(1) << "Start converting expression [class: '" << expr.getStmtClassName() << "'] {" <<
			utils::location(expr.getLocStart(), clangCtx->getSourceManager()) << "}: ";
	if( VLOG_IS_ON(2) ) {
		DVLOG(2) << "Dump of clang expression: \n~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n";
		expr.dump();
	}
	core::ExpressionPtr e = exprConv->Visit(const_cast<Expr*>(&expr)).ref;
	DVLOG(1) << "Converted into insieme 'expression': ";
	DVLOG(1) << "\t" << *e;
	return e;
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
			DVLOG(1) << "##########################################################################";
			DVLOG(1) << "Encountered function declaration '" << funcDecl->getNameAsString() << "': "
					 << frontend::utils::location( funcDecl->getLocStart(), mCtx->getSourceManager() );

			// finds a definition of this function if any
			const FunctionDecl* definition = NULL;
			// if this function is just a declaration, and it has no definition, we just skip it
			if(!funcDecl->hasBody(definition))
				continue;

			DVLOG(1) << "\t* Converting body";

			core::TypePtr funcType = fact.ConvertType( *definition->getType().getTypePtr() );
			// paramlist
			core::LambdaExpr::ParamList funcParamList;
			std::for_each(definition->param_begin(), definition->param_end(),
				[&funcParamList, &fact] (ParmVarDecl* currParam) {
					funcParamList.push_back(
						fact.getASTBuilder().paramExpr( fact.ConvertType( *currParam->getType().getTypePtr() ), currParam->getNameAsString()) );
				}
			);
			// this is a function decl


            //check Attributes of the function definition
            if(definition->hasAttrs()) {
                const clang::AttrVec attrVec = funcDecl->getAttrs();

                for(Attr *const*I = attrVec.begin(), *const*E = attrVec.end(); I != E; ++I) {
                    Attr* attr = *I;
//                    printf("Attribute \n");
                    if(attr->getKind() == attr::Kind::Annotate) {
//                        printf("    Annotate\n");
                        //get annotate string
                        AnnotateAttr* aa = (AnnotateAttr*)attr;
                        llvm::StringRef sr = aa->getAnnotation();

                        //check if it is an OpenCL kernel function
                        if(sr.compare(llvm::StringRef("__kernel")) == 0) {
//                            printf("        Kernel\n");
                            DVLOG(1) << "is OpenCL kernel function";

                            funcType.addAnnotation(std::make_shared<insieme::c_info::OclKernelFctAnnotation>());
                        }
                    }
                    if(attr->getKind() == attr::Kind::ReqdWorkGroupSize) {
                        funcType.addAnnotation(std::make_shared<insieme::c_info::OclWorkGroupSizeAnnotation>(
                                ((ReqdWorkGroupSizeAttr*)attr)->getXDim(),
                                ((ReqdWorkGroupSizeAttr*)attr)->getYDim(),
                                ((ReqdWorkGroupSizeAttr*)attr)->getZDim()));

                    }
                }
            }

			core::StatementPtr funcBody(NULL);
			assert(definition->getBody() && "Function Definition has no body");

			funcBody = fact.ConvertStmt( *definition->getBody() );
			core::ExpressionPtr lambaExpr = fact.getASTBuilder().lambdaExpr(funcType, funcParamList, funcBody);
			// annotate name of function
			lambaExpr.addAnnotation(std::make_shared<insieme::c_info::CNameAnnotation>(definition->getName()));
			if(definition->isMain()) {
				program = program->addEntryPoint(lambaExpr);
				//assert((*program->getEntryPoints().begin()).contains(insieme::c_info::CNameAnnotation::KEY) && "Key lost!");
			}
		}

//		else if(VarDecl* varDecl = dyn_cast<VarDecl>(decl)) {
//			fact.ConvertType( *varDecl->getType().getTypePtr() );
//		}
	}
}

void IRConsumer::HandleTranslationUnit (ASTContext &Ctx) { }

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
