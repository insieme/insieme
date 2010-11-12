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

#include "insieme/frontend/conversion.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/analysis/expr_analysis.h"
#include "insieme/frontend/omp/omp_pragma.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"

#include "insieme/core/lang_basic.h"
#include "insieme/core/transform/node_replacer.h"

#include "insieme/c_info/naming.h"

#include "clang/AST/StmtVisitor.h"

#include "clang/Index/Entity.h"
#include "clang/Index/Indexer.h"

using namespace clang;
using namespace insieme;
namespace fe = insieme::frontend;

namespace {
// Returns a string of the text within the source range of the input stream
std::string GetStringFromStream(const SourceManager& srcMgr, const SourceLocation& start) {
	// we use the getDecomposedSpellingLoc() method because in case we read macros values
	// we have to read the expanded value
	std::pair<FileID, unsigned> startLocInfo = srcMgr.getDecomposedSpellingLoc(start);
	llvm::StringRef startBuffer = srcMgr.getBufferData(startLocInfo.first);
	const char *strDataStart = startBuffer.begin() + startLocInfo.second;

	return string(strDataStart, clang::Lexer::MeasureTokenLength(srcMgr.getSpellingLoc(start), srcMgr, clang::LangOptions()));
}

// in case the the last argument of the function is a var_arg, we try pack the exceeding arguments with the pack
// operation provided by the IR.
vector<core::ExpressionPtr> tryPack(const core::ASTBuilder& builder, core::FunctionTypePtr funcTy, const ExpressionList& args) {

	// check if the function type ends with a VAR_LIST type
	core::TupleTypePtr&& argTy = core::dynamic_pointer_cast<const core::TupleType>(funcTy->getArgumentType());
	assert(argTy && "Function argument is of not type TupleType");

	const core::TupleType::ElementTypeList& elements = argTy->getElementTypes();
	// if the tuple type is empty it means we cannot pack any of the arguments
	if( elements.empty() )
		return args;

	if(*elements.back() == core::lang::TYPE_VAR_LIST_VAL) {
		ExpressionList ret;
		assert(args.size() >= elements.size()-1 && "Function called with fewer arguments than necessary");
		// last type is a var_list, we have to do the packing of arguments

		// we copy the first N-1 arguments, the remaining will be unpacked
		std::copy(args.begin(), args.begin()+elements.size()-1, std::back_inserter(ret));

		ExpressionList toPack;
		if(args.size() > elements.size()-1) {
			std::copy(args.begin()+elements.size()-1, args.end(), std::back_inserter(toPack));
		}

		// arguments has to be packed into a tuple expression, and then inserted into a pack expression
		ret.push_back(
			builder.callExpr(core::lang::TYPE_VAR_LIST_PTR, core::lang::OP_VAR_LIST_PACK_PTR, toVector<core::ExpressionPtr>(builder.tupleExpr(toPack)))
		);
		return ret;
	}
	return args;
}

// FIXME: this has to be rewritten once lang/core is in a final state
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

//        ss << "vector<" << getOperationType(vt->getElementType()) << ">";
        return ss.str();
    }
    // FIXME
    return "unit";
	// assert(false && "Type not supported");
}

}

namespace insieme {
namespace frontend {
namespace conversion {

#define START_LOG_EXPR_CONVERSION(expr) \
	DVLOG(1) << "\n****************************************************************************************\n" \
			 << "Converting expression [class: '" << expr->getStmtClassName() << "']\n" \
			 << "-> at location: (" << utils::location(expr->getLocStart(), convFact.currTU->getCompiler().getSourceManager()) << "): "; \
	if( VLOG_IS_ON(2) ) { \
		DVLOG(2) << "Dump of clang expression: \n" \
				 << "~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~\n"; \
		expr->dump(); \
	}

#define END_LOG_EXPR_CONVERSION(expr) \
	DVLOG(1) << "Converted into IR expression: "; \
	DVLOG(1) << "\t" << *expr;


// creates a function call from a list of expressions,
// usefull for implementing the semantics of ++ or -- or comma separated expressions in the IR
core::CallExprPtr ConversionFactory::createCallExpr(const StatementList& body, core::TypePtr retTy, bool useCapture) const {

	core::CompoundStmtPtr&& bodyStmt = builder.compoundStmt( body );
	// keeps the list variables used in the body
	insieme::frontend::analysis::VarRefFinder args(bodyStmt);

	core::LambdaExpr::ParamList params;
	core::TupleType::ElementTypeList paramTypes;
	core::LambdaExpr::CaptureList capture;
	std::for_each(args.begin(), args.end(),
		[ &capture, &params, &paramTypes, &builder, &bodyStmt, useCapture] (const core::ExpressionPtr& curr) {
			const core::VariablePtr& bodyVar = core::dynamic_pointer_cast<const core::Variable>(curr);
			core::VariablePtr&& parmVar = builder.variable( bodyVar->getType() );
			if(useCapture)
				capture.push_back( builder.declarationStmt(parmVar, bodyVar) );
			else {
				params.push_back( parmVar );
				paramTypes.push_back( parmVar->getType() );
			}
			// we have to replace the variable of the body with the newly created parmVar
			bodyStmt = core::dynamic_pointer_cast<const core::CompoundStmt>( core::transform::replaceNode(builder, bodyStmt, bodyVar, parmVar, true) );
			assert(bodyStmt);
		}
	);

	// build the type of the function
	core::FunctionTypePtr&& funcTy = builder.functionType( builder.tupleType( useCapture ?  core::TupleType::ElementTypeList() : paramTypes ), retTy);

	// build the expression body
	core::LambdaExprPtr retExpr;
	if(useCapture)
		retExpr = builder.lambdaExpr( funcTy, capture, core::LambdaExpr::ParamList(), bodyStmt );
	else
		retExpr = builder.lambdaExpr( funcTy, params, bodyStmt );

	if(useCapture)
		return builder.callExpr( retTy, retExpr, ExpressionList() );
	return builder.callExpr( retTy, retExpr, ExpressionList(args.begin(), args.end()) );
}

//#############################################################################
//
//							CLANG EXPRESSION CONVERTER
//
//############################################################################
class ConversionFactory::ClangExprConverter: public StmtVisitor<ClangExprConverter, core::ExpressionPtr> {
	ConversionFactory& convFact;
	ConversionContext& ctx;
public:
	ClangExprConverter(ConversionFactory& convFact): convFact(convFact), ctx(convFact.ctx) { }

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//								INTEGER LITERAL
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitIntegerLiteral(clang::IntegerLiteral* intLit) {
		START_LOG_EXPR_CONVERSION(intLit);
		core::ExpressionPtr&& retExpr =
			convFact.builder.literal(
				// retrieve the string representation from the source code
				GetStringFromStream( convFact.currTU->getCompiler().getSourceManager(), intLit->getExprLoc()),
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
				GetStringFromStream( convFact.currTU->getCompiler().getSourceManager(), stringLit->getExprLoc()),
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
				GetStringFromStream(convFact.currTU->getCompiler().getSourceManager(), boolLit->getExprLoc()), core::lang::TYPE_BOOL_PTR
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
	//						   IMPLICIT CAST EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitImplicitCastExpr(clang::ImplicitCastExpr* implCastExpr) {
		START_LOG_EXPR_CONVERSION(implCastExpr);
		const core::TypePtr& type = convFact.convertType( GET_TYPE_PTR(implCastExpr) );
		core::ExpressionPtr&& subExpr = Visit(implCastExpr->getSubExpr());
		core::ExpressionPtr&& nonRefExpr = convFact.tryDeref(subExpr);

		// if the subexpression is an array or a vector, remove all the C implicit casts
		if( dynamic_pointer_cast<const core::ArrayType>(nonRefExpr->getType()) ||
			dynamic_pointer_cast<const core::VectorType>(nonRefExpr->getType()) )
			return subExpr;

		// In the case the target type of the cast is not a reftype we deref the subexpression
		if(!core::dynamic_pointer_cast<const core::RefType>(type)) {
			subExpr = convFact.tryDeref(subExpr);
		}
		core::ExpressionPtr&& retExpr = convFact.builder.castExpr( type, subExpr );
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
		// In the case the target type of the cast is not a reftype we deref the subexpression
		if(!core::dynamic_pointer_cast<const core::RefType>(type)) {
			subExpr = convFact.tryDeref(subExpr);
		}
		core::ExpressionPtr&& retExpr = convFact.builder.castExpr( type, subExpr );
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							FUNCTION CALL EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitCallExpr(clang::CallExpr* callExpr) {
		START_LOG_EXPR_CONVERSION(callExpr);
		if( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(callExpr->getDirectCallee()) ) {
			const core::ASTBuilder& builder = convFact.builder;

			// collects the type of each argument of the expression
			ExpressionList args;
			std::for_each(callExpr->arg_begin(), callExpr->arg_end(),
				[ &args, &builder, this ] (Expr* currArg) {
					args.push_back( this->convFact.tryDeref(this->Visit(currArg)) );
				}
			);

			core::FunctionTypePtr&& funcTy = core::dynamic_pointer_cast<const core::FunctionType>( convFact.convertType( GET_TYPE_PTR(funcDecl) ) );
			ExpressionList&& packedArgs = tryPack(convFact.builder, funcTy, args);

			const TranslationUnit* oldTU = convFact.currTU;

			const FunctionDecl* definition = NULL;
			// this will find function definitions if they are declared in  the same translation unit (also defined as static)
			if( !funcDecl->hasBody(definition) ) {
				// if the function is not defined in this translation unit, maybe it is defined in another we already loaded
				// use the clang indexer to lookup the definition for this function declarations
				clang::idx::Entity&& funcEntity = clang::idx::Entity::get(funcDecl, const_cast<clang::idx::Program&>(convFact.program.getClangProgram()));
				std::pair<FunctionDecl*, clang::idx::TranslationUnit*>&& ret = convFact.program.getClangIndexer().getDefinitionFor(funcEntity);
				if(ret.first) {
					definition = ret.first;
					assert(ret.second);
					convFact.currTU = &Program::getTranslationUnit(ret.second);
				}
			}
			if(!definition) {
				// No definition has been found in any of the translation units, we mark this function as extern!
				core::ExpressionPtr irNode =
						convFact.builder.callExpr(	funcTy->getReturnType(), builder.literal(funcDecl->getNameAsString(), funcTy), packedArgs );
				// handle eventual pragmas attached to the Clang node
				frontend::omp::attachOmpAnnotation(irNode, callExpr, convFact);
				return irNode;
			}

			// If we are resolving the body of a recursive function we have to return the associated
			// variable every time a function in the strongly connected graph of function calls
			// is encountred.
			if(ctx.isResolvingRecFuncBody) {
				// check if this type has a typevar already associated, in such case return it
				ConversionContext::RecVarExprMap::const_iterator fit = ctx.recVarExprMap.find(definition);
				if( fit != ctx.recVarExprMap.end() ) {
					// we are resolving a parent recursive type, so we shouldn't
					convFact.currTU = oldTU;
					return builder.callExpr(funcTy->getReturnType(), fit->second, packedArgs);
				}
			}

			if(!ctx.isResolvingRecFuncBody) {
				ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(definition);
				if(fit != ctx.lambdaExprCache.end()) {
					convFact.currTU = oldTU;
					core::ExpressionPtr&& irNode = builder.callExpr(funcTy->getReturnType(), fit->second, packedArgs);
					// handle eventual pragmas attached to the Clang node
					frontend::omp::attachOmpAnnotation(irNode, callExpr, convFact);
					return irNode;
				}
			}

			assert(definition && "No definition found for function");
			core::ExpressionPtr&& lambdaExpr = convFact.convertFunctionDecl(definition);
			convFact.currTU = oldTU;

			core::ExpressionPtr irNode = builder.callExpr(funcTy->getReturnType(), lambdaExpr, packedArgs);
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
	//
	// [C99 6.5.2.3] Structure and Union Members. X->F and X.F.
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitMemberExpr(clang::MemberExpr* membExpr)  {
		START_LOG_EXPR_CONVERSION(membExpr);
		const core::ASTBuilder& builder = convFact.builder;

		core::ExpressionPtr&& base = convFact.tryDeref(Visit(membExpr->getBase()));
		if(membExpr->isArrow()) {
			// we have to check whether we currently have a ref or probably an array (which is used to represent C pointers)
			base = convFact.tryDeref(Visit(membExpr->getBase()));
			DLOG(INFO) << *base->getType();
			if(core::dynamic_pointer_cast<const core::VectorType>(base->getType()) ||
				core::dynamic_pointer_cast<const core::ArrayType>(base->getType())) {

				core::SingleElementTypePtr&& subTy = core::dynamic_pointer_cast<const core::SingleElementType>(base->getType());
				assert(subTy);
				base = builder.callExpr( subTy->getElementType(), core::lang::OP_SUBSCRIPT_SINGLE_PTR,
						toVector<core::ExpressionPtr>(base, builder.literal("0", core::lang::TYPE_INT_4_PTR)) );
				base = convFact.tryDeref(base);
			}
			DLOG(INFO) << *base->getType();
		}
		core::Identifier&& ident = membExpr->getMemberDecl()->getNameAsString();

		core::ExpressionPtr&& retExpr = builder.memberAccessExpr(base, ident);
		// handle eventual pragmas attached to the Clang node
		frontend::omp::attachOmpAnnotation(retExpr, membExpr, convFact);

		return retExpr;
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
			return convFact.createCallExpr(toVector<core::StatementPtr>(lhs, builder.returnStmt(rhs)), rhs->getType());
		}

		core::TypePtr&& exprTy = convFact.convertType( GET_TYPE_PTR(binOp) );

		// create Pair type
		core::TupleTypePtr&& tupleTy = builder.tupleType(toVector( exprTy, exprTy ) );
		std::string&& opType = getOperationType(exprTy);

		// we take care of compound operators first,
		// we rewrite the RHS expression in a normal form, i.e.:
		// a op= b  ---->  a = a op b
		std::string op;
		clang::BinaryOperatorKind baseOp;
		switch( binOp->getOpcode() ) {
		// a *= b
		case BO_MulAssign: 	op = "mul"; baseOp = BO_Mul; break;
		// a /= b
		case BO_DivAssign: 	op = "div"; baseOp = BO_Div; break;
		// a %= b
		case BO_RemAssign:	op = "mod"; baseOp = BO_Rem; break;
		// a += b
		case BO_AddAssign: 	op = "add"; baseOp = BO_Add; break;
		// a -= b
		case BO_SubAssign:	op = "sub"; baseOp = BO_Sub; break;
		// a <<= b
		case BO_ShlAssign: 	op = "shl"; baseOp = BO_Shl; break;
		// a >>= b
		case BO_ShrAssign: 	op = "shr"; baseOp = BO_Shr; break;
		// a &= b
		case BO_AndAssign: 	op = "and"; baseOp = BO_And; break;
		// a |= b
		case BO_OrAssign: 	op = "or"; 	baseOp = BO_Or; break;
		// a ^= b
		case BO_XorAssign: 	op = "xor"; baseOp = BO_Xor; break;
		default:
			break;
		}

		if( !op.empty() ) {
			// The operator is a compound operator, we substitute the RHS expression with the expanded one
			core::RefTypePtr&& lhsTy = core::dynamic_pointer_cast<const core::RefType>(lhs->getType());
			assert( lhsTy && "LHS operand must of type ref<a'>." );
			core::lang::OperatorPtr&& opFunc = builder.literal( opType + "." + op, builder.functionType(tupleTy, exprTy));

			// we check if the RHS is a ref, in that case we use the deref operator
			rhs = convFact.tryDeref(rhs);

			const core::TypePtr& lhsSubTy = lhsTy->getElementType();
			rhs = builder.callExpr(lhsSubTy, opFunc,
				toVector<core::ExpressionPtr>(builder.callExpr( lhsSubTy, core::lang::OP_REF_DEREF_PTR, toVector(lhs) ), rhs) );

			// add an annotation to the subexpression
			opFunc->addAnnotation( std::make_shared<c_info::COpAnnotation>( BinaryOperator::getOpcodeStr(baseOp)) );
		}

		bool isAssignment = false;
		bool isLogical = false;
		baseOp = binOp->getOpcode();

		core::lang::OperatorPtr opFunc;

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
		case BO_LAnd: 	op = "land"; isLogical=true; break;
		// a || b
		case BO_LOr:  	op = "lor";  isLogical=true; break;
		// a < b
		case BO_LT:	 	op = "lt";   isLogical=true; break;
		// a > b
		case BO_GT:  	op = "gt";   isLogical=true; break;
		// a <= b
		case BO_LE:  	op = "le";   isLogical=true; break;
		// a >= b
		case BO_GE:  	op = "ge";   isLogical=true; break;
		// a == b
		case BO_EQ:  	op = "eq";   isLogical=true; break;
		// a != b
		case BO_NE:	 	op = "ne";   isLogical=true; break;

		case BO_MulAssign: case BO_DivAssign: case BO_RemAssign: case BO_AddAssign: case BO_SubAssign:
		case BO_ShlAssign: case BO_ShrAssign: case BO_AndAssign: case BO_XorAssign: case BO_OrAssign:
		case BO_Assign:
			baseOp = BO_Assign;
			// This is an assignment, we have to make sure the LHS operation is of type ref<a'>
			assert( core::dynamic_pointer_cast<const core::RefType>(lhs->getType()) && "LHS operand must of type ref<a'>." );
			isAssignment = true;
			opFunc = convFact.mgr->get(core::lang::OP_REF_ASSIGN_PTR);
			exprTy = core::lang::TYPE_UNIT_PTR;
			break;
		default:
			assert(false && "Operator not supported");
		}

		// build a callExpr with the 2 arguments
		rhs = convFact.tryDeref(rhs);

		if( !isAssignment )
			lhs = convFact.tryDeref(lhs);

		// check the types
//		const core::TupleTypePtr& opTy = core::dynamic_pointer_cast<const core::FunctionType>(opFunc->getType())->getArgumentType();
//		if(lhs->getType() != opTy->getElementTypes()[0]) {
//			// add a castepxr
//			lhs = convFact.builder.castExpr(opTy->getElementTypes()[0], lhs);
//		}
//		if(rhs->getType() != opTy->getElementTypes()[1]) {
//			// add a castepxr
//			rhs = convFact.builder.castExpr(opTy->getElementTypes()[1], rhs);
//		}

		if(isLogical) {
			exprTy = core::lang::TYPE_BOOL_PTR;
			tupleTy = builder.tupleType(toVector(lhs->getType(), rhs->getType())); // FIXME
		}

		if(!isAssignment)
			opFunc = builder.literal( opType + "." + op, builder.functionType(tupleTy, exprTy));

		core::ExpressionPtr&& retExpr = convFact.builder.callExpr( exprTy, opFunc, toVector(lhs, rhs) );

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

		// build lambda expression for post/pre increment/decrement unary operators
		auto encloseIncrementOperator = [ this, &builder ](core::ExpressionPtr subExpr, bool post, bool additive) {
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
									builder.castExpr( subTy, builder.literal("1", core::lang::TYPE_INT_4_PTR))
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
			return this->convFact.createCallExpr(std::vector<core::StatementPtr>(stmts), subTy);
		};

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
			subExpr = encloseIncrementOperator(subExpr, post, false);
			break;
		// a++
		case UO_PreInc:
			post = false;
		// ++a
		case UO_PostInc:
			subExpr = encloseIncrementOperator(subExpr, post, true);
			break;
		// &a
		case UO_AddrOf:
			// assert(false && "Conversion of AddressOf operator '&' not supported");
			break;
		// *a
		case UO_Deref: {
			subExpr = convFact.tryDeref(subExpr);

			if(core::dynamic_pointer_cast<const core::VectorType>(subExpr->getType()) ||
				core::dynamic_pointer_cast<const core::ArrayType>(subExpr->getType())) {

				core::SingleElementTypePtr&& subTy = core::dynamic_pointer_cast<const core::SingleElementType>(subExpr->getType());
				assert(subTy);
				subExpr = builder.callExpr( subTy->getElementType(), core::lang::OP_SUBSCRIPT_SINGLE_PTR,
						toVector<core::ExpressionPtr>(subExpr, builder.literal("0", core::lang::TYPE_INT_4_PTR)) );
			}
			break;
		}
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
	//							CONDITIONAL OPERATOR
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitConditionalOperator(clang::ConditionalOperator* condOp) {
		START_LOG_EXPR_CONVERSION(condOp);

		assert(condOp->getSaveExpr() == NULL && "Conditional operation with 'gcc save' expession not supperted.");
		core::TypePtr&& retTy = convFact.convertType( GET_TYPE_PTR(condOp) );

		core::ExpressionPtr&& trueExpr = Visit(condOp->getTrueExpr());
		core::ExpressionPtr&& falseExpr = Visit(condOp->getFalseExpr());
		core::ExpressionPtr&& condExpr = Visit( condOp->getCond() );

		// add ref.deref if needed
		condExpr = convFact.tryDeref(condExpr);

		if(*condExpr->getType() != core::lang::TYPE_BOOL_VAL) {
			// the return type of the condition is not a boolean, we add a cast expression
			condExpr = convFact.builder.castExpr(core::lang::TYPE_BOOL_PTR, condExpr);
		}

		// builder.callExpr(retTy, core::lang::OP_ITE_PTR, )
		core::StatementPtr&& ifStmt = convFact.builder.ifStmt(condExpr, trueExpr, falseExpr);
		core::ExpressionPtr&& retExpr = convFact.createCallExpr( toVector( ifStmt ),  retTy);
		END_LOG_EXPR_CONVERSION(retExpr);
		return retExpr;
	}

	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//						ARRAY SUBSCRIPT EXPRESSION
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitArraySubscriptExpr(clang::ArraySubscriptExpr* arraySubExpr) {
		START_LOG_EXPR_CONVERSION(arraySubExpr);

		// CLANG introduces implicit cast for the base expression of array subscripts
		// which cast the array type into a simple pointer. As insieme supports subscripts
		// only for array or vector types, we skip eventual implicit cast operations
		Expr* baseExpr = arraySubExpr->getBase();
		while(ImplicitCastExpr *castExpr = dyn_cast<ImplicitCastExpr>(baseExpr))
			baseExpr = castExpr->getSubExpr();

		// IDX
		core::ExpressionPtr&& idx = convFact.tryDeref(Visit( arraySubExpr->getIdx() ));

		// BASE
		core::ExpressionPtr&& base = convFact.tryDeref(Visit( baseExpr ) );

		// TODO: we need better checking for vector type
		assert( (core::dynamic_pointer_cast<const core::VectorType>( base->getType() ) ||
				core::dynamic_pointer_cast<const core::ArrayType>( base->getType() )) &&
				"Base expression of array subscript is not a vector/array type.");

		// We are sure the type of base is either a vector or an array
		const core::TypePtr& subTy = core::dynamic_pointer_cast<const core::SingleElementType>(base->getType())->getElementType();

		core::ExpressionPtr&& retExpr =
			convFact.builder.callExpr( subTy, core::lang::OP_SUBSCRIPT_SINGLE_PTR, toVector<core::ExpressionPtr>(base, convFact.builder.castExpr(core::lang::TYPE_UINT_4_PTR, idx)) );

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

        core::TypePtr&& exprTy = convFact.convertType( GET_TYPE_PTR(vecElemExpr) );
        core::ExpressionPtr&& idx = convFact.builder.literal(pos, exprTy); // FIXME! are you sure the type is exprTy? and not ref<rexprTy>?
        // if the type of the vector is a refType, we deref it
        base = convFact.tryDeref(base);

        core::ExpressionPtr&& retExpr = convFact.builder.callExpr(convFact.builder.refType(exprTy), core::lang::OP_SUBSCRIPT_SINGLE_PTR, toVector( base, idx ));
        END_LOG_EXPR_CONVERSION(retExpr);
        return retExpr;
    }

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	//							VAR DECLARATION REFERENCE
	//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
		START_LOG_EXPR_CONVERSION(declRef);
		// check whether this is a reference to a variable
		if(VarDecl* varDecl = dyn_cast<VarDecl>(declRef->getDecl())) {
			return convFact.lookUpVariable(varDecl);
		}
		// todo: C++ check whether this is a reference to a class field, or method (function).
		assert(false && "DeclRefExpr not supported!");
	}

    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    //                  VECTOR/STRUCT INITALIZATION EXPRESSION
    //~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	core::ExpressionPtr VisitInitListExpr(clang::InitListExpr* initList) {
		assert(false && "Visiting of initializer list is not allowed!");
    }
};

ConversionFactory::ClangExprConverter* ConversionFactory::makeExprConverter(ConversionFactory& fact) {
	return new ClangExprConverter(fact);
}

core::ExpressionPtr ConversionFactory::convertExpr(const clang::Expr* expr) const {
	assert(expr && "Calling convertExpr with a NULL pointer");
	return exprConv->Visit( const_cast<Expr*>(expr) );
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace

