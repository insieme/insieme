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

#include "insieme/frontend/expr_converter.h"

#include "insieme/annotations/ocl/ocl_annotations.h"
#include "insieme/annotations/c/location.h"

#include "insieme/frontend/utils/source_locations.h"
#include "insieme/frontend/utils/dep_graph.h"
#include "insieme/frontend/utils/clang_utils.h"
#include "insieme/frontend/utils/ir_cast.h"
#include "insieme/frontend/utils/indexer.h"
#include "insieme/frontend/analysis/expr_analysis.h"
#include "insieme/frontend/omp/omp_pragma.h"
#include "insieme/frontend/ocl/ocl_compiler.h"

#include "insieme/frontend/pragma/insieme.h"

#include "insieme/utils/container_utils.h"
#include "insieme/utils/logging.h"
#include "insieme/utils/numeric_cast.h"
#include "insieme/utils/functional_utils.h"

#include "insieme/core/lang/basic.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/analysis/ir_utils.h"
#include "insieme/core/arithmetic/arithmetic_utils.h"
#include "insieme/core/datapath/datapath.h"

#include "insieme/frontend/cpp/temporary_handler.h"
#include "insieme/annotations/c/naming.h"

#include "clang/AST/StmtVisitor.h"

// clang [3.0]
// #include "clang/Index/Entity.h"
// #include "clang/Index/Indexer.h"

#include "clang/Basic/FileManager.h"

using namespace clang;
using namespace insieme;
using namespace exprutils;

namespace std {

std::ostream& operator<<(std::ostream& out, const clang::FunctionDecl* funcDecl) {
	return out << funcDecl->getNameAsString() << "(" << funcDecl->param_size() << ")";
}

} // end std namespace

namespace exprutils {
// FIXME
// Covert clang source location into a annotations::c::SourceLocation object to be inserted in an CLocAnnotation
annotations::c::SourceLocation convertClangSrcLoc(clang::SourceManager& sm, const clang::SourceLocation& loc) {

	clang::SourceLocation cloc = loc;

	if (sm.isMacroArgExpansion(cloc)) {
		cloc = sm.getExpansionLoc(cloc);
	}

	clang::FileID&& fileId = sm.getFileID( sm.getSpellingLoc(cloc) );
	const clang::FileEntry* fileEntry = sm.getFileEntryForID(fileId);
	assert(fileEntry && "File cannot be NULL");

	return annotations::c::SourceLocation(fileEntry->getName(), sm.getExpansionLineNumber(cloc),
			sm.getExpansionColumnNumber(cloc));
}

// Returns a string of the text within the source range of the input stream
std::string GetStringFromStream(const SourceManager& srcMgr, const SourceLocation& start) {
	/*
	 *  we use the getDecomposedSpellingLoc() method because in case we read macros values we have
	 *  to read the expanded value
	 */
	std::pair<FileID, unsigned>&& startLocInfo = srcMgr.getDecomposedSpellingLoc(start);
	llvm::StringRef&& startBuffer = srcMgr.getBufferData(startLocInfo.first);
	const char *strDataStart = startBuffer.begin() + startLocInfo.second;

	return string(strDataStart,
			clang::Lexer::MeasureTokenLength(srcMgr.getSpellingLoc(start), srcMgr, clang::LangOptions())
	);
}

/*
 * In case the the last argument of the function is a var_arg, we try pack the exceeding arguments
 * with the pack operation provided by the IR.
 */
vector<core::ExpressionPtr> tryPack(const core::IRBuilder& builder, core::FunctionTypePtr funcTy,
		const ExpressionList& args) {

	// check if the function type ends with a VAR_LIST type
	const core::TypeList& argsTy = funcTy->getParameterTypes()->getElements();
	// assert(argsTy && "Function argument is of not type TupleType");

	// if the tuple type is empty it means we cannot pack any of the arguments
	if (argsTy.empty()) {
		return args;
	}

	const core::lang::BasicGenerator& gen = builder.getLangBasic();
	if (gen.isVarList(argsTy.back())) {
		ExpressionList ret;
		assert(args.size() >= argsTy.size()-1 && "Function called with fewer arguments than necessary");
		// last type is a var_list, we have to do the packing of arguments

		// we copy the first N-1 arguments, the remaining will be unpacked
		std::copy(args.begin(), args.begin() + argsTy.size() - 1, std::back_inserter(ret));

		ExpressionList toPack;
		if (args.size() > argsTy.size() - 1) {
			std::copy(args.begin() + argsTy.size() - 1, args.end(), std::back_inserter(toPack));
		}

		// arguments has to be packed into a tuple expression, and then inserted into a pack expression
		ret.push_back(builder.callExpr(gen.getVarList(), gen.getVarlistPack(), builder.tupleExpr(toPack)));
		return ret;
	}
	return args;
}

core::CallExprPtr getSizeOfType(const core::IRBuilder& builder, const core::TypePtr& type) {
	core::LiteralPtr size;

	const core::lang::BasicGenerator& gen = builder.getLangBasic();
	if ( core::VectorTypePtr&& vecTy = core::dynamic_pointer_cast<const core::VectorType>(type)) {
		return builder.callExpr(gen.getUnsignedIntMul(), builder.literal(gen.getUInt8(), toString(*(vecTy->getSize()))),
				getSizeOfType(builder, vecTy->getElementType()));
	}
	// in case of ref<'a>, recurr on 'a
	if ( core::RefTypePtr&& refTy = core::dynamic_pointer_cast<const core::RefType>(type)) {
		return getSizeOfType(builder, refTy->getElementType());
	}

	return builder.callExpr(gen.getSizeof(), builder.getTypeLiteral(type));
}

/**
 * Special method which handle malloc and calloc which need to be treated in a special way in the IR.
 */
core::ExpressionPtr handleMemAlloc(const core::IRBuilder& builder, const core::TypePtr& type,
		const core::ExpressionPtr& subExpr) {

	if ( core::CallExprPtr&& callExpr = core::dynamic_pointer_cast<const core::CallExpr>(subExpr)) {

		if ( core::LiteralPtr&& lit = core::dynamic_pointer_cast<const core::Literal>(callExpr->getFunctionExpr())) {
	
			if (!(lit->getStringValue() == "malloc" || lit->getStringValue() == "calloc")) {
				return core::ExpressionPtr();
			}

			assert(((lit->getStringValue() == "malloc" && callExpr->getArguments().size() == 1) || 
				    (lit->getStringValue() == "calloc" && callExpr->getArguments().size() == 2)) && 
					"malloc() and calloc() takes respectively 1 and 2 arguments");

			const core::lang::BasicGenerator& gen = builder.getLangBasic();
			// The type of the cast should be ref<array<'a>>, and the sizeof('a) need to be derived
			assert(type->getNodeType() == core::NT_RefType);
			assert(core::analysis::getReferencedType(type)->getNodeType() == core::NT_ArrayType);

			const core::RefTypePtr& refType = core::static_pointer_cast<const core::RefType>(type);
			const core::ArrayTypePtr& arrayType = refType->getElementType().as<core::ArrayTypePtr>();
			const core::TypePtr& elemType = arrayType->getElementType();

			/*
			 * The number of elements to be allocated of type 'targetType' is:
			 * 		-> 	expr / sizeof(targetType)
			 */
			core::CallExprPtr size;
			if (lit->getStringValue() == "malloc") {
				size = builder.callExpr(
					gen.getUInt8(),
					gen.getUnsignedIntDiv(),
					callExpr->getArgument(0),
					getSizeOfType(builder, elemType)
				);
			} else {
				size = builder.callExpr(
					gen.getUInt8(),
					gen.getUnsignedIntDiv(),
					builder.mul(callExpr->getArgument(0), callExpr->getArgument(1)),
					getSizeOfType(builder, elemType)
				);

			}

			auto memAlloc = builder.refNew(
					builder.callExpr(arrayType, gen.getArrayCreate1D(), builder.getTypeLiteral(elemType), size)
				);

			if (lit->getStringValue() == "malloc") { return memAlloc; }
			// this is a calloc, then we have to do a memset to initialize the memory 
			
			auto var = builder.variable(builder.refType(arrayType));
			auto declStmt = builder.declarationStmt(var, memAlloc);

			auto memSet = builder.callExpr(
					builder.literal(builder.parseType("(anyRef, int<4>, uint<8>) -> anyRef"), "memset"),
					builder.callExpr(gen.getRefToAnyRef(), var), 
					builder.intLit(0), 
					size);

			return builder.createCallExprFromBody(
					builder.compoundStmt( declStmt, memSet, builder.returnStmt(var) ),
					var->getType()
				);
		}
	}
	return core::ExpressionPtr();
}

core::ExpressionPtr getCArrayElemRef(const core::IRBuilder& builder, const core::ExpressionPtr& expr) {
	const core::TypePtr& exprTy = expr->getType();
	if (exprTy->getNodeType() == core::NT_RefType) {
		const core::TypePtr& subTy = GET_REF_ELEM_TYPE(exprTy);

		if (subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType) {
			core::TypePtr elemTy = core::static_pointer_cast<const core::SingleElementType>(subTy)->getElementType();

			return builder.callExpr(
					builder.refType(elemTy),
					(subTy->getNodeType() == core::NT_VectorType ?
							builder.getLangBasic().getVectorRefElem() : builder.getLangBasic().getArrayRefElem1D()),
					expr, builder.uintLit(0));
		}
	}
	return expr;
}

core::ExpressionPtr scalarToVector(core::ExpressionPtr scalarExpr, core::TypePtr refVecTy,
		const core::IRBuilder& builder, const frontend::conversion::ConversionFactory& convFact) {
	const core::lang::BasicGenerator& gen = builder.getNodeManager().getLangBasic();
	const core::VectorTypePtr vecTy = convFact.tryDeref(refVecTy).as<core::VectorTypePtr>();

	core::CastExprPtr cast = core::dynamic_pointer_cast<const core::CastExpr>(scalarExpr);
	core::ExpressionPtr secondArg = cast ? cast->getSubExpression() : scalarExpr; // remove wrong casts added by clang
	if (*secondArg->getType() != *vecTy->getElementType()) // add correct cast (if needed)
		secondArg = builder.castExpr(vecTy->getElementType(), secondArg);

	return builder.callExpr(gen.getVectorInitUniform(), secondArg, builder.getIntTypeParamLiteral(vecTy->getSize()));
}

} // end anonymous namespace

namespace insieme {
namespace frontend {

namespace utils {

template<>
void DependencyGraph<const clang::FunctionDecl*>::Handle(const clang::FunctionDecl* func,
		const DependencyGraph<const clang::FunctionDecl*>::VertexTy& v) {
	// This is potentially dangerous
	FunctionDependencyGraph& funcDepGraph = static_cast<FunctionDependencyGraph&>(*this);

	CallExprVisitor callExprVis(funcDepGraph.getIndexer());
	CallExprVisitor::CallGraph&& graph = callExprVis.getCallGraph(func);

	std::for_each(graph.begin(), graph.end(),
			[ this, v ](const clang::FunctionDecl* currFunc) {assert(currFunc); this->addNode(currFunc, &v);});
}

/*************************************************************************************************
 * CallExprVisitor 
 *************************************************************************************************/
void CallExprVisitor::addFunctionDecl(clang::FunctionDecl* funcDecl) {
	/*
	 * FIXME: clang [3.0]
	const clang::FunctionDecl* def = NULL;
	// this will find function definitions if they are declared in  the same translation unit
	// (also defined as static)
	if (!funcDecl->hasBody(def)) {
		// if the function is not defined in this translation unit, maybe it is defined in another we already
		// loaded use the clang indexer to lookup the definition for this function declarations
		clang::idx::Entity&& funcEntity = clang::idx::Entity::get( funcDecl, indexer.getProgram() );
		conversion::ConversionFactory::TranslationUnitPair&& ret = indexer.getDefinitionFor(funcEntity);
		if ( ret.first ) {def = ret.first;}
	}
	*/

	const clang::FunctionDecl* def = NULL;
	// if the function has no body, we need to find the right declaration with
	// the definition in another translation unit
	if (!funcDecl->hasBody(def)) {
		clang::Decl* raw = indexer.getDefinitionFor (funcDecl);
		if (raw){
			def = llvm::cast<clang::FunctionDecl>(raw);
		}
	}

	if (def){
		callGraph.insert(def);
	}
}

void CallExprVisitor::VisitCallExpr(clang::CallExpr* callExpr) {
	if (callExpr->getDirectCallee()) {
		if (clang::FunctionDecl * funcDecl = llvm::dyn_cast<clang::FunctionDecl>(callExpr->getDirectCallee())) {
			addFunctionDecl(funcDecl);
		}
	}
	VisitStmt(callExpr);
}

void CallExprVisitor::VisitDeclRefExpr(clang::DeclRefExpr* expr) {
	// if this variable is used to invoke a function (therefore is a
	// function pointer) and it has been defined here, we add a potentially
	// dependency to the current definition
	//if ( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(expr->getDecl()) ) {
	// addFunctionDecl(funcDecl);
	//}
}

} // end utils namespace 

namespace conversion {

//---------------------------------------------------------------------------------------------------------------------
//										BASE EXPRESSION CONVERTER
//---------------------------------------------------------------------------------------------------------------------

core::ExpressionPtr ConversionFactory::ExprConverter::wrapVariable(clang::Expr* expr) {
	const DeclRefExpr* ref = utils::skipSugar<const DeclRefExpr>(expr);
	if (ref && isa<const ParmVarDecl>(ref->getDecl())) {
		const core::VariablePtr& parmVar = core::static_pointer_cast<const core::Variable>(
				convFact.convertExpr(ref));

		auto fit = ctx.wrapRefMap.find(parmVar);
		if (fit == ctx.wrapRefMap.end()) {
			fit = ctx.wrapRefMap.insert(
					std::make_pair(parmVar,
							builder.variable(builder.refType(parmVar->getType())))).first;
		}
		return fit->second;
	}
	return convFact.convertExpr(expr);
}

core::ExpressionPtr ConversionFactory::ExprConverter::asLValue(const core::ExpressionPtr& value) {

	// this only works for call-expressions
	if (value->getNodeType() != core::NT_CallExpr || value->getType()->getNodeType() == core::NT_RefType) {
		return value;
	}

	// extract the call
	const core::CallExprPtr& call = static_pointer_cast<const core::CallExpr>(value);

	// check final state - deref has been encountered => drop
	if (core::analysis::isCallOf(call, gen.getRefDeref())) {
		return call->getArgument(0);
	}

	// check whether it is a array-subscript instruction and argument has been de-refernced
	if (core::analysis::isCallOf(value, gen.getArraySubscript1D())) {
		const core::ExpressionPtr arg = call->getArgument(0);
		const core::ExpressionPtr inner = asLValue(arg);
		if (*inner != *arg) {
			return builder.callExpr(builder.refType(value->getType()), gen.getArrayRefElem1D(), inner,
					call->getArgument(1));
		}
	}

	// check whether it is a vector-subscript instruction and argument has been de-refernced
	if (core::analysis::isCallOf(value, gen.getVectorSubscript())) {
		const core::ExpressionPtr arg = call->getArgument(0);
		const core::ExpressionPtr inner = asLValue(arg);
		if (*inner != *arg) {
			return builder.callExpr(builder.refType(value->getType()), gen.getVectorRefElem(), inner,
					call->getArgument(1));
		}
	}

	// check whether it is a struct element access
	if (core::analysis::isCallOf(value, gen.getCompositeMemberAccess())) {
		const core::ExpressionPtr arg = call->getArgument(0);
		const core::ExpressionPtr inner = asLValue(arg);
		if (*inner != *arg) {
			return builder.callExpr(builder.refType(value->getType()), gen.getCompositeRefElem(), inner,
					call->getArgument(1), call->getArgument(2));
		}
	}

	// there is nothing to do
	return value;
}

core::ExpressionPtr ConversionFactory::ExprConverter::asRValue(const core::ExpressionPtr& value) {

	// check whether value is parameter to the current function
	if (value->getNodeType() == core::NT_Variable) {
		auto var = value.as<core::VariablePtr>();
		if (ctx.curParameter && contains(*ctx.curParameter, var)) {
			// => parameters are always r-values
			return var;
		}
	}

	auto type = value->getType();
	// adds a deref to expression in case expression is of a ref type, only if the target tpye is
	// not a vector or an array 
	if (core::analysis::isRefType(value->getType()) && 
		!(utils::isRefVector(type) || utils::isRefArray(type))) 
	{
		return builder.deref(value);
	}
	return value;
}

template<class ClangExprTy>
ExpressionList ConversionFactory::ExprConverter::getFunctionArguments(const core::IRBuilder& builder, ClangExprTy* callExpr,
		const core::FunctionTypePtr& funcTy) {
	ExpressionList args;
	for (size_t argId = 0, end = callExpr->getNumArgs(); argId < end; ++argId) {
		core::ExpressionPtr&& arg = Visit( callExpr->getArg(argId) );
		// core::TypePtr&& argTy = arg->getType();
		if ( argId < funcTy->getParameterTypes().size() ) {
			const core::TypePtr& funcArgTy = funcTy->getParameterTypes()[argId];
			arg = utils::cast(arg, funcArgTy);
		} else {
			arg = utils::cast(arg, builder.getNodeManager().getLangBasic().getVarList());
		}
		args.push_back( arg );
	}

	return args;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								INTEGER LITERAL
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitIntegerLiteral(clang::IntegerLiteral* intLit) {
	START_LOG_EXPR_CONVERSION(intLit);

	core::ExpressionPtr retExpr;

	/**********************************************
	 *  DEPRECATED CODE: do not read raw code
	 *  1)  translation unit use is deprecated
	 *  2)	templating may fail with this aproach
	 *
	std::string&& strVal =
	GetStringFromStream( convFact.currTU->getCompiler().getSourceManager(), intLit->getExprLoc() );

	core::GenericTypePtr intTy = core::static_pointer_cast<const core::GenericType>(
			convFact.convertType(GET_TYPE_PTR(intLit)));

	return (retExpr = builder.literal(
	// retrieve the string representation from the source code
			strVal, intTy));
	*************************************************/

	std::string value;
	if (!intLit->getValue().isNegative()) {
		value = toString(intLit->getValue().getLimitedValue());
	}
	else{
		value = toString(intLit->getValue().getSExtValue());
	}

	core::TypePtr type;
	int width = intLit->getValue().getBitWidth()/8;
	switch(width){
		case 4:
			type = builder.getLangBasic().getInt4();
			break;
		case 8:
			type = builder.getLangBasic().getInt8();
			break;
		case 16:
			type = builder.getLangBasic().getInt16();
			break;
		default:
			assert(false && "unknow integer literal width");
	}

    retExpr =  builder.literal(type, toString(value));
	END_LOG_EXPR_CONVERSION(retExpr);
	return retExpr; 

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								FLOATING LITERAL
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitFloatingLiteral(clang::FloatingLiteral* floatLit) {
	START_LOG_EXPR_CONVERSION(floatLit);

	core::ExpressionPtr retExpr;

	/* ****************************************************
	 * DEPRECATED CODE
	return (retExpr =
	// retrieve the string representation from the source code
			builder.literal(
					GetStringFromStream(convFact.currTU->getCompiler().getSourceManager(), floatLit->getExprLoc()),
					convFact.convertType(GET_TYPE_PTR(floatLit))));
	**************************************************/

	const llvm::fltSemantics& sema = floatLit->getValue().getSemantics();

	if (llvm::APFloat::semanticsPrecision(sema) == llvm::APFloat::semanticsPrecision(llvm::APFloat::IEEEsingle))
		retExpr = builder.floatLit (floatLit->getValue().convertToFloat());
	else if (llvm::APFloat::semanticsPrecision(sema) == llvm::APFloat::semanticsPrecision(llvm::APFloat::IEEEdouble))
		retExpr = builder.doubleLit (floatLit->getValue().convertToDouble());
	else
		assert (false &&"no idea how you got here, but only single/double precission literals are allowed in insieme");


	END_LOG_EXPR_CONVERSION(retExpr);
	return retExpr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								CHARACTER LITERAL
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitCharacterLiteral(CharacterLiteral* charLit) {
	START_LOG_EXPR_CONVERSION(charLit);

	core::ExpressionPtr retExpr;

	/* ****************************************************
	 * DEPRECATED CODE
	return (retExpr = builder.literal(
			// retrieve the string representation from the source code
			GetStringFromStream(convFact.currTU->getCompiler().getSourceManager(), charLit->getExprLoc()),
			(charLit->getKind() == CharacterLiteral::Wide ?
					mgr.getLangBasic().getWChar() : mgr.getLangBasic().getChar())));
	********************************************************/
	
	string value;
	unsigned int v = charLit->getValue();

	if (charLit->getKind() == clang::CharacterLiteral::Ascii){

		value.append("\'");
		if(v == '\\') value.append("\\\\"); 
		else if(v == '\n') value.append("\\n");
		else if(v == '\t') value.append("\\t");
		else if(v == '\b') value.append("\\b");
		else if(v == '\a') value.append("\\a");
		else if(v == '\v') value.append("\\v");
		else if(v == '\r') value.append("\\r");
		else if(v == '\f') value.append("\\f");
		else if(v == '\?') value.append("\\\?");
		else if(v == '\'') value.append("\\\'");
		else if(v == '\"') value.append("\\\"");
		else if(v == '\0') value.append("\\0");
		else{
			char cad[2];
			cad[0] = v;
			cad[1] = '\0';
			value.append(cad);
		}
		value.append("\'");
	}
	else
		/// FIXME: windows and linux implementation may differ here, need to study the case
		assert (false && "widechar not supported");
		
	retExpr = builder.literal(
			value,
			(charLit->getKind() == CharacterLiteral::Wide ?
					mgr.getLangBasic().getWChar() : mgr.getLangBasic().getChar()));

	END_LOG_EXPR_CONVERSION(retExpr);
	return retExpr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								STRING LITERAL
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitStringLiteral(clang::StringLiteral* stringLit) {
	core::ExpressionPtr retExpr;
	LOG_EXPR_CONVERSION(retExpr);

	std::string strValue = stringLit->getString().str();
	auto expand = [&](char lookup, const char *replacement) { 
		int last = 0;
		int it;
		string rep = replacement;
		while((it = strValue.find(lookup, last)) < strValue.length()){
			last = it + rep.length();
			strValue.replace(it, 1, rep);
		}
	};

	expand('\\', "\\\\"); 
	expand('\n', "\\n");
	expand('\t', "\\t");
	expand('\b', "\\b");
	expand('\a', "\\a");
	expand('\v', "\\v");
	expand('\r', "\\r");
	expand('\f', "\\f");
	expand('\?', "\\\?");
	expand('\'', "\\\'");
	expand('\"', "\\\"");
	expand('\0', "\\0");

	auto vecType = 
		builder.refType(
			builder.vectorType(
				gen.getChar(), 
				core::ConcreteIntTypeParam::get(builder.getNodeManager(), strValue.length()+1)
			)
		);

	retExpr = builder.literal("\"" + strValue + "\"", vecType);

	VLOG(2) << retExpr;

	return retExpr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							PARENTESIS EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitParenExpr(clang::ParenExpr* parExpr) {
	core::ExpressionPtr retExpr;

	LOG_EXPR_CONVERSION(retExpr);
	return (retExpr = Visit(parExpr->getSubExpr()));
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//					      GNU NULL EXPR EXPRESSION
//
// GNUNullExpr - Implements the GNU __null extension, which is a name for a
// null pointer constant that has integral type (e.g., int or long) and is
// the same size and alignment as a pointer. The __null extension is
// typically only used by system headers, which define NULL as __null in
// C++ rather than using 0 (which is an integer that may not match the size
// of a pointer).
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitGNUNullExpr(clang::GNUNullExpr* nullExpr) {
	core::TypePtr&& type = convFact.convertType(GET_TYPE_PTR(nullExpr));
	assert(type->getNodeType() != core::NT_ArrayType && "C pointer type must of type array<'a,1>");
	return builder.callExpr(gen.getGetNull(), builder.getTypeLiteral(type));
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						  IMPLICIT CAST EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitImplicitCastExpr(clang::ImplicitCastExpr* castExpr) {
	START_LOG_EXPR_CONVERSION(castExpr);

	core::ExpressionPtr retIr = Visit(castExpr->getSubExpr());
	auto type = convFact.convertType(GET_TYPE_PTR(castExpr));

	LOG_EXPR_CONVERSION(retIr);

	// capture the case of cast to arrays. because arrays cannot exists without a ref we need 
	// to make sure to add a refVar to the element 
	if (utils::isRefArray(type) && utils::isVector(retIr->getType()))
	{
		return retIr = utils::cast(builder.refVar(retIr), type);
	}

	// handle implicit casts according to their kind
	switch (castExpr->getCastKind()) {
	case CK_LValueToRValue:
		return (retIr = asRValue(retIr));

	case CK_ArrayToPointerDecay:
		return retIr;

	case CK_NoOp:
		//CK_NoOp - A conversion which does not affect the type other than (possibly) adding qualifiers. int -> int char** -> const char * const *
		return retIr;

	default:
		// use default cast expr handling (fallback)
		return (retIr = VisitCastExpr(castExpr));
	}
	assert(false);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						EXPLICIT CAST EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitExplicitCastExpr(clang::ExplicitCastExpr* castExpr) {
	START_LOG_EXPR_CONVERSION(castExpr);

	core::ExpressionPtr retIr = Visit(castExpr->getSubExpr());
	LOG_EXPR_CONVERSION(retIr);

	VLOG(2) << retIr << " " << retIr->getType();
	switch (castExpr->getCastKind()) {

	case CK_NoOp:
		//CK_NoOp - A conversion which does not affect the type other than (possibly) adding qualifiers. int -> int char** -> const char * const *
		return retIr;

	default:
		// use default cast expr handling (fallback)
		return (retIr = VisitCastExpr(castExpr));
	}

	assert(false);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//								CAST EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitCastExpr(clang::CastExpr* castExpr) {
	START_LOG_EXPR_CONVERSION(castExpr);

	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(retIr);

	retIr = Visit(castExpr->getSubExpr());
	auto nonRefExpr = convFact.tryDeref(retIr);

	auto type = convFact.convertType(GET_TYPE_PTR(castExpr));

	if (gen.isUnit(type)) { return gen.getUnitConstant(); }

	// if the cast is to a 'void*' type then we simply skip it
	if (gen.isAnyRef(type)) { return retIr; }

	if ((type->getNodeType() == core::NT_RefType) && (*retIr == *builder.literal(retIr->getType(), "0"))) {
		return (retIr = builder.callExpr(gen.getGetNull(),
				builder.getTypeLiteral(GET_REF_ELEM_TYPE(type))));
	}

	// Mallocs/Allocs are replaced with ref.new expression
	if (core::ExpressionPtr&& retExpr = handleMemAlloc(builder, type, retIr))
		return (retIr = retExpr);

	// If the subexpression is a string, remove the implicit casts
	if (gen.isString(retIr->getType())) {
		return retIr;
	}

	// handle truncation of floating point numbers
	const core::TypePtr& subExprType = retIr->getType();
	if (subExprType->getNodeType() == core::NT_RefType) {
		// check whether it is a truncation
		if (gen.isReal(GET_REF_ELEM_TYPE(subExprType)) && gen.isSignedInt(type)) {
			const core::GenericTypePtr& intType = static_pointer_cast<const core::GenericType>(type);
			return (retIr = builder.callExpr(type, gen.getRealToInt(), nonRefExpr,
					builder.getIntTypeParamLiteral(intType->getIntTypeParameter()[0])));
		}
	}

	VLOG(2) << retIr << retIr->getType();
	// LOG(DEBUG) << *subExpr << " -> " << *type;
	// Convert casts form scalars to vectors to vector init exrpessions
	return (retIr = utils::cast(retIr, type));
}


//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							FUNCTION CALL EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitCallExpr(clang::CallExpr* callExpr) {
	START_LOG_EXPR_CONVERSION(callExpr);

	// return converted node
	core::ExpressionPtr irNode;
	LOG_EXPR_CONVERSION(irNode);

	if (callExpr->getDirectCallee()) {

		FunctionDecl* funcDecl = llvm::cast<FunctionDecl>(callExpr->getDirectCallee());
		auto funcTy = convFact.convertType(GET_TYPE_PTR(funcDecl)).as<core::FunctionTypePtr>() ;

		// collects the type of each argument of the expression
		ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);

		assert(!convFact.currTU.empty() && "Translation unit not set.");

		const FunctionDecl* definition = NULL;
		const TranslationUnit* rightTU = NULL;

		// this will find function definitions if they are declared in  the same translation unit
		// (also defined as static)
		if (!funcDecl->hasBody(definition)) {
			// if the function is not defined in this translation unit, maybe it is defined in another we already
			// loaded use the clang indexer to lookup the definition for this function declarations
			FunctionDecl* fd = funcDecl;
			rightTU = convFact.getTranslationUnitForDefinition(fd);
			if (rightTU && fd->hasBody()) { definition = fd; }
		}

		// point to the right TU
		if (rightTU)
			convFact.currTU.push (rightTU);
		else
			convFact.currTU.push (convFact.currTU.top());


		if (!definition) {
			//-----------------------------------------------------------------------------------------------------
			//     						Handle of 'special' built-in functions
			//-----------------------------------------------------------------------------------------------------
			// free(): check whether this is a call to the free() function
			if (funcDecl->getNameAsString() == "free" && callExpr->getNumArgs() == 1) {
				// in the case the free uses an input parameter
				if (args.front()->getType()->getNodeType() == core::NT_RefType) {
					return (irNode = builder.callExpr(builder.getLangBasic().getUnit(),
							builder.getLangBasic().getRefDelete(), args.front()));
				}

				// select appropriate deref operation: AnyRefDeref for void*, RefDeref for anything else
				core::ExpressionPtr arg = wrapVariable(callExpr->getArg(0));
				core::ExpressionPtr delOp =
						*arg->getType() == *builder.getLangBasic().getAnyRef() ?
								builder.getLangBasic().getAnyRefDelete() : builder.getLangBasic().getRefDelete();

				// otherwise this is not a L-Value so it needs to be wrapped into a variable
				return (irNode = builder.callExpr(builder.getLangBasic().getUnit(), delOp, arg));
			}
		}

		ExpressionList&& packedArgs = tryPack(builder, funcTy, args);

		// No definition has been found in any translation unit, 
		// we mark this function as extern. and return
		if (!definition) {
			std::string callName = funcDecl->getNameAsString();
			irNode = builder.callExpr(funcTy->getReturnType(), builder.literal(callName, funcTy),
					packedArgs);

			// In the case this is a call to MPI, attach the loc annotation, handlling of those
			// statements will be then applied by mpi_sema
			if (callName.compare(0, 4, "MPI_") == 0) {

				auto loc = std::make_pair(callExpr->getLocStart(), callExpr->getLocEnd());

				// add a marker node because multiple istances of the same MPI call must be distinct
				irNode = builder.markerExpr( core::static_pointer_cast<const core::Expression>(irNode) );

				irNode->addAnnotation( std::make_shared<annotations::c::CLocAnnotation>(
								convertClangSrcLoc(convFact.getCurrentSourceManager(), loc.first),
								convertClangSrcLoc(convFact.getCurrentSourceManager(), loc.second))
				);
			}

			return irNode;
		}

		// =====  We found a definition for funcion, need to be translated ======
		
		// need tolookup if this fuction needs to access the globals, in that case the capture
		// list needs to be initialized with the value of global variable in the current scope
		if (ctx.globalFuncMap.find(definition) != ctx.globalFuncMap.end()) {
			// we expect to have a the currGlobalVar set to the value of the var keeping global definitions in the
			// current context
			assert( ctx.globalVar && "No global definitions forwarded to this point");
			packedArgs.insert(packedArgs.begin(), ctx.globalVar);
		}

		// function definition might be already processed and cached, 
		// otherwise create the call expression node,
		ConversionContext::LambdaExprMap::const_iterator fit = ctx.lambdaExprCache.find(definition);
		if (fit != ctx.lambdaExprCache.end()) {
			irNode = builder.callExpr(funcTy->getReturnType(), static_cast<core::ExpressionPtr>(fit->second),
					 packedArgs);

			convFact.currTU.pop();
			return irNode;
		}

		assert(definition && "No definition found for function");

		auto lambdaExpr = core::static_pointer_cast<const core::Expression>(
				convFact.convertFunctionDecl(definition));

		convFact.currTU.pop();
		return (irNode = builder.callExpr(funcTy->getReturnType(), lambdaExpr, packedArgs));
	} 


	// if there callee is not a fuctionDecl we need to use other method.
	// it might be a pointer to function. or another artifact
	if ( callExpr->getCallee() ) {
		core::ExpressionPtr funcPtr = convFact.tryDeref(Visit(callExpr->getCallee()));
		core::TypePtr subTy = funcPtr->getType();

		if (subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType) {

			subTy = subTy.as<core::SingleElementTypePtr>()->getElementType();
			funcPtr = builder.callExpr(subTy, builder.getLangBasic().getArraySubscript1D(), funcPtr, builder.uintLit(0));

		}
		
		assert( subTy->getNodeType() == core::NT_FunctionType && "Using () operator on a non function object");

		auto funcTy = subTy.as<core::FunctionTypePtr>();

		ExpressionList&& args = getFunctionArguments(builder, callExpr, funcTy);
		return irNode = builder.callExpr(funcPtr, args);
	} 
	
	assert( false && "Call expression not referring a function");

	return core::ExpressionPtr();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							PREDEFINED EXPRESSION
//
// [C99 6.4.2.2] - A predefined identifier such as __func__.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitPredefinedExpr(clang::PredefinedExpr* preExpr) {
	
	string lit;
	switch(preExpr->getIdentType()) {
	case PredefinedExpr::Func: 				
		lit = "__func__"; break;
	case PredefinedExpr::Function: 			
		lit = "__FUNCTION__"; break;
	case PredefinedExpr::PrettyFunction: 	
		lit = "__PRETTY_FUNCTION__"; break;
	case PredefinedExpr::PrettyFunctionNoVirtual: 
	default:
		assert(false && "Handle for predefined function not defined");
	}

	core::TypePtr type = convFact.convertType(GET_TYPE_PTR(preExpr));
	assert(type->getNodeType() == core::NT_VectorType);
	core::TypePtr elemType = type.as<core::VectorTypePtr>()->getElementType();
	return builder.literal(lit, builder.refType(builder.arrayType(elemType)));
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						SIZEOF ALIGNOF EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//core::ExpressionPtr VisitSizeOfAlignOfExpr(clang::SizeOfAlignOfExpr* expr) {
//START_LOG_EXPR_CONVERSION(expr);

//core::ExpressionPtr irNode;
//LOG_EXPR_CONVERSION(irNode);

//if ( expr->isSizeOf() ) {
//core::TypePtr&& type = expr->isArgumentType() ?
//convFact.convertType( expr->getArgumentType().getTypePtr() ) :
//convFact.convertType( expr->getArgumentExpr()->getType().getTypePtr() );
//return (irNode = getSizeOfType(convFact.getIRBuilder(), type));
//}
//assert(false && "SizeOfAlignOfExpr not yet supported");
//}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						UnaryExprOrTypeTraitExpr
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// UnaryExprOrTypeTraitExpr - expression with either a type or (unevaluated)
// expression operand. Used for sizeof/alignof (C99 6.5.3.4) and vec_step
// (OpenCL 1.1 6.11.12).
core::ExpressionPtr ConversionFactory::ExprConverter::VisitUnaryExprOrTypeTraitExpr(clang::UnaryExprOrTypeTraitExpr* expr) {
	START_LOG_EXPR_CONVERSION(expr);
	core::ExpressionPtr irNode;

	switch (expr->getKind()) {
	case UETT_SizeOf: {
		core::TypePtr&& type = expr->isArgumentType() ?
		convFact.convertType( expr->getArgumentType().getTypePtr() ) :
		convFact.convertType( expr->getArgumentExpr()->getType().getTypePtr() );
		return (irNode = getSizeOfType(builder, type));
	}
	case UETT_AlignOf:
	case UETT_VecStep:
	default:
	assert(false && "Kind of expressions not handled");
	return core::ExpressionPtr();
}

}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							MEMBER EXPRESSION
//
// [C99 6.5.2.3] Structure and Union Members. X->F and X.F.
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitMemberExpr(clang::MemberExpr* membExpr) {
	START_LOG_EXPR_CONVERSION(membExpr);

	core::ExpressionPtr&& base = Visit(membExpr->getBase());

	if(membExpr->isArrow()) {
		/*
		 * we have to check whether we currently have a ref or probably an array (which is used to represent
		 * C pointers)
		 */
		//VLOG(2) << "is arrow " << base->getType();
		base = getCArrayElemRef(builder, base);
	}

	core::TypePtr structTy = base->getType();

	// Start to build the return Expression from here
	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(retIr);

	core::ExpressionPtr op = gen.getCompositeMemberAccess();

	if (structTy->getNodeType() == core::NT_RefType) {
		// skip over reference wrapper
		structTy = core::analysis::getReferencedType( structTy );
		op = gen.getCompositeRefElem();
	}

	// There are 2 basic cases which need to be handled: Struct/Unions and Recursive Types
	assert((structTy->getNodeType() == core::NT_StructType || structTy->getNodeType() == core::NT_UnionType ||
					structTy->getNodeType() == core::NT_RecType) &&
			"Using a member access operation on a non struct/union type"
	);

	// if the inner type is a RecType then we need to unroll it to get the contained composite type
	if ( structTy->getNodeType() == core::NT_RecType ) {
		structTy = core::static_pointer_cast<const core::RecType>(structTy)->unroll(mgr);
	}
	assert(structTy && "Struct Type not being initialized");

	//identifier of the member
	core::StringValuePtr ident;
	core::NamedCompositeTypePtr compType = core::static_pointer_cast<const core::NamedCompositeType>(structTy);

	if (!membExpr->getMemberDecl()->getIdentifier()) {

		FieldDecl* field = dyn_cast<FieldDecl>(membExpr->getMemberDecl());
		assert(field && field->isAnonymousStructOrUnion());

		// Union may have anonymous member which have been tagged with a '__m' name by the type
		// convert
		ident = builder.stringValue("__m"+insieme::utils::numeric_cast<std::string>(field->getFieldIndex()));
	} else {
		ident = builder.stringValue(membExpr->getMemberDecl()->getName().data());
	}

	assert(ident);

	auto memberTy = structTy.as<core::NamedCompositeTypePtr>()->getTypeOfMember(ident);
	auto resType = memberTy;

	assert(resType);
	if (base->getType()->getNodeType() == core::NT_RefType) {
		resType = builder.refType(resType);
	}

	retIr = builder.callExpr(resType, op, base, builder.getIdentifierLiteral(ident), builder.getTypeLiteral(memberTy));

	END_LOG_EXPR_CONVERSION(retIr);
	VLOG(2) << "End of expression MemberExpr\n";
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							BINARY OPERATOR
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitBinaryOperator(clang::BinaryOperator* binOp) {
	START_LOG_EXPR_CONVERSION(binOp);
	core::ExpressionPtr retIr;

	core::ExpressionPtr&& rhs = Visit(binOp->getRHS());
	core::ExpressionPtr&& lhs = Visit(binOp->getLHS());

	// handle of volatile variables
	if (binOp->getOpcode() != BO_Assign && core::analysis::isVolatileType(lhs->getType()) ) {
		lhs = builder.callExpr( builder.getLangBasic().getVolatileRead(), lhs);
	}
	if ( core::analysis::isVolatileType(rhs->getType()) ) {
		rhs = builder.callExpr( builder.getLangBasic().getVolatileRead(), rhs);
	}

	// if the binary operator is a comma separated expression, we convert it into a function call which returns the
	// value of the last expression
	if ( binOp->getOpcode() == BO_Comma ) {
		
		core::TypePtr retType;
		// the return type of this lambda is the type of the last expression (according to the C
		// standard) 
		std::vector<core::StatementPtr> stmts { lhs };
		// LOG(INFO) << core::analysis::isCallOf(rhs, gen.getRefAssign());
		if (core::analysis::isCallOf(rhs, gen.getRefAssign())) {
			stmts.push_back(rhs);
			auto retExpr = rhs.as<core::CallExprPtr>()->getArgument(0);
			// additionally we have to return the value of the lhs of the assignment stmt 
			stmts.push_back(builder.returnStmt(retExpr));
			retType = retExpr->getType();
		} else {
			stmts.push_back(gen.isUnit(rhs->getType()) ? static_cast<core::StatementPtr>(rhs) : builder.returnStmt(rhs));
			retType = rhs->getType();
		}
		// LOG(INFO) << stmts;
		return (retIr = builder.createCallExprFromBody(builder.compoundStmt(stmts), retType));
	}

	// the type of this expression is the type of the LHS expression
	core::TypePtr exprTy = convFact.convertType( GET_TYPE_PTR(binOp) );
	//	lhs->getType()->getNodeType() == core::NT_RefType ?
	//		GET_REF_ELEM_TYPE(lhs->getType()) : lhs->getType();

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
		case BO_MulAssign: op = core::lang::BasicGenerator::Mul; baseOp = BO_Mul; break;
		// a /= b
		case BO_DivAssign: op = core::lang::BasicGenerator::Div; baseOp = BO_Div; break;
		// a %= b
		case BO_RemAssign: op = core::lang::BasicGenerator::Mod; baseOp = BO_Rem; break;
		// a += b
		case BO_AddAssign: op = core::lang::BasicGenerator::Add; baseOp = BO_Add; break;
		// a -= b
		case BO_SubAssign: op = core::lang::BasicGenerator::Sub; baseOp = BO_Sub; break;
		// a <<= b
		case BO_ShlAssign: op = core::lang::BasicGenerator::LShift; baseOp = BO_Shl; break;
		// a >>= b
		case BO_ShrAssign: op = core::lang::BasicGenerator::RShift; baseOp = BO_Shr; break;
		// a &= b
		case BO_AndAssign: op = core::lang::BasicGenerator::And; baseOp = BO_And; break;
		// a |= b
		case BO_OrAssign: op = core::lang::BasicGenerator::Or; baseOp = BO_Or; break;
		// a ^= b
		case BO_XorAssign: op = core::lang::BasicGenerator::Xor; baseOp = BO_Xor; break;
		default:
		isCompound = false;
	}

	// perform any pointer arithmetic needed
	auto doPointerArithmetic = [&] () {
		// LHS must be a ref<array<'a>>
		core::TypePtr subRefTy = GET_REF_ELEM_TYPE(lhs->getType());

		// In the case we have ref<ref<array<>>> then we deref again the argument 
		if (core::analysis::isRefType(subRefTy)) {
			lhs = builder.deref(lhs);
			subRefTy = lhs->getType();
		}

		assert( core::analysis::isRefType(lhs->getType()) );
		if(subRefTy->getNodeType() == core::NT_VectorType)
			lhs = builder.callExpr(gen.getRefVectorToRefArray(), lhs);

		// Capture pointer arithmetics
		// 	Base op must be either a + or a -
		assert( (baseOp == BO_Add || baseOp == BO_Sub) &&
				"Operators allowed in pointer arithmetic are + and - only");

		assert(GET_REF_ELEM_TYPE(lhs->getType())->getNodeType() == core::NT_ArrayType &&
				"LHS operator must be of type ref<array<'a,#l>>");

		// LOG(INFO) << rhs->getType();
		assert(gen.isInt(rhs->getType()) && "Array view displacement must be a signed int");
		if (gen.isUnsignedInt(rhs->getType()))
			rhs = builder.castExpr(gen.getInt8(), rhs);

		// check whether the RHS is of integer type
		rhs = builder.callExpr(gen.getArrayView(), lhs, baseOp == BO_Add ? rhs : builder.invertSign(rhs));
	};

	if ( isCompound ) {
		// we check if the RHS is a ref, in that case we use the deref operator
		rhs = convFact.tryDeref(rhs);

		// We have a compound operation applied to a ref<array<'a>> type which is probably one of
		// the function parameters, We have to wrap this variable in a way it becomes a
		// ref<ref<array<'a>>>
		if (core::analysis::isRefType(lhs->getType()) && !core::analysis::isRefType(rhs->getType()) &&
			core::analysis::getReferencedType(lhs->getType())->getNodeType() == core::NT_ArrayType) 
		{
			lhs = wrapVariable(binOp->getLHS());
		}

		// capture the case when pointer arithmetic is performed 
		if (core::analysis::isRefType(lhs->getType()) && !core::analysis::isRefType(rhs->getType()) &&
			core::analysis::isRefType(core::analysis::getReferencedType(lhs->getType())) &&  
			core::analysis::getReferencedType( core::analysis::getReferencedType(lhs->getType()))->getNodeType() == core::NT_ArrayType)
		{
			// do pointer arithmetic 
			doPointerArithmetic(); 
		} else {
			rhs = builder.callExpr(exprTy, gen.getOperator(exprTy, op), subExprLHS, rhs);
		}
			
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
		case BO_Mul: op = core::lang::BasicGenerator::Mul; break;
		// a / b
		case BO_Div: op = core::lang::BasicGenerator::Div; break;
		// a % b
		case BO_Rem: op = core::lang::BasicGenerator::Mod; break;
		// a + b
		case BO_Add: op = core::lang::BasicGenerator::Add; break;
		// a - b
		case BO_Sub: op = core::lang::BasicGenerator::Sub; break;
		// a << b
		case BO_Shl: op = core::lang::BasicGenerator::LShift; break;
		// a >> b
		case BO_Shr: op = core::lang::BasicGenerator::RShift; break;
		// a & b
		case BO_And: op = core::lang::BasicGenerator::And; break;
		// a ^ b
		case BO_Xor: op = core::lang::BasicGenerator::Xor; break;
		// a | b
		case BO_Or: op = core::lang::BasicGenerator::Or; break;

		// Logic operators

		// a && b
		case BO_LAnd: op = core::lang::BasicGenerator::LAnd; isLogical=true; break;
		// a || b
		case BO_LOr: op = core::lang::BasicGenerator::LOr; isLogical=true; break;
		// a < b
		case BO_LT: op = core::lang::BasicGenerator::Lt; isLogical=true; break;
		// a > b
		case BO_GT: op = core::lang::BasicGenerator::Gt; isLogical=true; break;
		// a <= b
		case BO_LE: op = core::lang::BasicGenerator::Le; isLogical=true; break;
		// a >= b
		case BO_GE: op = core::lang::BasicGenerator::Ge; isLogical=true; break;
		// a == b
		case BO_EQ: op = core::lang::BasicGenerator::Eq; isLogical=true; break;
		// a != b
		case BO_NE: op = core::lang::BasicGenerator::Ne; isLogical=true; break;

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

			// make sure the lhs is a L-Value
			lhs = asLValue(lhs);

			// This is an assignment, we have to make sure the LHS operation is of type ref<a'>
			assert( lhs->getType()->getNodeType() == core::NT_RefType && "LHS operand must be of type ref<'a>.");

			rhs = utils::cast(rhs, GET_REF_ELEM_TYPE(lhs->getType()));
			isAssignment = true;
			opFunc = gen.getRefAssign();
			exprTy = gen.getUnit();
			break;
		}
		default:
		assert(false && "Operator not supported");
	}

	// Operators && and || introduce short circuit operations, this has to be directly supported in the IR.
	if ( baseOp == BO_LAnd || baseOp == BO_LOr ) {
		lhs = utils::cast(lhs, gen.getBool());
		rhs = utils::cast(rhs, gen.getBool());
		// lazy evaluation of RHS
		exprTy = gen.getBool();

		rhs = builder.createCallExprFromBody(builder.returnStmt(rhs), gen.getBool(), true);
	}

	VLOG(2) << "LHS( " << *lhs << "[" << *lhs->getType() << "]) " << opFunc <<
	" RHS(" << *rhs << "[" << *rhs->getType() << "])" << std::endl;;

	if( !isAssignment ) {

		core::TypePtr&& lhsTy = lhs->getType();
		core::TypePtr&& rhsTy = rhs->getType();
		VLOG(2) << "LHS( " << *lhs << "[" << *lhs->getType() << "]) " << opFunc <<
		" RHS(" << *rhs << "[" << *rhs->getType() << "])" << std::endl;

		if(binOp->getLHS()->getType().getUnqualifiedType()->isExtVectorType() ||
				binOp->getRHS()->getType().getUnqualifiedType()->isExtVectorType()) { // handling for ocl-vector operations
			
			lhs = utils::cast(lhs, exprTy);
			// check if lhs is not an ocl-vector, in this case create a vector form the scalar
//			if(binOp->getLHS()->getStmtClass() == Stmt::ImplicitCastExprClass) { 
//			// the rhs is a scalar, implicitly casted to a vector
//				// lhs is a scalar
//				lhs = scalarToVector(lhs, rhsTy, builder, convFact);
//			} else
//				lhs = convFact.tryDeref(lhs); // lhs is an ocl-vector
//
//			if(binOp->getRHS()->getStmtClass() == Stmt::ImplicitCastExprClass ) { 
//			// the rhs is a scalar, implicitly casted to a vector
//				// rhs is a scalar
//				rhs = scalarToVector(rhs, lhsTy, builder, convFact);
//			} else
			rhs = utils::cast(rhs, exprTy); // rhs is an ocl-vector

			// generate a ocl_vector - scalar operation
			opFunc = gen.getOperator(lhs->getType(), op);

			// TODO to be tested
			if (const core::FunctionTypePtr funTy = core::dynamic_pointer_cast<const core::FunctionType>(opFunc->getType()))
				if(funTy->getReturnType() == funTy->getParameterTypeList().at(0)) { // check if we can use the type of the first argument as retun type
					return (retIr = builder.callExpr(lhs->getType(), opFunc, lhs, utils::cast(rhs, lhs->getType())));
				} else { // let deduce it otherwise
					return (retIr = builder.callExpr(opFunc, lhs, utils::cast(rhs, lhs->getType())));
				}
			else {
				assert(false && "old stuff needed, tell Klaus");
				return (retIr = builder.callExpr(lhsTy, opFunc, lhs, rhs));
			}
			assert(false && "Never reach this point");
		}

		if (!core::analysis::isRefType(rhs->getType()) && 
			(utils::isRefArray(lhs->getType()) || utils::isRefVector(lhs->getType()))
		) {
			doPointerArithmetic();	
			return (retIr = rhs);
		}

		if (!core::analysis::isRefType(lhs->getType()) && 
			(utils::isRefArray(rhs->getType()) || utils::isRefVector(rhs->getType()))
		) {
			std::swap(rhs, lhs);
			doPointerArithmetic();	
			return (retIr = rhs);
		}
	
		if (utils::isRefArray(lhs->getType()) &&  utils::isRefArray(rhs->getType()) &&
			baseOp == BO_Sub)
		{
			return retIr = builder.callExpr( gen.getArrayRefDistance(), lhs, rhs);
		}

		if ( lhsTy->getNodeType() != core::NT_RefType && rhsTy->getNodeType() != core::NT_RefType)
		{
			// ----------------------------- Hack begin --------------------------------
			// TODO: this is a quick solution => maybe clang allows you to determine the actual type
			// => otherwise the sub-type checks within the core may be used
			//
			// Bug fixed by this:
			//		when multiplying an integer with a double, the double is casted to an integer and the
			//		results is an integer.
			//

			// check whether result type needs to be adjusted
			// if (*lhsTy != *rhsTy) {
			//	// if second argument is a real
			//	if (!gen.isReal(lhsTy) && gen.isReal(rhsTy)) {
			//		exprTy = rhsTy;
			//	}
			//}
			// ----------------------------- Hack end --------------------------------
			// all binary operators have the same input and result types
			if (baseOp != BO_LAnd && baseOp != BO_LOr) {
				lhs = utils::cast(lhs, convFact.convertType( GET_TYPE_PTR(binOp->getLHS())) );
				rhs = utils::cast(rhs, convFact.convertType( GET_TYPE_PTR(binOp->getRHS())) );
				
				// LOG(INFO) << lhs->getType() << " " << rhs->getType(); 
				if (*lhs->getType()==*rhs->getType()) {
					exprTy = lhs->getType();
				}
				// LOG(INFO) << exprTy;
			}
			// Handle pointers arithmetic
			VLOG(2) << "Lookup for operation: " << op << ", for type: " << *exprTy;
			opFunc = gen.getOperator(exprTy, op);
		}
		if (lhsTy->getNodeType() == core::NT_RefType && rhsTy->getNodeType() == core::NT_RefType) {
			assert(*lhsTy == *rhsTy && "Comparing incompatible types");
			opFunc = gen.getOperator(lhsTy, op);
		}
		if ( DeclRefExpr* declRefExpr = utils::skipSugar<DeclRefExpr>(binOp->getLHS()) ) {
			if ( isa<ArrayType>(declRefExpr->getDecl()->getType().getTypePtr()) )
				assert(false && "Pointer arithmetic not yet supported");
		}
		if(isLogical) { exprTy = gen.getBool(); }

	} else {
		// check if there is a kernelFile annotation
		ocl::attatchOclAnnotation(rhs, binOp, convFact);
	}

	assert(opFunc);

	/*if ( !isAssignment ) {*/
	//// We know thie operator now we have to make sure that all the arguments are of the correct type
	//core::FunctionTypePtr funcTy = core::static_pointer_cast<const core::FunctionType>( opFunc->getType() );
	//assert(funcTy->getParameterTypes().size() == 2);
	//core::TypePtr lhsFuncTy = funcTy->getParameterTypes()[0];
	//if (!gen.isUIntGen(lhsFuncTy) && !gen.isRealGen(lhsFuncTy) && !gen.isIntGen(lhsFuncTy)) {
	//lhs = cast(lhsFuncTy, lhs);
	//}
	//core::TypePtr rhsFuncTy = funcTy->getParameterTypes()[1];
	//if (!gen.isUIntGen(rhsFuncTy) && !gen.isRealGen(rhsFuncTy) && !gen.isIntGen(rhsFuncTy)) {
	//rhs = cast(rhsFuncTy, rhs);
	//}
	//if (*lhsFuncTy == *rhsFuncType && *lhs->getType() != *rhs->getType()) {
	//// we still need to adjust
	//}
	/*}*/
	// add source code annotation to the rhs if present
	
	VLOG(2) << "LHS( " << *lhs << "[" << *lhs->getType() << "]) " << opFunc <<
				" RHS(" << *rhs << "[" << *rhs->getType() << "])" << std::endl;

	retIr = builder.callExpr( exprTy, opFunc, lhs, rhs );
	return retIr;
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							UNARY OPERATOR
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitUnaryOperator(clang::UnaryOperator *unOp) {
	START_LOG_EXPR_CONVERSION(unOp);

	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(retIr);

	core::ExpressionPtr&& subExpr = Visit(unOp->getSubExpr());

	// build lambda expression for post/pre increment/decrement unary operators
	auto encloseIncrementOperator =
	[ this, &builder, &gen ]
	(core::ExpressionPtr subExpr, core::lang::BasicGenerator::Operator op) -> core::ExpressionPtr {

		if ((subExpr->getNodeType() == core::NT_Variable && 
			 subExpr->getType()->getNodeType() != core::NT_RefType) 
			|| 
			(subExpr->getNodeType() == core::NT_Variable && 
			 subExpr->getType()->getNodeType() == core::NT_RefType && 
			 GET_REF_ELEM_TYPE(subExpr->getType())->getNodeType() == core::NT_ArrayType)) 
		{
			// It can happen we are incrementing a variable which is coming from an input
			// argument of a function
			core::VariablePtr var = subExpr.as<core::VariablePtr>();

			auto&& fit = convFact.ctx.wrapRefMap.find(var);
			if ( fit == convFact.ctx.wrapRefMap.end() ) {
				fit = convFact.ctx.wrapRefMap.insert(
						std::make_pair( var, builder.variable( builder.refType(var->getType()) ) )
				).first;
			}

			subExpr = fit->second;
		}

		core::TypePtr type = subExpr->getType();
		assert( type->getNodeType() == core::NT_RefType && "Illegal increment/decrement operand - not a ref type" );
		core::TypePtr elementType = GET_REF_ELEM_TYPE(type);

		if ( core::analysis::isRefType(elementType) && 
			 (GET_REF_ELEM_TYPE(elementType)->getNodeType() == core::NT_ArrayType)) 
		{
			// if this is a post/pre incremenet operator applied to an array we have to deal with it
			// immediatelly because the getOperator function wouldn't deal with such case 

			core::ExpressionPtr opLit;
			switch (op) {
			case core::lang::BasicGenerator::PreInc:  opLit = gen.getArrayViewPreInc(); 	break;
			case core::lang::BasicGenerator::PostInc: opLit = gen.getArrayViewPostInc();	break;
			case core::lang::BasicGenerator::PreDec:  opLit = gen.getArrayViewPreDec();		break;
			case core::lang::BasicGenerator::PostDec: opLit = gen.getArrayViewPostDec();	break;
			default:
				assert(false && "Operator not handled for pointer arithmetic");
			}
			return builder.callExpr(elementType, opLit, subExpr);
		}

		switch (op) {
		case core::lang::BasicGenerator::PreInc: 	return builder.preInc(subExpr);
		case core::lang::BasicGenerator::PostInc:	return builder.postInc(subExpr);
		case core::lang::BasicGenerator::PreDec:	return builder.preDec(subExpr);
		case core::lang::BasicGenerator::PostDec:	return builder.postDec(subExpr);
		default :
			assert(false);
		}
		return core::ExpressionPtr(); // should not be reachable
	};

	switch ( unOp->getOpcode() ) {
		// conversion of post increment/decrement operation is done by creating a tuple expression i.e.:
		// a++ ==> (__tmp = a, a=a+1, __tmp)
		// ++a ==> ( a=a+1, a)
		// --a
	case UO_PreDec:  return retIr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PreDec);
	// a--
	case UO_PostDec: return (retIr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PostDec));
	// a++
	case UO_PreInc:  return (retIr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PreInc));
	// ++a
	case UO_PostInc: return (retIr = encloseIncrementOperator(subExpr, core::lang::BasicGenerator::PostInc));
	// &a
	case UO_AddrOf:
		{
			/*
			 * We need to be careful paramvars are not dereferenced and the address passed around. If this happens
			 * we have to declare a variable holding the memory location for that value and replace every use of
			 * the paramvar with the newly generated variable: the structure needRef in the ctx is used for this
			 */
			retIr = wrapVariable(unOp->getSubExpr());

			// in the case we are getting the address of a function the & operator
			// has no effects, therefore we return
			if (retIr->getType()->getNodeType() == core::NT_FunctionType) {
				return retIr;
			}

			// make sure it is a L-Value
			retIr = asLValue(retIr);

			assert(retIr->getType()->getNodeType() == core::NT_RefType);
			return (retIr = utils::refScalarToRefArray(retIr));
		}
	// *a
	case UO_Deref: {
			// make sure it is a L-Value
			retIr = asLValue(subExpr);

			assert(retIr->getType()->getNodeType() == core::NT_RefType &&
					"Impossible to apply * operator to an R-Value");

			const core::TypePtr& subTy = GET_REF_ELEM_TYPE(retIr->getType());

			return (retIr =
					(subTy->getNodeType() == core::NT_VectorType || subTy->getNodeType() == core::NT_ArrayType) ?
					getCArrayElemRef(builder, retIr) : convFact.tryDeref(retIr)
			);
		}
	// +a
	case UO_Plus:  return retIr = subExpr;
	// -a
	case UO_Minus: return (retIr = builder.invertSign( convFact.tryDeref(subExpr) ));
	// ~a
	case UO_Not:
		retIr = convFact.tryDeref(subExpr);
		return (retIr = builder.callExpr(
						retIr->getType(),
						gen.getOperator(retIr->getType(), core::lang::BasicGenerator::Not),
						retIr)
		);
		// !a
	case UO_LNot:
		if( !gen.isBool(subExpr->getType()) ) {
			subExpr = utils::cast(subExpr, gen.getBool());
		}
		assert( gen.isBool(subExpr->getType()) );

		return (retIr = builder.callExpr( subExpr->getType(), gen.getBoolLNot(), subExpr ) );

	case UO_Extension: 
		return retIr = subExpr;

	case UO_Real:
	case UO_Imag:
	default:
		assert(false && "Unary operator not supported");
	}
	return core::ExpressionPtr();	// should not be reachable
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							CONDITIONAL OPERATOR
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitConditionalOperator(clang::ConditionalOperator* condOp) {
	START_LOG_EXPR_CONVERSION(condOp);

	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(retIr);

	core::TypePtr retTy = convFact.convertType( GET_TYPE_PTR(condOp) );
	core::ExpressionPtr trueExpr  = Visit(condOp->getTrueExpr());
	core::ExpressionPtr falseExpr = Visit(condOp->getFalseExpr());
	core::ExpressionPtr condExpr  = Visit( condOp->getCond() );

	condExpr = utils::cast(condExpr, gen.getBool());

	// Dereference eventual references
	if ( retTy->getNodeType() == core::NT_RefType && !utils::isRefArray(retTy) ) 
	{
		retTy = GET_REF_ELEM_TYPE(retTy);
	}

	// in C++, string literals with same size may produce an error, do not cast to avoid 
	// weird behaviour 
	if (!llvm::isa<StringLiteral>(condOp->getTrueExpr()) ||
		!llvm::isa<StringLiteral>(condOp->getTrueExpr())){

		trueExpr  = utils::cast(trueExpr, retTy);
		falseExpr = utils::cast(falseExpr, retTy);
	}
	else{
		retTy = trueExpr->getType();
	}


	return (retIr =
			builder.callExpr(retTy, gen.getIfThenElse(),
					condExpr, // Condition
					builder.createCallExprFromBody(
							builder.returnStmt(trueExpr), retTy, true
					),// True
					builder.createCallExprFromBody(
							builder.returnStmt(falseExpr), retTy, true
					)// False
			)
	);
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						ARRAY SUBSCRIPT EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitArraySubscriptExpr(clang::ArraySubscriptExpr* arraySubExpr) {
	START_LOG_EXPR_CONVERSION(arraySubExpr);

	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(retIr);

	/*
	 * CLANG introduces implicit cast for the base expression of array subscripts which cast the array type into a
	 * simple pointer. As insieme supports subscripts only for array or vector types, we skip eventual implicit
	 * cast operations.
	 */
	Expr* baseExpr = arraySubExpr->getBase();

	// IDX
	core::ExpressionPtr idx = convFact.tryDeref( Visit( arraySubExpr->getIdx() ) );
	if (!gen.isUInt4(idx->getType())) {
		idx = builder.castExpr(gen.getUInt4(), idx);
	}

	// BASE
	core::ExpressionPtr base = Visit( baseExpr );

	core::TypePtr opType;
	core::LiteralPtr op;

	if ( base->getType()->getNodeType() == core::NT_RefType ) {
		// The vector/array is an L-Value so we use the array.ref.elem
		// operator to return a reference to the addressed memory location
		core::TypePtr refSubTy = GET_REF_ELEM_TYPE(base->getType());

		// TODO: we need better checking for vector type
		assert( (refSubTy->getNodeType() == core::NT_VectorType ||
						refSubTy->getNodeType() == core::NT_ArrayType) &&
				"Base expression of array subscript is not a vector/array type.");

		op = refSubTy->getNodeType() == core::NT_ArrayType ? gen.getArrayRefElem1D() : gen.getVectorRefElem();

		opType = builder.refType(refSubTy.as<core::SingleElementTypePtr>()->getElementType());

	} else {

		/*
		 * The vector/array is an R-value (e.g. (int[2]){0,1}[1] ) in this case the subscript returns an R-value so
		 * the array.subscript operator must be used
		 */
		// TODO: we need better checking for vector type
		assert( (base->getType()->getNodeType() == core::NT_VectorType ||
						base->getType()->getNodeType() == core::NT_ArrayType) &&
				"Base expression of array subscript is not a vector/array type.");

		op = base->getType()->getNodeType() == core::NT_ArrayType ? gen.getArraySubscript1D() : gen.getVectorSubscript();

		opType = core::static_pointer_cast<const core::SingleElementType>(base->getType())->getElementType();
	}

	return (retIr = builder.callExpr( opType, op, base, idx) );
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//						EXT VECTOR ELEMENT EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitExtVectorElementExpr(ExtVectorElementExpr* vecElemExpr) {
	START_LOG_EXPR_CONVERSION(vecElemExpr);
	core::ExpressionPtr&& base = Visit( vecElemExpr->getBase() );

	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(retIr);

	llvm::StringRef&& accessor = vecElemExpr->getAccessor().getName();

	core::TypePtr&& exprTy = convFact.convertType( GET_TYPE_PTR(vecElemExpr) );
	unsigned int pos = 0u;

	//translate OpenCL accessor string to index
	if ( accessor == "x" ) pos = 0u;
	else if ( accessor == "y" ) pos = 1u;
	else if ( accessor == "z" ) pos = 2u;
	else if ( accessor == "w" ) pos = 3u;
	else if ( (accessor.front() == 's' || accessor.front() == 'S') && accessor.size() == 2) {
		// the input string is in a form sXXX
		// we skip the s and return the value to get the number
		llvm::StringRef numStr = accessor.substr(1,accessor.size()-1);
		std::string posStr = numStr;

		if(posStr.at(0) <= '9')
		pos = posStr.at(0) - '0';
		else if(posStr.at(0) <= 'F')
		pos = (10 + posStr.at(0) - 'A');//convert A .. E to 10 .. 15
		else if(posStr.at(0) <= 'e')
		pos = (10 + posStr.at(0) - 'a');//convert a .. e to 10 .. 15
		else
		assert(posStr.at(0) <= 'e' && "Invalid vector accessing string");
	} else if ( accessor.size() <= 16 ) { // opencl vector permutation
		vector<core::ExpressionPtr> args;

		// expression using x, y, z and w
		auto acc = accessor.begin();
		if(*acc == 'S' || *acc == 's') { // expression using s0 .. sE
			++acc;// skip the s
			for ( auto I = acc, E = accessor.end(); I != E; ++I ) {
				if(*I <= '9')
				pos = *I - '0';
				else if(*I <= 'E')
				pos = (10 + (*I)-'A'); //convert A .. E to 10 .. 15
				else if(*I <= 'e')
				pos = (10 + (*I)-'a');//convert a .. e to 10 .. 15
				else
				assert(*I <= 'e' && "Unexpected accessor in ExtVectorElementExpr");

				args.push_back(builder.uintLit(pos));
			}
			return (retIr = builder.vectorPermute(convFact.tryDeref(base), builder.vectorExpr(args)) );
		} else {
			for ( auto I = acc, E = accessor.end(); I != E; ++I ) {
				args.push_back(builder.uintLit(*I == 'w' ? 3 : (*I)-'x')); //convert x, y, z, w to 0, 1, 2, 3
			}
			return (retIr = builder.vectorPermute(convFact.tryDeref(base), builder.vectorExpr(args)) );
		}

	} else {
		assert(accessor.size() <= 16 && "ExtVectorElementExpr has unknown format");
	}

	// The type of the index is always uint<4>
	core::ExpressionPtr&& idx = builder.uintLit(pos);
	// if the type of the vector is a refType, we deref it
	base = convFact.tryDeref(base);

	return (retIr = builder.callExpr(exprTy, gen.getVectorSubscript(), base, idx));
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//							VAR DECLARATION REFERENCE
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitDeclRefExpr(clang::DeclRefExpr* declRef) {
	START_LOG_EXPR_CONVERSION(declRef);

	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(retIr);

	// check whether this is a reference to a variable
	core::ExpressionPtr retExpr;
	if (ParmVarDecl* parmDecl = dyn_cast<ParmVarDecl>(declRef->getDecl())) {
		VLOG(2) << "Parameter type: " << convFact.convertType(parmDecl->getOriginalType().getTypePtr() );
		return ( retIr = convFact.lookUpVariable( parmDecl ) );
	}
	if ( VarDecl* varDecl = dyn_cast<VarDecl>(declRef->getDecl()) ) {

		retIr = convFact.lookUpVariable( varDecl );
		return retIr;
	}
	if( FunctionDecl* funcDecl = dyn_cast<FunctionDecl>(declRef->getDecl()) ) {
		return (retIr =
				core::static_pointer_cast<const core::Expression>(
						convFact.convertFunctionDecl(funcDecl)
				)
		);
	}
	if (EnumConstantDecl* enumDecl = dyn_cast<EnumConstantDecl>(declRef->getDecl() ) ) {
		return (retIr =
				builder.literal(
						enumDecl->getInitVal().toString(10),
						builder.getLangBasic().getInt4()
				)
		);
	}
	assert(false && "DeclRefExpr not supported!");
	return core::ExpressionPtr();
}

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
//                  VECTOR/STRUCT INITALIZATION EXPRESSION
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::ExprConverter::VisitInitListExpr(clang::InitListExpr* initList) {
	assert(false && "Visiting of initializer list is not allowed!"); return core::ExpressionPtr();
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
core::ExpressionPtr ConversionFactory::ExprConverter::VisitCompoundLiteralExpr(clang::CompoundLiteralExpr* compLitExpr) {
	START_LOG_EXPR_CONVERSION(compLitExpr);

	core::ExpressionPtr retIr;
	LOG_EXPR_CONVERSION(retIr);

	if ( clang::InitListExpr* initList =
			dyn_cast<clang::InitListExpr>(compLitExpr->getInitializer())
	) {
		return (retIr =
				convFact.convertInitExpr(
						NULL, 
						initList,
						convFact.convertType(compLitExpr->getType().getTypePtr()),
						false
				)
		);
	}
	return (retIr = Visit(compLitExpr->getInitializer()));
}


//---------------------------------------------------------------------------------------------------------------------
//										C EXPRESSION CONVERTER
//										calls Base: Expression Converter
//---------------------------------------------------------------------------------------------------------------------

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
// Overwrite the basic visit method for expression in order to automatically
// and transparently attach annotations to node which are annotated
//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
core::ExpressionPtr ConversionFactory::CExprConverter::Visit(clang::Expr* expr) {
	core::ExpressionPtr retIr = StmtVisitor<CExprConverter, core::ExpressionPtr>::Visit(expr);

	// check for OpenMP annotations
	return omp::attachOmpAnnotation(retIr, expr, convFact);
}

} // End conversion namespace
} // End frontend namespace
} // End insieme namespace
