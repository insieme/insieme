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

#include "insieme/frontend/ocl/ocl_host_replace_buffers.h"
#include "insieme/utils/logging.h"
#include "insieme/analysis/cba/analysis.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/subtyping.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/printer/pretty_printer.h"

using namespace insieme::core;
using namespace insieme::core::pattern;

namespace insieme {
namespace frontend {
namespace ocl {

namespace {
bool extractSizeFromSizeof(const core::ExpressionPtr& arg, core::ExpressionPtr& size, core::TypePtr& type, bool foundMul) {
	// get rid of casts
	NodePtr uncasted = arg;
	while (uncasted->getNodeType() == core::NT_CastExpr) {
		uncasted = static_pointer_cast<CastExprPtr>(uncasted)->getType();
	}

	if (const CallExprPtr call = dynamic_pointer_cast<const CallExpr> (uncasted)) {
		// check if there is a multiplication
		if(call->getFunctionExpr()->toString().find(".mul") != string::npos && call->getArguments().size() == 2) {
			IRBuilder builder(arg->getNodeManager());
			// recursively look into arguments of multiplication
			if(extractSizeFromSizeof(call->getArgument(0), size, type, true)) {
				if(size)
					size = builder.callExpr(call->getType(), call->getFunctionExpr(), size, call->getArgument(1));
				else
					size = call->getArgument(1);
				return true;
			}
			if(extractSizeFromSizeof(call->getArgument(1), size, type, true)){
				if(size)
					size = builder.callExpr(call->getType(), call->getFunctionExpr(), call->getArgument(0), size);
				else
					size = call->getArgument(0);
				return true;
			}
		}
		// check if we reached a sizeof call
		if (call->toString().substr(0, 6).find("sizeof") != string::npos) {
			// extract the type to be allocated
			type = dynamic_pointer_cast<GenericTypePtr>(call->getArgument(0)->getType())->getTypeParameter(0);
			assert(type && "Type could not be extracted!");

			if(!foundMul){ // no multiplication, just sizeof alone is passed as argument -> only one element
				IRBuilder builder(arg->getNodeManager());
				size = builder.literal(arg->getNodeManager().getLangBasic().getUInt8(), "1");
				return true;
			}

			return true;
		}
	}
	return false;
}


template<typename Enum>
void recursiveFlagCheck(const NodePtr& flagExpr, std::set<Enum>& flags) {
	if (const CallExprPtr call = dynamic_pointer_cast<const CallExpr>(flagExpr)) {
		const lang::BasicGenerator& gen = flagExpr->getNodeManagerPtr()->getLangBasic();
		// check if there is an lshift -> flag reached
		if (call->getFunctionExpr() == gen.getGenLShift() ||
					call->getFunctionExpr() == gen.getSignedIntLShift() || call->getFunctionExpr() == gen.getUnsignedIntLShift()) {
			if (const LiteralPtr flagLit = dynamic_pointer_cast<const Literal>(call->getArgument(1))) {
				int flag = flagLit->getValueAs<int>();
				if (flag < Enum::size) // last field of enum to be used must be size
					flags.insert(Enum(flag));
				else
					LOG(ERROR) << "Flag " << flag << " is out of range. Max value is " << CreateBufferFlags::size - 1;
			}
		} else if (call->getFunctionExpr() == gen.getGenOr() ||
				call->getFunctionExpr() == gen.getSignedIntOr() || call->getFunctionExpr() == gen.getUnsignedIntOr()) {
			// two flags are ored, search flags in the arguments
			recursiveFlagCheck(call->getArgument(0), flags);
			recursiveFlagCheck(call->getArgument(1), flags);
		} else
			LOG(ERROR) << "Unexpected operation in flag argument: " << call->getFunctionExpr() << "\nUnable to deduce flags, using default settings";

	}
}

template<typename Enum>
std::set<Enum> getFlags(const NodePtr& flagExpr) {

	const lang::BasicGenerator& gen = flagExpr->getNodeManagerPtr()->getLangBasic();
	std::set<Enum> flags;
	// remove cast to uint<8>
	if (flagExpr.isa<CallExprPtr>() && gen.isScalarCast(flagExpr.as<CallExprPtr>()->getFunctionExpr())){
		recursiveFlagCheck(flagExpr.as<CallExprPtr>()->getArgument(0), flags);
	}else
	if (const CastExprPtr cast = dynamic_pointer_cast<const CastExpr>(flagExpr)) {
		recursiveFlagCheck(cast->getSubExpression(), flags);
	} else
		LOG(ERROR) << "No flags found in " << flagExpr << "\nUsing default settings";
	return flags;
}

ExpressionAddress extractVariable(ExpressionAddress expr) {
	const lang::BasicGenerator& gen = expr->getNodeManagerPtr()->getLangBasic();

	if(expr->getNodeType() == NT_Variable) // return variable
		return expr;

	if(expr->getNodeType() == NT_Literal) // rreturn literal, e.g. global varialbe
		return expr;

	if(CallExprAddress call = dynamic_address_cast<const CallExpr>(expr)) {
		if(gen.isSubscriptOperator(call->getFunctionExpr()))
			return expr;

		if(gen.isCompositeRefElem(call->getFunctionExpr())) {
			return expr;
		}
	}

	return expr;
}


ExpressionPtr getClCreateBuffer(bool copyHostPtr, bool setErrcodeRet, IRBuilder builder) {
	// read/write flags ignored
	// errcorcode always set to 0 = CL_SUCCESS for clCreatBuffer and ignored for icl_create_buffer

	std::string returnErrorcode = setErrcodeRet ? "		errorcode_ret[0u] = 0; " : "";

	if (copyHostPtr)
		return builder.parseExpr(
		"("
		"	type<'a> 				elemType, "
		"	uint<8> 				size, "
		"	ref<any> 				hostPtr, "
		"	ref<array<int<4>,1> > 	errorcode_ret"
		") -> ref<array<'a, 1> >  { "
		"		ref<array<'a,1>> devicePtr = new( array.create.1D( elemType, size ) ); "
		"		ref<array<'a,1>> 		hp = ref.reinterpret(hostPtr, lit(array<'a,1>)); "
		"		for(uint<8> i = 0u .. size) { "
		"			devicePtr[i] = *hp[i]; "
		"		} "
		+ returnErrorcode +
		"		return devicePtr; "
	 	"}");

	return builder.parseExpr(
		"( "
		"	type<'a>				elemType, "
		"	uint<8> 				size, "
		"	ref<array<int<4>, 1> >  errorcode_ret"
		") -> ref<array<'a, 1> > { "
		+ returnErrorcode +
		"	return new( array.create.1D( elemType, size )); "
       	"}");
}

ExpressionPtr getCreateBuffer(const TypePtr& type, const ExpressionPtr& size, const bool copyPtr,
		const ExpressionPtr& hostPtr, const ExpressionPtr& errcode_ret) {
	NodeManager& mgr = size->getNodeManager();
	IRBuilder builder(mgr);
	ExpressionPtr fun = getClCreateBuffer(copyPtr, errcode_ret == builder.getTypeLiteral(builder.arrayType(mgr.getLangBasic().getInt4())), builder);

	vector<ExpressionPtr> args;
	args.push_back(builder.getTypeLiteral(type));
	args.push_back(size);
	if(copyPtr) args.push_back(hostPtr);
	args.push_back(errcode_ret);
	return builder.callExpr(builder.refType(builder.arrayType(type)), fun, args);
}

}

const NodePtr BufferMapper::resolveElement(const NodePtr& ptr) {
	return ptr;
}

BufferReplacer::BufferReplacer(ProgramPtr& prog) : prog(prog) {
	collectInformation();
	generateReplacements();
/*
	for_each(clMemReplacements, [prog](std::pair<core::NodePtr, core::NodePtr> replacement){
		std::cout << "toll " << (replacement.first) << " -> " << (replacement.second) << std::endl;
//		transform::replaceAll(prog->getNodeManager(), prog->getElement(0), replacement.first, replacement.second, false);
	});
*/
	NodePtr newProg = transform::replaceAll(prog->getNodeManager(), prog->getElement(0), clMemReplacements, false);
	newProg = transform::replaceAll(prog->getNodeManager(), newProg, generalReplacements, false);

//	printer::PrettyPrinter pp(newProg);
//	std::cout << "\nPrETTy: \n" <<  pp << std::endl;
}

void BufferReplacer::collectInformation() {
	NodeManager& mgr = prog->getNodeManager();
	NodeAddress pA(prog->getChild(0));
	IRBuilder builder(mgr);

	TreePatternPtr clCreateBuffer = var("clCreateBuffer", irp::callExpr(pattern::any, irp::literal("clCreateBuffer"),
			pattern::any << var("flags", pattern::any) << var("size", pattern::any) << var("host_ptr", pattern::any) << var("err", pattern::any) ));
	TreePatternPtr bufferDecl = var("type", irp::declarationStmt(var("buffer", pattern::any), irp::callExpr(pattern::any, irp::atom(mgr.getLangBasic().getRefVar()),
			pattern::single(clCreateBuffer))));
	TreePatternPtr bufferAssign = irp::callExpr(pattern::any, var("type", irp::atom(mgr.getLangBasic().getRefAssign())),
			var("buffer", pattern::any) << clCreateBuffer);
	TreePatternPtr bufferPattern = bufferDecl | bufferAssign;

	irp::matchAll(bufferPattern, pA, [&](const AddressMatch& createBuffer) {


//	visitDepthFirst(pA, [&](const NodeAddress& node) {
//		AddressMatchOpt createBuffer = bufferPattern->matchAddress(node);

//		if(createBuffer) {
		NodePtr flagArg = createBuffer["flags"].getValue();

		std::set<enum CreateBufferFlags> flags = getFlags<enum CreateBufferFlags>(flagArg);
		// check if CL_MEM_USE_HOST_PTR is set
		bool usePtr = flags.find(CreateBufferFlags::CL_MEM_USE_HOST_PTR) != flags.end();
		// check if CL_MEM_COPY_HOST_PTR is set
		bool copyPtr = flags.find(CreateBufferFlags::CL_MEM_COPY_HOST_PTR) != flags.end();

		ExpressionPtr hostPtr = createBuffer["host_ptr"].getValue().as<ExpressionPtr>();
		if(CastExprPtr c = dynamic_pointer_cast<const CastExpr>(hostPtr)) {
			assert(!copyPtr && "When CL_MEM_COPY_HOST_PTR is set, host_ptr parameter must be a valid pointer");
			if(c->getSubExpression()->getType() != mgr.getLangBasic().getAnyRef()) {// a scalar (probably NULL) has been passed as hostPtr arg
				hostPtr = builder.callExpr(mgr.getLangBasic().getRefVar(), c->getSubExpression());
			}
		}


		// extract type form size argument
		ExpressionPtr size;
		TypePtr type;
#ifdef	NDEBUG
		extractSizeFromSizeof(createBuffer["size"].getValue().as<ExpressionPtr>(), size, type, false);
#else
		assert(extractSizeFromSizeof(createBuffer["size"].getValue().as<ExpressionPtr>(), size, type, false)
				&& "cannot extract size and type from size paramater fo clCreateBuffer");
#endif

// 			std::cout << "\nyipieaiey: " << lhs << std::endl << std::endl;


		ExpressionPtr deviceMemAlloc = usePtr ? hostPtr :
				getCreateBuffer(type, size, copyPtr, hostPtr, createBuffer["err"].getValue().as<ExpressionPtr>());
		generalReplacements[createBuffer["clCreateBuffer"].getValue()] = deviceMemAlloc;

		// get the buffer expression
		ExpressionAddress lhs = createBuffer["buffer"].getValue().as<ExpressionAddress>();

		// add gathered information to clMemMetaMap
		clMemMeta[lhs] = ClMemMetaInfo(size, type, flags, hostPtr);
	});

}

bool BufferReplacer::alreadyThereAndCorrect(ExpressionPtr& bufferExpr, const TypePtr& newType) {
	bool alreadyThereAndCorrect = false;

	// check if there is already a replacement for the current expression (or an alias of it) with a different type
//				std::cout << "\nnewTy " << *bufferExpr << std::endl;
	for_each(clMemReplacements, [&](std::pair<NodePtr, NodePtr> replacement) {
//				std::cout << "repty " << *replacement.first << std::endl;
		if(*replacement.first == *bufferExpr){// || analysis::cba::isAlias(replacement.first, bufferExpr)) {
			ExpressionPtr repExpr = replacement.second.as<ExpressionPtr>();
			if(*repExpr->getType() != *newType) {
				if(types::isSubTypeOf(repExpr->getType(), newType)) { // if the new type is subtype of the current one, replace the type
					//newType = replacement.second->getType();
					bufferExpr = replacement.first.as<ExpressionPtr>(); // do not add aliases to the replacement map
				} else if(types::isSubTypeOf(newType, repExpr->getType())) { // if the current type is subtype of the new type, do nothing
					alreadyThereAndCorrect = true;
					return;
				} else // if the types are not related, fail
					assert(false && "Buffer used twice with different types. Not supported by Insieme.");
			} else
				alreadyThereAndCorrect = true;
		}
	});

	return false;
}


void BufferReplacer::generateReplacements() {
	NodeManager& mgr = prog.getNodeManager();
	IRBuilder builder(mgr);

	TypePtr clMemTy;

	for_each(clMemMeta, [&](std::pair<ExpressionAddress, ClMemMetaInfo> meta) {
		ExpressionPtr bufferExpr = extractVariable(meta.first);
		visitDepthFirst(ExpressionPtr(bufferExpr)->getType(), [&](const NodePtr& node) {
	//			std::cout << node << std::endl;
		//if(node.toString().compare("_cl_mem") == 0) std::cout << "\nGOCHA" << node << "  " << node.getNodeType() << "\n";
			if(GenericTypePtr gt = dynamic_pointer_cast<const GenericType>(node)) {
				if(gt.toString().compare("_cl_mem") == 0)
					clMemTy = gt;
			}
		}, true, true);

//std::cout << NodePtr(meta.first) << " "  << meta.second.type << std::endl;

		const TypePtr newType = transform::replaceAll(mgr, meta.first->getType(), clMemTy, meta.second.type).as<TypePtr>();

		if(alreadyThereAndCorrect(bufferExpr, newType)) return;

		// local variable case
		if(VariablePtr variable = dynamic_pointer_cast<const Variable>(bufferExpr)) {
			clMemReplacements[variable] = builder.variable(newType);
			return;
		}
		// global variable case
		if(LiteralPtr lit = dynamic_pointer_cast<const Literal>(bufferExpr)) {
			clMemReplacements[lit] = builder.literal(newType, lit->getStringValue());
			return;
		}

		// try to extract the variable
//		TreePatternPtr subscriptPattern = irp::subscript1D("operation", aT(var("variable", irp::variable())), aT(var("idx", pattern::any)));
		TreePatternPtr subscriptPattern = irp::subscript1D("operation",
				var("variable", irp::variable()) | irp::callExpr(pattern::any, var("variable", irp::variable())));

		MatchOpt subscript = subscriptPattern->matchPointer(bufferExpr);

		if(subscript) {
//			std::cout << "MATCH: " << *(subscript.get()["operation"].getValue()) << std::endl;
			ExpressionPtr expr = dynamic_pointer_cast<const Expression>(subscript.get()["variable"].getValue());
			TypePtr newArrType = transform::replaceAll(mgr, expr->getType(), clMemTy, meta.second.type).as<TypePtr>();
			if(alreadyThereAndCorrect(expr, newArrType)) return;

//std::cout << "\nall: " << *expr << "\n" << newArrType << std::endl;
			clMemReplacements[expr] = builder.variable(newArrType);
			return;
		}
	});
/*
	for_each(clMemReplacements, [&](std::pair<NodePtr, NodePtr> replacement) {
		std::cout << printer::PrettyPrinter(replacement.first) << " -> " << printer::PrettyPrinter(replacement.second) << std::endl;
	});
*/
}

} //namespace ocl
} //namespace frontend
} //namespace insieme

