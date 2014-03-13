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

#include "insieme/utils/logging.h"
#include "insieme/analysis/cba/analysis.h"
#include "insieme/core/ir_visitor.h"
#include "insieme/core/pattern/ir_pattern.h"
#include "insieme/core/pattern/pattern_utils.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/types/subtyping.h"
#include "insieme/core/transform/node_replacer.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/frontend/ocl/ocl_host_replace_buffers.h"
#include "insieme/frontend/ocl/ocl_host_utils1.h"

using namespace insieme::core;
using namespace insieme::core::pattern;

namespace insieme {
namespace frontend {
namespace ocl {

namespace {





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

BufferReplacer::BufferReplacer(NodePtr prog) : prog(prog) {
	collectInformation();
	generateReplacements();

	ExpressionMap em;
	for_each(clMemReplacements, [&](std::pair<core::ExpressionPtr, core::ExpressionPtr> replacement){
//		std::cout << "toll " << (replacement.first) << " -> " << (replacement.second) << std::endl;
		// use pointer in replacements to replace all occurences of the addressed expression
//		prog = transform::replaceAll(prog->getNodeManager(), prog, NodePtr(replacement.first), replacement.second, false);
		em[replacement.first] = replacement.second;
	});

	prog = transform::replaceVarsRecursive(prog->getNodeManager(), prog, em, false, transform::defaultTypeRecovery, id<StatementPtr>(), declInitReplacements);
	prog = transform::replaceAll(prog->getNodeManager(), prog, generalReplacements, false);

	printer::PrettyPrinter pp(prog);
//	std::cout << "\nPrETTy: \n" <<  pp << std::endl;
}

void BufferReplacer::collectInformation() {
	NodeManager& mgr = prog->getNodeManager();
	NodeAddress pA(prog);
	IRBuilder builder(mgr);

	TreePatternPtr clCreateBuffer = pattern::var("clCreateBuffer", irp::callExpr(pattern::any, irp::literal("clCreateBuffer"),
			pattern::any << pattern::var("flags", pattern::any) << pattern::var("size", pattern::any) <<
			pattern::var("host_ptr", pattern::any) << pattern::var("err", pattern::any) ));
	TreePatternPtr bufferDecl = pattern::var("type", irp::declarationStmt(pattern::var("buffer", pattern::any),
			irp::callExpr(pattern::any, irp::atom(mgr.getLangBasic().getRefVar()), pattern::single(clCreateBuffer))));
	TreePatternPtr bufferAssign = irp::callExpr(pattern::any, pattern::var("type", irp::atom(mgr.getLangBasic().getRefAssign())),
			pattern::var("buffer", pattern::any) << clCreateBuffer);
	TreePatternPtr bufferPattern = pattern::var("all", bufferDecl | bufferAssign);
/*
	visitDepthFirst(pA, [&](const NodeAddress& node) {
		AddressMatchOpt createBuffer = bufferPattern->matchAddress(node);

		if(createBuffer) {
			std::cout << "All: " << createBuffer->getVarBinding("all").getValue() << std::endl;
		}
	});
*/

	irp::matchAllPairs(bufferPattern, pA, [&](const NodeAddress& matchAddress, const AddressMatch& createBuffer) {

//std::cout << "All: " << createBuffer["all"].getValue() << std::endl;
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
		if (core::CallExprPtr x = hostPtr.isa<core::CallExprPtr>()){
			if (mgr.getLangBasic().isRefReinterpret(x->getFunctionExpr()))
				hostPtr = x[0];
		}



		// extract type form size argument
		ExpressionPtr size;
		TypePtr type;
		__unused bool check = utils::extractSizeFromSizeof(createBuffer["size"].getValue().as<ExpressionPtr>(), size, type, false);

		assert(check && "cannot extract size and type from size paramater fo clCreateBuffer");

// 			std::cout << "\nyipieaiey: " << lhs << std::endl << std::endl;


		ExpressionPtr deviceMemAlloc = usePtr ? hostPtr :
				getCreateBuffer(type, size, copyPtr, hostPtr, createBuffer["err"].getValue().as<ExpressionPtr>());
		generalReplacements[createBuffer["clCreateBuffer"].getValue()] = deviceMemAlloc;

		// get the buffer expression address relative to root node of the pattern query
		ExpressionAddress lhs = matchAddress >> createBuffer["buffer"].getValue().as<ExpressionAddress>();
//std::cout << "\nvariable " << *lhs << std::endl;
		// generate new init expression in  case of declaration
		ExpressionPtr initExpr;
		if(createBuffer["type"].getValue().isa<DeclarationStmtAddress>()) {
			initExpr = builder.refVar(deviceMemAlloc);
//std::cout << "Init: " << *initExpr << std::endl;
		}

		// add gathered information to clMemMetaMap
		clMemMeta[lhs] = ClMemMetaInfo(size, type, flags, initExpr);
	});

}

bool BufferReplacer::alreadyThereAndCorrect(ExpressionAddress& bufferExpr, const TypePtr& newType) {
	bool correct = false;

	// check if there is already a replacement for the current expression (or an alias of it) with a different type
//std::cout << "\nnewTy " << *bufferExpr << std::endl;
	for_each(clMemReplacements, [&](std::pair<ExpressionAddress, ExpressionPtr> replacement) {
//std::cout << "\tchecking " << *replacement.first << std::endl;
		if(*replacement.first == *bufferExpr /*|| analysis::cba::isAlias(replacement.first, bufferExpr)*/) {
			ExpressionPtr repExpr = replacement.second;
			if(*repExpr->getType() != *newType) {
/* FORBID subtypes, too complicated in terms of size to allocate and memory layout anyway
				if(types::isSubTypeOf(repExpr->getType(), newType)) { // if the new type is subtype of the current one, replace the type
					//newType = replacement.second->getType();
					bufferExpr = replacement.first; // do not add aliases to the replacement map
				} else if(types::isSubTypeOf(newType, repExpr->getType())) { // if the current type is subtype of the new type, do nothing
					correct = true;
					return;
				} else*/ // if the types are not related, fail
					assert(false && "Buffer used twice with different types. Not supported by Insieme.");
			} else
				correct = true;
		}
	});

	return false;
}


void BufferReplacer::generateReplacements() {
	NodeManager& mgr = prog.getNodeManager();
	IRBuilder builder(mgr);

	TypePtr clMemTy;
	TreePatternPtr subscriptPattern = irp::subscript1D("operation",
			pattern::var("variable", irp::variable()) | irp::callExpr(pattern::any, pattern::var("variable", irp::variable())));

	for_each(clMemMeta, [&](std::pair<ExpressionAddress, ClMemMetaInfo> meta) {
		ExpressionAddress bufferExpr = utils::extractVariable(meta.first);
//std::cout << "\nto replace: " << *bufferExpr << std::endl;
		visitDepthFirst(ExpressionPtr(bufferExpr)->getType(), [&](const NodePtr& node) {
		//if(node.toString().compare("_cl_mem") == 0) std::cout << "\nGOCHA" << node << "  " << node.getNodeType() << "\n";
			if(GenericTypePtr gt = dynamic_pointer_cast<const GenericType>(node)) {
				if(gt.toString().compare("_cl_mem") == 0)
					clMemTy = gt;
			}
		}, true, true);

//std::cout << NodePtr(meta.first) << " "  << meta.second.type << std::endl;


		AddressMatchOpt subscript = subscriptPattern->matchAddress(bufferExpr);
		if(subscript) {
//			std::cout << "MATCH: " << *(subscript.get()["operation"].getValue()) << std::endl;
			ExpressionAddress expr = dynamic_address_cast<const Expression>(subscript.get()["variable"].getValue());
			TypePtr newArrType = transform::replaceAll(mgr, expr->getType(), clMemTy, meta.second.type).as<TypePtr>();

//std::cout << "arr: " << expr << " root " << getRootVariable(expr) << std::endl;
			expr = utils::getRootVariable(expr).as<ExpressionAddress>();

			if(alreadyThereAndCorrect(expr, newArrType)) return;

//std::cout << "\nall: " << *expr << "\n" << newArrType << std::endl;
			clMemReplacements[expr] = builder.variable(newArrType);
			return;
		}

		const TypePtr newType = transform::replaceAll(mgr, meta.first->getType(), clMemTy, meta.second.type).as<TypePtr>();

//std::cout << "var: " << bufferExpr << " root " << getRootVariable(bufferExpr) << std::endl;
		bufferExpr = utils::getRootVariable(bufferExpr).as<ExpressionAddress>();

		if(alreadyThereAndCorrect(bufferExpr, newType)) return;

		// local variable case
		if(VariableAddress variable = dynamic_address_cast<const Variable>(bufferExpr)) {

			VariablePtr newBuffer = builder.variable(newType);
			clMemReplacements[variable] = newBuffer;

			// if clCreateBuffer was called at initialization, update it now
			if(meta.second.initExpr) {


				declInitReplacements[newBuffer] = meta.second.initExpr;

std::cout << "initializing " << *newBuffer << " with ";
dumpPretty(meta.second.initExpr);
std::cout << std::endl;
			}
			return;
		}
		// global variable case
		if(LiteralAddress lit = dynamic_address_cast<const Literal>(bufferExpr)) {
			clMemReplacements[lit] = builder.literal(newType, "newLit");//lit->getStringValue());
			return;
		}
	});


	for_each(clMemReplacements, [&](std::pair<NodePtr, ExpressionPtr> replacement) {
		std::cout << printer::PrettyPrinter(replacement.first) << " -> " << printer::PrettyPrinter(replacement.second) << std::endl;
	});

}

} //namespace ocl
} //namespace frontend
} //namespace insieme

