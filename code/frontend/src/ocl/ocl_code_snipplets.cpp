/**
 * Copyright (c) 2002-2015 Distributed and Parallel Systems Group,
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

#include "insieme/core/parser3/ir_parser.h"

#include "insieme/frontend/ocl/ocl_code_snipplets.h"

namespace insieme {
namespace frontend {
namespace ocl {

using namespace insieme::core;

ExpressionPtr getConvert(unsigned length, IRBuilder builder) {
	std::string irCode = format(
		"lambda (vector<'a,%i> fromVec, type<'b> toElemTy) -> vector<'b,%i> { "
		"	decl ref<vector<'b,%i> > toVec = var( undefined(vector<'b,%i>) );"
			""
			"for(uint<8> i = 0u .. %iu ) "
			"	toVec[i] = CAST('b) fromVec[i]; "
			""
			"return *toVec; "
		"}", length, length, length, length, length, length);

	return builder.parseExpr(irCode);
}



bool Ocl2Inspire::extractSizeFromSizeof(const core::ExpressionPtr& arg, core::ExpressionPtr& size, core::TypePtr& type, bool foundMul) {
	core::NodeManager& mgr = arg.getNodeManager();
	core::IRBuilder builder(mgr);

	// get rid of casts
	NodePtr uncasted = arg;
	while (uncasted->getNodeType() == core::NT_CastExpr) {
		uncasted = static_pointer_cast<CastExprPtr>(uncasted)->getType();
	}

	if (const CallExprPtr call = dynamic_pointer_cast<const CallExpr> (uncasted)) {
		// check if there is a multiplication
		if(call->getFunctionExpr()->toString().find(".mul") != string::npos && call->getArguments().size() == 2) {
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
			assert_true(type) << "Type could not be extracted!";

			if(!foundMul){ // no multiplication, just sizeof alone is passed as argument -> only one element
				size = builder.literal(mgr.getLangBasic().getUInt8(), "1");
				return true;
			}

			return true;
		}
	}
	return false;
}

ExpressionPtr Ocl2Inspire::getClCreateBuffer(bool copyHostPtr, bool setErrcodeRet, IRBuilder builder) {
	// read/write flags ignored
	// errcorcode always set to 0 = CL_SUCCESS for clCreatBuffer and ignored for icl_create_buffer

	std::string returnErrorcode = setErrcodeRet ? "		errorcode_ret[0u] = 0; " : "";

	if (copyHostPtr)
		return builder.parseExpr( R"(
		lambda (
			type<'a> 				elemType, 
			uint<8> 				size, 
			ref<any> 				hostPtr, 
			ref<array<int<4>,1> > 	errorcode_ret
		) -> ref<array<'a, 1> >  { 
				decl ref<array<'a,1>> devicePtr = new( array_create_1D( elemType, size ) ); 
				decl ref<array<'a,1>> 		hp = ref_reinterpret(hostPtr, lit(array<'a,1>)); 
				for(uint<8> i = 0u .. size) { 
					devicePtr[i] = *(hp[i]); 
				} )"
		                + returnErrorcode +
         R"(
				return devicePtr; 
	 	})");

	return builder.parseExpr(R"(
		lambda ( 
			type<'a>				elemType, 
			uint<8> 				size, 
			ref<array<int<4>, 1> >  errorcode_ret
		) -> ref<array<'a, 1> > { )"
		+ returnErrorcode +
        R"(
			return new( array_create_1D( elemType, size )); 
       	})");
}

ExpressionPtr Ocl2Inspire::getClCopyBuffer(IRBuilder builder) {
	// event stuff removed
	// always returns 0 = CL_SUCCESS
	return builder.parseExpr(R"(
		lambda ( 
			ref<array<'a, 1> > srcBuffer, 
			 ref<array<'a, 1> > dstBuffer, 
			 uint<8>      		srcOffset, 
			 uint<8> 			dstOffset, 
			 uint<8> 			cb
		) -> int<4> {
			decl uint<8> do = dstOffset / sizeof( lit('a)); 
			decl uint<8> so = srcOffset / sizeof( lit('a)); 
           for(uint<8> i = 0u .. cb) { 
				dstBuffer[i + do] = *(srcBuffer[i + so]); 
			}
			return 0; 
    	})");
}

ExpressionPtr Ocl2Inspire::getClCopyBufferFallback(core::IRBuilder builder) {
	// event stuff removed
	// always returns 0 = CL_SUCCESS
	return builder.parseExpr(R"(
		lambda (
			ref<array<'a, 1> >	srcBuffer, 
			ref<array<'a, 1> > 	dstBuffer, 
			uint<8>				srcOffset, 
			uint<8>				dstOffset, 
			uint<8>				cb
		) -> int<4> { 
			decl uint<8> size = cb / sizeof( lit('a) ); 
			decl uint<8> do = dstOffset / sizeof( lit('a)); 
			decl uint<8> so = srcOffset / sizeof( lit('a)); 
			for(uint<8> i = 0u .. size) { 
				dstBuffer[i + dstOffset] = *(srcBuffer[i + dstOffset]); 
			}
			return 0; 
		})");
}

ExpressionPtr Ocl2Inspire::getClWriteBuffer(core::IRBuilder builder) {
	// blocking_write ignored
	// event stuff removed
	// always returns 0 = CL_SUCCESS
	return builder.parseExpr( R"(
		lambda (
			ref<array<'a, 1> > 	devicePtr, 
			uint<4>				blocking_write, 
			uint<8>				offset, 
			uint<8>				cb, 
			ref<any>			hostPtr
		) -> int<4> { 
			decl ref<array<'a,1>> hp = ref_reinterpret(hostPtr, lit(array<'a, 1>)); 
			decl uint<8> 	  	  o = offset / sizeof( lit('a) ); 
			for(uint<8> i = 0u .. cb) { 
				devicePtr[i + o] = *(hp[i]); 
			}
			return 0; 
    	})");
}

ExpressionPtr Ocl2Inspire::getClWriteBufferFallback(core::IRBuilder builder) {
	// blocking_write ignored
	// event stuff removed
	// always returns 0 = CL_SUCCESS
	return builder.parseExpr( R"(
		lambda (
			ref<array<'a, 1>> 	devicePtr, 
			uint<4> 			blocking_write, 
			uint<8>				offset, 
			uint<8>				cb, 
			ref<any>			hostPtr
		) -> int<4> { 
			decl ref<array<'a,1>> hp = ref_reinterpret(hostPtr, lit(array<'a,1>)); 
			decl uint<8> 		  o = offset / sizeof( lit('a) ); 
        	decl uint<8> 	   size = cb / sizeof( lit('a) ); 
        	for(uint<8> i = 0u .. size) { 
        		devicePtr[i + o] = *(hp[i]); 
			}
        	return 0; 
    	})");
}

ExpressionPtr Ocl2Inspire::getClReadBuffer(core::IRBuilder builder) {
	// blocking_write ignored
	// event stuff removed
	// always returns 0 = CL_SUCCESS
	return builder.parseExpr(R"(
		lambda (
			ref<array<'a,1>> 	devicePtr, 
			uint<4> 			blocking_read, 
			uint<8>				offset, 
			uint<8>				cb, 
			ref<any> 			hostPtr
		) -> int<4> { 
			decl ref<array<'a,1>> hp = ref_reinterpret(hostPtr, lit(array<'a,1>)); 
			decl uint<8>		  o = offset / sizeof( lit('a) ); 
			for(uint<8> i = 0u .. cb) { 
				hp[i] = *(devicePtr[i + o]); 
			}
        	return 0; 
 	   	})");
}

ExpressionPtr Ocl2Inspire::getClReadBufferFallback(core::IRBuilder builder) {
	// blocking_write ignored
	// event stuff removed
	// always returns 0 = CL_SUCCESS
	return builder.parseExpr(R"(
		lambda (
			ref<array<'a, 1> > 	devicePtr, 
			uint<4> 			blocking_read, 
			uint<8> 			offset, 
			uint<8> 			cb, 
			ref<any> 			hostPtr
		) -> int<4> { 
            decl ref<array<'a, 1> > hp = ref_reinterpret(hostPtr, lit(array<'a,1>)); 
			decl uint<8> 		 size = cb / sizeof( lit('a) ); 
			decl uint<8> 			o = offset / sizeof( lit('a) ); 
			for(uint<8> i = 0u .. size) { 
				hp[i] = *(devicePtr[i + o]); 
			} 
        	return 0; 
    	})");
}

ExpressionPtr Ocl2Inspire::getClGetIDs(core::IRBuilder builder) {
	// does simply set the number of devices to 1 and returns 0 = CL_SUCCESS
	// TODO add functionality
	return builder.parseExpr(
		"lambda (ref<array<uint<4>,1>> num_devices) -> int<4> { "
		"	num_devices[0u] = 1u; "
		"	return 0; "
		"}");
}


}
}
}
