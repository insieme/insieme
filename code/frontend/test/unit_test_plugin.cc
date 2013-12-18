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

//#define __STDC_LIMIT_MACROS
//#define __STDC_CONSTANT_MACROS
//
//#pragma GCC diagnostic push
//#pragma GCC diagnostic ignored "-Wstrict-aliasing"
//#include "clang/AST/Decl.h"
//#include <clang/AST/Stmt.h>
//#include <clang/AST/Type.h>
//#include <clang/AST/Decl.h>
//#pragma GCC diagnostic pop
#include "insieme/frontend/clang.h"

#include <gtest/gtest.h>
#include "insieme/utils/test/test_utils.h"
#include "test_utils.inc"

#include "insieme/core/ir_program.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"

#include "insieme/core/pattern/ir_pattern.h"

#include "insieme/core/lang/complex_extension.h"
#include "insieme/core/lang/simd_vector.h"
#include "insieme/core/lang/enum_extension.h"

#include "insieme/frontend/convert.h"
#include "insieme/frontend/tu/ir_translation_unit.h"
#include "insieme/frontend/extensions/frontend_plugin.h"
#include "insieme/frontend/extensions/cpp11_extension.h"

using namespace insieme::core;
using namespace insieme::core::checks;
using namespace insieme::utils::log;
namespace fe = insieme::frontend;

	


class UnitTestBuiltinTypes : public insieme::frontend::extensions::FrontendPlugin {

	insieme::core::TypePtr PostVisit(const clang::Type* type, const insieme::core::TypePtr& irType, insieme::frontend::conversion::Converter& convFact) {
		EXPECT_TRUE(irType);

		if(const clang::BuiltinType* builtin = llvm::dyn_cast<clang::BuiltinType>(type) ) {
			switch(builtin->getKind()) {
				case clang::BuiltinType::Void:			EXPECT_EQ("unit", toString(*irType)); break;
				case clang::BuiltinType::Bool:			EXPECT_EQ("bool", toString(*irType)); break;

				// char types
				case clang::BuiltinType::Char_U:		EXPECT_EQ("uint<1>", toString(*irType)); break;
				case clang::BuiltinType::UChar:			EXPECT_EQ("uint<1>", toString(*irType)); break;
				case clang::BuiltinType::Char16:		EXPECT_EQ("wchar<16>", toString(*irType)); break;
				case clang::BuiltinType::Char32:		EXPECT_EQ("wchar<32>", toString(*irType)); break;
				case clang::BuiltinType::Char_S:		EXPECT_EQ("char", toString(*irType)); break; 
				case clang::BuiltinType::SChar:			EXPECT_EQ("char", toString(*irType)); break;
				case clang::BuiltinType::WChar_S:		EXPECT_EQ("wchar<32>", toString(*irType)); break;
				case clang::BuiltinType::WChar_U:		EXPECT_EQ("wchar<32>", toString(*irType)); break;

				// integer types
				case clang::BuiltinType::UShort:		EXPECT_EQ("uint<2>", toString(*irType)); break;
				case clang::BuiltinType::Short:			EXPECT_EQ("int<2>", toString(*irType)); break;
				case clang::BuiltinType::UInt:			EXPECT_EQ("uint<4>", toString(*irType)); break;
				case clang::BuiltinType::Int:			EXPECT_EQ("int<4>", toString(*irType)); break;
				case clang::BuiltinType::UInt128:		EXPECT_EQ("uint<16>", toString(*irType)); break;
				case clang::BuiltinType::Int128:		EXPECT_EQ("int<16>", toString(*irType)); break;
				case clang::BuiltinType::ULong:			EXPECT_EQ("uint<8>", toString(*irType)); break;
				case clang::BuiltinType::Long:			EXPECT_EQ("int<8>", toString(*irType)); break;

												// long long is packed in a struct to avoid aliases with just long
				case clang::BuiltinType::LongLong:		EXPECT_EQ("struct<longlong_val:int<8>>", toString(*irType)); break;
				case clang::BuiltinType::ULongLong:		EXPECT_EQ("struct<longlong_val:uint<8>>", toString(*irType)); break;

				// real types
				case clang::BuiltinType::Float:			EXPECT_EQ("real<4>", toString(*irType)); break;
				case clang::BuiltinType::Double:		EXPECT_EQ("real<8>", toString(*irType)); break;
				case clang::BuiltinType::LongDouble:	EXPECT_EQ("real<16>", toString(*irType)); break;

				// not supported types
				case clang::BuiltinType::NullPtr:		EXPECT_EQ("'nullptr_t", toString(*irType)); break;

				default:
					//ASSERT_TRUE(false) << "fix the builtintype unit test";
					throw "type not supported"; //todo introduce exception class
			}
		}

		//if(const clang::ComplexType* complexType = llvm::dyn_cast<clang::ComplexType>(type)) {
		if(llvm::isa<clang::ComplexType>(type)) {
			//check if it is a IR complexType
			EXPECT_TRUE(convFact.getIRBuilder().getNodeManager().getLangExtension<insieme::core::lang::ComplexExtension>().isComplexType(irType));
		}
	
		if(llvm::isa<clang::ArrayType>(type)) {
			if(const clang::ConstantArrayType* constArrType = llvm::dyn_cast<clang::ConstantArrayType>(type)) {
				//ConstantArray turns into vector<int<4>, CONST_SIZE>
				
				//check for vector type
				EXPECT_TRUE(irType.isa<VectorTypePtr>());

				//checking elementType
				VectorTypePtr vecType = irType.as<VectorTypePtr>();
				TypePtr&& elemTy = convFact.convertType( constArrType->getElementType().getTypePtr() );
				EXPECT_TRUE( *elemTy == *vecType.getElementType());

				//checking size
				size_t arrSize = *constArrType->getSize().getRawData();
				EXPECT_EQ( toString(*vecType.getSize()),toString(arrSize));
			} 

			/*TODO how to get this clang type node?
			if(const clang::IncompleteArrayType* incArrType = llvm::dyn_cast<clang::IncompleteArrayType>(type)) {
				//array<int<4>,1>
			}
			*/

			if(const clang::VariableArrayType* varArrType = llvm::dyn_cast<clang::VariableArrayType>(type)) {
				//VariableArray turns into array<int<4>,1>
				
				//checking array type
				EXPECT_TRUE(irType.isa<ArrayTypePtr>());
				
				//checking elementType
				ArrayTypePtr arrType = irType.as<ArrayTypePtr>();
				TypePtr&& elemTy = convFact.convertType( varArrType->getElementType().getTypePtr() );
				EXPECT_TRUE( *elemTy == *arrType.getElementType());
			}
		}

		//if(const clang::FunctionType* funcType = llvm::dyn_cast<clang::FunctionType>(type)) {
		if(llvm::isa<clang::FunctionType>(type)) {
			//check isFunctionType
			EXPECT_TRUE(irType.isa<FunctionTypePtr>());
			auto funcTy = irType.as<FunctionTypePtr>();
			
			//check return type
			auto retTy = funcTy.getReturnType();
			if(retTy.isa<RefTypePtr>()) {
				//check ref for vector/array irTypes
				auto refTy = retTy.as<RefTypePtr>();
				EXPECT_TRUE(refTy.getElementType().isa<VectorTypePtr>() || refTy.getElementType().isa<ArrayTypePtr>());

				//TODO check for OCL/GCC vector Types --> shouldn't have ref
				// this checks only if SIMDvectors are used without ref, probably not enough
				EXPECT_FALSE(insieme::core::lang::isSIMDVector(irType));
			}

			if(const clang::FunctionProtoType* funcProtoType = llvm::dyn_cast<clang::FunctionProtoType>(type)) {
				auto parameterTypes = funcTy.getParameterTypes();

				//check variadic
				if(funcProtoType->isVariadic()) {
					auto varTy = parameterTypes.back();
					//TODO add check if varTy is IR VarList

					//check number of arguments --> Variadic arguments are represented as extra
					//argument in IR!
					EXPECT_EQ(funcProtoType->getNumArgs() + 1, parameterTypes.size());
				} else {
					//check number of arguments
					EXPECT_EQ(funcProtoType->getNumArgs(), parameterTypes.size());
				}


				//check ref for vector/array irTypes of arguments
				for(auto t : parameterTypes) {
					if(retTy.isa<RefTypePtr>()) {
						//check ref for vector/array irTypes
						auto refTy = retTy.as<RefTypePtr>();
						EXPECT_TRUE(refTy.getElementType().isa<VectorTypePtr>() || refTy.getElementType().isa<ArrayTypePtr>());
		
						
						//TODO check for OCL/GCC vector Types --> shouldn't have ref
						// this checks only if SIMDvectors are used without ref, probably not enough
						EXPECT_FALSE(insieme::core::lang::isSIMDVector(irType));
					} 
				}

				//check single "void" arg is removed
				if(funcProtoType->getNumArgs() == 1 && funcProtoType->getArgType(0).getTypePtr()->isVoidType()) {
					EXPECT_EQ(parameterTypes.size(), 0);
				}

				
			}
		}

		//if(const clang::VectorType* vecType = llvm::dyn_cast<clang::VectorType>(type)) {
		if(llvm::isa<clang::VectorType>(type)) {
			//check if irType is a IR SIMDtype
			EXPECT_TRUE(insieme::core::lang::isSIMDVector(irType));
		}

		return irType;
	}
};

TEST(TypeConversion, C99_BuiltinTypes) {
	fe::Source src(
		R"(
			#include <wchar.h>
			_Bool b;
			
			unsigned char uc;
			char c;

			wchar_t wc;

			int i;
			unsigned int ui;
			short s;
			unsigned short us;
			long l;
			unsigned long ul;

			__int128_t i128;
			__uint128_t ui128;

			long long ll;
			unsigned long long ull;

			float f;
			double d;
			long double ld;
		)"
	);

	NodeManager mgr;
	IRBuilder builder(mgr);
	fe::ConversionJob job(src);
	job.registerFrontendPlugin<UnitTestBuiltinTypes>();
	//std::cout << job.toIRTranslationUnit(mgr);
}

TEST(TypeConversion, CPP03_BuiltinTypes) {
	fe::Source src(
		R"(
		int main() {
			bool b;
			
			unsigned char uc;
			char c;

			wchar_t wc;

			int i;
			unsigned int ui;
			short s;
			unsigned short us;
			long l;
			unsigned long ul;
			
			__int128_t i128;
			__uint128_t ui128;

			long long ll;
			unsigned long long ull;

			float f;
			double d;
			long double ld;

			return 0;
		}
		)", fe::CPP
	);

	NodeManager mgr;
	IRBuilder builder(mgr);
	fe::ConversionJob job(src);
	job.setStandard(fe::ConversionSetup::Cxx03);
	job.registerFrontendPlugin<UnitTestBuiltinTypes>();
	//std::cout << job.toIRTranslationUnit(mgr);
}

TEST(TypeConversion, CPP11_BuiltinTypes) {
	fe::Source src(
		R"(
		int main() {
			bool b;
			
			unsigned char uc;
			char c;

			wchar_t wc;
			char16_t c16;
			char32_t c32;

			int i;
			unsigned int ui;
			short s;
			unsigned short us;
			long l;
			unsigned long ul;
			
			__int128_t i128;
			__uint128_t ui128;

			long long ll;
			unsigned long long ull;
			
			float f;
			double d;
			long double ld;


			return 0;
		}
		)", fe::CPP
	);

	NodeManager mgr;
	IRBuilder builder(mgr);
	fe::ConversionJob job(src);
	job.setStandard(fe::ConversionSetup::Cxx11);
	job.registerFrontendPlugin<UnitTestBuiltinTypes>();
	//std::cout << job.toIRTranslationUnit(mgr);
}

TEST(TypeConversion, ComplexType) {
	fe::Source src(
		R"(

		int main() {
			{
				_Complex float cf;
				//TODO test operations
			}
			{
				_Complex double cd;
				//TODO test operations
			}
			{
				_Complex int ci;
				//TODO test operations
			}
			return 0;
		}
			
		)"
	);

	NodeManager mgr;
	IRBuilder builder(mgr);
	fe::ConversionJob job(src);
	job.registerFrontendPlugin<UnitTestBuiltinTypes>();
	//std::cout << job.toIRTranslationUnit(mgr);
}

TEST(TypeConversion, ArrayType) {
	fe::Source src(
		R"(

		int fun() { return 10; }

		int main() {
			{
				//constant array (size 404)
				int intAarr[4+400];
				double doubleArr[4+400];
			}
			{
				//constant array (size 404)
				int intAarr[404][4+400];
				double doubleArr[404][4+400];
			}
			{
				//constant array (size 1)
				int intArr[] = {0};
				double doubleArr[] = {0};
			}
			{
				//constant array ([2][1])
				int intArr[][1] = {{0},{0}};
				double doubleArr[][1] = {{0},{0}};
			}

			{
				//variable array
				int x = 10;
				int intArr[x + fun()];
				double doubleArr[x + fun()];
			}
			
			{
				//variable array
				int x = 10;
				int intArr[1][x + fun()];
				double doubleArr[1][x + fun()];
			}
			{
				//variable array
				int x = 10;
				int intArr[x+fun()][x + fun()];
				double doubleArr[x+fun()][x + fun()];

			} 
			return 0;
		}
			
		)"
	);

	NodeManager mgr;
	IRBuilder builder(mgr);
	fe::ConversionJob job(src);
	job.registerFrontendPlugin<UnitTestBuiltinTypes>();
	//std::cout << job.toIRTranslationUnit(mgr);
}

TEST(TypeConversion, FunctionType) {
	fe::Source src(
		R"(

		void fun_v_() { }
		void fun_v_v(void) { }
		int fun_i_() { return 10; }
		int fun_i_v(void) { return 10; }
		int fun_i_i(int a) { return 10; }
		int fun_i_ia(int a[]) { return 10; }
		int fun_i_ip(int* a) { return 10; }
		int fun_i_i_var(int a, ...) { return 10; }

		int main() {
			
			return 0;
		}
			
		)"
	);

	NodeManager mgr;
	IRBuilder builder(mgr);
	fe::ConversionJob job(src);
	job.registerFrontendPlugin<UnitTestBuiltinTypes>();
	//std::cout << job.toIRTranslationUnit(mgr);
}

TEST(TypeConversion, SIMDVectorType) {
	fe::Source src(
		R"(

		int __attribute__((vector_size(4*sizeof(int)))) fun() { 
			int __attribute__((vector_size(4*sizeof(int)))) vectorInt4;
			return vectorInt4;
		}

		int __attribute__((vector_size(4*sizeof(int)))) fun1(int __attribute__((vector_size(4*sizeof(int)))) arg) { 
			return arg;
		}

		int main() {
			int __attribute__((vector_size(4*sizeof(int)))) vectorInt4;
			fun();
			fun1(vectorInt4);
			return 0;
		}
			
		)"
	);

	NodeManager mgr;
	IRBuilder builder(mgr);
	fe::ConversionJob job(src);
	job.registerFrontendPlugin<UnitTestBuiltinTypes>();
	std::cout << job.toIRTranslationUnit(mgr);
}
