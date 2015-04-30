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

#include <gtest/gtest.h>

#include "insieme/frontend/frontend.h"
#include "insieme/core/ir_builder.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/frontend/tu/ir_translation_unit_check.h"

#include "insieme/utils/test/test_utils.h"

#include "insieme/driver/cmd/insiemecc_options.h"

#include "test_utils.inc"

using namespace insieme::driver;

namespace insieme {
namespace frontend {

#define RUN_SEMA(irCode) \
		auto msg = insieme::core::checks::check(irCode);\
		if(!msg.empty()) {\
			for(auto m: msg.getErrors()) {\
				std::cout << m.getMessage() << "\n\t" << m.getLocation() << " code: " << m.getErrorCode() << std::endl;\
			}\
		}\
		EXPECT_TRUE(msg.empty());

	TEST(AnonymousTypes, StructTypedef) {

		Source src(
				R"(

					typedef struct {
					} A;

					int main() {
						A x;
					}

				)"
		);

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);
        const boost::filesystem::path& fileName = src;
        std::vector<std::string> argv = { "compiler",  fileName.string() };
        cmd::Options options = cmd::Options::parse(argv);

		auto res = builder.normalize(options.job.execute(mgr));

//		dumpPretty(res);
		RUN_SEMA(res);
	}

	TEST(AnonymousTypes, StructArray) {
		// TRACK THIS ERROR
	//struct<_const_cpp_refrc<vector<struct __mpz_struct <_mp_alloc:int<4>,_mp_size:int<4>,_mp_d:ref<array<uint<8>,1>>>,1>>>
	//struct<_const_cpp_refrc<vector<struct              <_mp_alloc:int<4>,_mp_size:int<4>,_mp_d:ref<array<uint<8>,1>>>,1>>>

		Source src(
				R"(
					typedef struct {
						int a;
						int b;
					} A;

					typedef A X[1];

					//  parameters tests

					int val (X x){
						return x[0].a;
					}
					int ref (X& x){
						return x[0].a;
					}
					int constRef (const X& x){
						return x[0].a;
					}

					//  return tests

					X global;

					X& byRef(){
						return global;
					}
					const X& byConstRef(){
						return global;
					}

					int main() {
						X x;

						val(x);
						ref(x);
						constRef(x);

						byRef();
						byConstRef();

						val(byRef());
						ref(byRef());
						constRef(byRef());
						constRef(byConstRef());
					}
				)"
		);

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);
        const boost::filesystem::path& fileName = src;
        std::vector<std::string> argv = { "compiler",  fileName.string(), "--std=c++03" };
        cmd::Options options = cmd::Options::parse(argv);

		auto res = builder.normalize(options.job.execute(mgr));

//		dumpPretty(res);
		RUN_SEMA(res);
	}

	TEST(AnonymousTypes, Tempalated) {
		// TRACK THIS ERROR
	//struct<_const_cpp_refrc<vector<struct __mpz_struct <_mp_alloc:int<4>,_mp_size:int<4>,_mp_d:ref<array<uint<8>,1>>>,1>>>
	//struct<_const_cpp_refrc<vector<struct              <_mp_alloc:int<4>,_mp_size:int<4>,_mp_d:ref<array<uint<8>,1>>>,1>>>
	//
     //return RefIRToConstCpp(fun000(scalar.to.array(ref.narrow(scalar.to.array(v1)&[0], dp.root.as<type<type003>>, type<type003>))&[0])&[0]->mpZ);
	 //return RefIRToConstCpp(fun001(scalar.to.array(ref.narrow(scalar.to.array(v1)&[0], dp.root.as<type<type001>>, type<type001>))&[0])&[0]->a);
	 //


		Source src(
				R"(

					template <typename T>
					struct handle {
						struct block{
							T t;
						} field;

						typedef T element_type;

						const element_type* Ptr() const{
							return &(field.t);
						}

					};

					typedef struct {
						int a;
						int b;
					} A;

					void f(const A* ptr){
					}

					typedef A X[1];

					struct wrap{
						X field;
					};

					struct C : public handle<wrap>{
						const X& getConstRef()const {
							return Ptr()->field;
						}
					};

					int main() {
						C obj;
						f(obj.getConstRef());
					}
				)"
		);

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);
        const boost::filesystem::path& fileName = src;
        std::vector<std::string> argv = { "compiler",  fileName.string(), "--std=c++03" };
        cmd::Options options = cmd::Options::parse(argv);

		auto res = builder.normalize(options.job.execute(mgr));

	//	dumpPretty(res);
		auto msg = insieme::core::checks::check(res);
		if(!msg.empty()) {
			for(auto m: msg.getErrors()) {
				std::cout << m.getMessage() << "\n\t" << m.getLocation() << " code: " << m.getErrorCode() << std::endl;
			}
		}
		EXPECT_TRUE(msg.empty());
	}

	TEST(AnonymousTypes, Union) {
		Source src(
				R"(

					typedef union{
						int a;
						double b;
					} mytype;

					int main() {
						mytype a;
						a.a = 1;
						mytype n;
						a.b = 1.0;
					}
				)"
		);

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);
        const boost::filesystem::path& fileName = src;
        std::vector<std::string> argv = { "compiler",  fileName.string(), "--std=c++03" };
        cmd::Options options = cmd::Options::parse(argv);

		auto res = builder.normalize(options.job.execute(mgr));

	//	dumpPretty(res);
		auto msg = insieme::core::checks::check(res);
		if(!msg.empty()) {
			for(auto m: msg.getErrors()) {
				std::cout << m.getMessage() << "\n\t" << m.getLocation() << " code: " << m.getErrorCode() << std::endl;
			}
		}
		EXPECT_TRUE(msg.empty());
	}

	TEST(AnonymousTypes, Nested) {
		Source src(
				R"(

					typedef struct{
						int a;
					} B;

					typedef struct{
						B a;
					} A;

					int main() {
						A var;
					}
				)"
		);

		core::NodeManager mgr;
		core::IRBuilder builder(mgr);
        const boost::filesystem::path& fileName = src;
        std::vector<std::string> argv = { "compiler",  fileName.string(), "--std=c++03" };
        cmd::Options options = cmd::Options::parse(argv);

		auto res = builder.normalize(options.job.execute(mgr));

		dumpPretty(res);
		auto msg = insieme::core::checks::check(res);
		if(!msg.empty()) {
			for(auto m: msg.getErrors()) {
				std::cout << m.getMessage() << "\n\t" << m.getLocation() << " code: " << m.getErrorCode() << std::endl;
			}
		}
		EXPECT_TRUE(msg.empty());
	}
} // end namespace frontend
} // end namespace insieme
