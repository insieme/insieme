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
#include <boost/filesystem.hpp>

#include "insieme/core/ir_builder.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/checks/full_check.h"
#include "insieme/core/printer/pretty_printer.h"
#include "insieme/core/printer/error_printer.h"

#include "insieme/backend/converter.h"
#include "insieme/backend/opencl/opencl_backend.h"

#include "insieme/annotations/opencl/opencl_annotations.h"

#include "insieme/utils/compiler/compiler.h"

namespace insieme {
namespace backend {
namespace opencl {

	using namespace insieme::annotations::opencl;

	namespace {
		void addAnnotations(const core::NodePtr& node, BaseAnnotation::AnnotationList& annos) {
			if(annos.empty()) return;
			// get old annotation list and append our annotations
			if(node->hasAnnotation(BaseAnnotation::KEY)) {
				auto& lst = node->getAnnotation(BaseAnnotation::KEY)->getAnnotationList();
				lst.insert(lst.end(), annos.begin(), annos.end());
			} else {
				// in this case we need to create a new one
				node->addAnnotation(std::make_shared<BaseAnnotation>(annos));
			}
		}

		void addDeviceAnnotation(const core::NodePtr& node, Device::Type type) {
			BaseAnnotation::AnnotationList annos;
			annos.push_back(std::make_shared<DeviceAnnotation>(std::make_shared<Device>(type)));
			addAnnotations(node, annos);
		}

		void addLoopAnnotation(const core::NodePtr& node, bool independent) {
			BaseAnnotation::AnnotationList annos;
			annos.push_back(std::make_shared<LoopAnnotation>(independent));
			addAnnotations(node, annos);
		}

		void addVariableRequirement(const core::NodePtr& node, const core::VariablePtr& var,
									VariableRequirement::AccessMode accessMode, const core::ExpressionPtr& size,
									const core::ExpressionPtr& start, const core::ExpressionPtr& end) {
			VariableRangeList ranges;
			ranges.push_back(std::make_shared<VariableRange>(size, start, end));

			BaseAnnotation::AnnotationList annos;
			annos.push_back(std::make_shared<VariableRequirement>(var, accessMode, ranges));
			addAnnotations(node, annos);
		}
	}

	TEST(MatrixMultiplication, Basic) {
		core::NodeManager manager;
		core::IRBuilder builder(manager);

		/*
		this test models the following C code:
		note1: timex is emitted as it is not relevant for the functionality
		note2: compound assignments have been manually stripped

		#include <stdio.h>
		#include <stdlib.h>
		#include <math.h>
		#include <stdbool.h>
		#include <time.h>
		#include <sys/time.h>

		#define TIMEX_BGN(name) \
			{ \
				const char *__name = name; \
				struct timeval __t1, __t2; \
				double __elapsed; \
				gettimeofday(&__t1, NULL); \
				{

		#define TIMEX_END() \
				} \
				gettimeofday(&__t2, NULL); \
				__elapsed  = (__t2.tv_sec  - __t1.tv_sec) * 1000.0; \
				__elapsed += (__t2.tv_usec - __t1.tv_usec) / 1000.0; \
				fprintf(stdout, "%s: %.3f ms\n", __name, __elapsed); \
			}

		bool mat_is_equal(float *fst, float *snd, unsigned elements)
		{
			for (unsigned i = 0; i < elements; ++i) {
				if ((unsigned) fst[i] != (unsigned) snd[i]) return false;
			}
			return true;
		}

		bool mat_is_value(float *mat, unsigned elements, float value)
		{
			for (unsigned i = 0; i < elements; ++i)
				if (mat[i] != value) return false;
			return true;
		}

		bool mat_init(float *mat, unsigned elements, float value)
		{
			TIMEX_BGN("mat_init_acc");
			#pragma opencl device type(ALL)
			#pragma opencl requirement(mat, range(elements:0:elements), WO)
			#pragma opencl loop independent(yes)
			#pragma omp parallel for
			for (unsigned i = 0; i < elements; ++i)
				mat[i] = value;
			TIMEX_END();
			return mat_is_value(mat, elements, value);
		}

		int main(int argc, char **argv)
		{
			const int M = 1000;
			const int N = 1000;
			const int P = 1000;

			float *A = malloc(sizeof(*A)    * N * P);
			float *B = malloc(sizeof(float) * M * P);

			const size_t szFloat = sizeof(float);
			float *C = malloc(szFloat       * M * N);
			float *D = malloc(szFloat       * M * N);

			bool result = true;
			result &= mat_init(A, N * P, 2);
			result &= mat_init(B, M * P, 3);
			result &= mat_init(C, M * N, 0);
			result &= mat_init(D, M * N, 0);
			if (result) {
				// introduce some random bits
				for (unsigned i = 0; i < 1024; ++i) {
					A[rand() % (N*P)] = (rand() % 10) + 1;
					B[rand() % (M*P)] = (rand() % 10) + 1;
				}

				TIMEX_BGN("mat_mult_acc");
				// now do the matrix multiplication
				#pragma opencl device type(ALL)
				#pragma opencl loop independent(yes)
				#pragma omp parallel for
				for (unsigned i = 0; i < N; ++i)
					#pragma opencl loop independent(yes)
					for (unsigned j = 0; j < M; ++j)
						#pragma opencl loop independent(no)
						for (unsigned k = 0; k < P; ++k)
							C[i*N+j] += A[i*P+k] * B[k*N+j];
				TIMEX_END();

				TIMEX_BGN("mat_mult_seq");
				// check with cpu if mat computation is correct
				for (unsigned i = 0; i < N; ++i)
					for (unsigned j = 0; j < M; ++j)
						for (unsigned k = 0; k < P; ++k)
							D[i*N+j] += A[i*P+k] * B[k*N+j];
				TIMEX_END();
				result = mat_is_equal(C, D, M*N);
			}

			free(A);
			free(B);
			free(C);
			free(D);
			return result ? 0 : 1;
		}
		*/

		auto prog = builder.parseProgram(R"(
			decl IMP_malloc : (uint<8>) -> ptr<unit>;
			decl IMP_gettimeofday : (ptr<IMP_timeval>, ptr<IMP_timezone>) -> int<4>;
			decl IMP_fprintf : (ptr<IMP__IO_FILE>, ptr<char,t,f>, var_list) -> int<4>;
			decl IMP_rand : () -> int<4>;
			decl IMP_free : (ptr<unit>) -> unit;
			decl IMP_main : (int<4>, ptr<ptr<char>>) -> int<4>;
			decl IMP_mat_init : (ptr<real<4>>, uint<4>, real<4>) -> bool;
			decl IMP_mat_is_value : (ptr<real<4>>, uint<4>, real<4>) -> bool;
			decl IMP_mat_is_equal : (ptr<real<4>>, ptr<real<4>>, uint<4>) -> bool;
			def IMP_mat_init = function (v56 : ref<ptr<real<4>>,f,f,plain>, v57 : ref<uint<4>,f,f,plain>, v58 : ref<real<4>,f,f,plain>) -> bool {
				{
					var ref<uint<4>,f,f,plain> v81 = num_cast(0, type_lit(uint<4>));
					for( uint<4> v143 = num_cast(0, type_lit(uint<4>)) .. *v57 : 1u) {
						ptr_subscript(*v56, v143) = *v58;
					};
				};
				return IMP_mat_is_value(*v56, *v57, *v58);
			};
			def IMP_mat_is_value = function (v48 : ref<ptr<real<4>>,f,f,plain>, v49 : ref<uint<4>,f,f,plain>, v50 : ref<real<4>,f,f,plain>) -> bool {
				{
					var ref<uint<4>,f,f,plain> v51 = num_cast(0, type_lit(uint<4>));
					while(*v51<*v49) {
						if(*ptr_subscript(*v48, *v51)!=*v50) {
							return 0!=0;
						};
						gen_pre_inc(v51);
					};
				};
				return 1!=0;
			};
			def IMP_mat_is_equal = function (v17 : ref<ptr<real<4>>,f,f,plain>, v18 : ref<ptr<real<4>>,f,f,plain>, v19 : ref<uint<4>,f,f,plain>) -> bool {
				{
					var ref<uint<4>,f,f,plain> v20 = num_cast(0, type_lit(uint<4>));
					while(*v20<*v19) {
						if(num_cast(*ptr_subscript(*v17, *v20), type_lit(uint<4>))!=num_cast(*ptr_subscript(*v18, *v20), type_lit(uint<4>))) {
							return 0!=0;
						};
						gen_pre_inc(v20);
					};
				};
				return 1!=0;
			};
			// Inspire Program
			int<4> function IMP_main (v91 : ref<int<4>,f,f,plain>, v92 : ref<ptr<ptr<char>>,f,f,plain>){
				var ref<int<4>,t,f,plain> v93 = 1000;
				var ref<int<4>,t,f,plain> v94 = 1000;
				var ref<int<4>,t,f,plain> v95 = 1000;
				var ref<ptr<real<4>>,f,f,plain> v96 = ptr_reinterpret(IMP_malloc(sizeof(type_lit(real<4>))*num_cast(*v94, type_lit(uint<8>))*num_cast(*v95, type_lit(uint<8>))), type_lit(real<4>));
				var ref<ptr<real<4>>,f,f,plain> v100 = ptr_reinterpret(IMP_malloc(sizeof(type_lit(real<4>))*num_cast(*v93, type_lit(uint<8>))*num_cast(*v95, type_lit(uint<8>))), type_lit(real<4>));
				var ref<uint<8>,t,f,plain> v101 = sizeof(type_lit(real<4>));
				var ref<ptr<real<4>>,f,f,plain> v102 = ptr_reinterpret(IMP_malloc(*v101*num_cast(*v93, type_lit(uint<8>))*num_cast(*v94, type_lit(uint<8>))), type_lit(real<4>));
				var ref<ptr<real<4>>,f,f,plain> v103 = ptr_reinterpret(IMP_malloc(*v101*num_cast(*v93, type_lit(uint<8>))*num_cast(*v94, type_lit(uint<8>))), type_lit(real<4>));
				var ref<bool,f,f,plain> v104 = 1!=0;
				v104 = v104 && IMP_mat_init(*v96, num_cast(*v94**v95, type_lit(uint<4>)), num_cast(2, type_lit(real<4>)));
				v104 = v104 && IMP_mat_init(*v100, num_cast(*v93**v95, type_lit(uint<4>)), num_cast(3, type_lit(real<4>)));
				v104 = v104 && IMP_mat_init(*v102, num_cast(*v93**v94, type_lit(uint<4>)), num_cast(0, type_lit(real<4>)));
				v104 = v104 && IMP_mat_init(*v103, num_cast(*v93**v94, type_lit(uint<4>)), num_cast(0, type_lit(real<4>)));
				if(*v104) {
					{
						var ref<uint<4>,f,f,plain> v108 = num_cast(0, type_lit(uint<4>));
						for( uint<4> v150 = num_cast(0, type_lit(uint<4>)) .. num_cast(1024, type_lit(uint<4>)) : 1u) {
							{
								ptr_subscript(*v96, IMP_rand()%(*v94**v95)) = num_cast(IMP_rand()%10+1, type_lit(real<4>));
								ptr_subscript(*v100, IMP_rand()%(*v93**v95)) = num_cast(IMP_rand()%10+1, type_lit(real<4>));
							};
						};
					};
					{
						var ref<uint<4>,f,f,plain> v113 = num_cast(0, type_lit(uint<4>));
						for( uint<4> v149 = num_cast(0, type_lit(uint<4>)) .. num_cast(*v94, type_lit(uint<4>)) : 1u) {
							{
								var ref<uint<4>,f,f,plain> v114 = num_cast(0, type_lit(uint<4>));
								for( uint<4> v148 = num_cast(0, type_lit(uint<4>)) .. num_cast(*v93, type_lit(uint<4>)) : 1u) {
									{
										var ref<uint<4>,f,f,plain> v115 = num_cast(0, type_lit(uint<4>));
										for( uint<4> v147 = num_cast(0, type_lit(uint<4>)) .. num_cast(*v95, type_lit(uint<4>)) : 1u) {
											ptr_subscript(*v102, v149*num_cast(*v94, type_lit(uint<4>))+v148) = ptr_subscript(*v102, v149*num_cast(*v94, type_lit(uint<4>))+v148) + *ptr_subscript(*v96, v149*num_cast(*v95, type_lit(uint<4>))+v147)**ptr_subscript(*v100, v147*num_cast(*v94, type_lit(uint<4>))+v148);
										};
									};
								};
							};
						};
					};
					{
						var ref<uint<4>,f,f,plain> v120 = num_cast(0, type_lit(uint<4>));
						for( uint<4> v146 = num_cast(0, type_lit(uint<4>)) .. num_cast(*v94, type_lit(uint<4>)) : 1u) {
							{
								var ref<uint<4>,f,f,plain> v121 = num_cast(0, type_lit(uint<4>));
								for( uint<4> v145 = num_cast(0, type_lit(uint<4>)) .. num_cast(*v93, type_lit(uint<4>)) : 1u) {
									{
										var ref<uint<4>,f,f,plain> v122 = num_cast(0, type_lit(uint<4>));
										for( uint<4> v144 = num_cast(0, type_lit(uint<4>)) .. num_cast(*v95, type_lit(uint<4>)) : 1u) {
											ptr_subscript(*v103, v146*num_cast(*v94, type_lit(uint<4>))+v145) = ptr_subscript(*v103, v146*num_cast(*v94, type_lit(uint<4>))+v145) + *ptr_subscript(*v96, v146*num_cast(*v95, type_lit(uint<4>))+v144)**ptr_subscript(*v100, v144*num_cast(*v94, type_lit(uint<4>))+v145);
										};
									};
								};
							};
						};
					};
					v104 = IMP_mat_is_equal(*v102, *v103, num_cast(*v93**v94, type_lit(uint<4>)));
				};
				IMP_free(ptr_reinterpret(*v96, type_lit(unit)));
				IMP_free(ptr_reinterpret(*v100, type_lit(unit)));
				IMP_free(ptr_reinterpret(*v102, type_lit(unit)));
				IMP_free(ptr_reinterpret(*v103, type_lit(unit)));
				return *v104?0:1;
			}
		)").as<core::ProgramPtr>();
		ASSERT_TRUE(prog);
		EXPECT_TRUE(core::checks::check(prog).empty())
			<< "semantic checks failed: " << core::printer::dumpErrors(core::checks::check(prog));

		// collect all for statements to obtain the addresses of the loops
		core::ForStmtAddressList markers;
		core::visitDepthFirstOnce(core::NodeAddress(prog), [&](const core::ForStmtAddress& forStmt) { markers.push_back(forStmt); });
		EXPECT_EQ(markers.size(), 8) << "failed to determine all for loops";

		// set the preferred device
		addDeviceAnnotation(markers[2].getAddressedNode(), Device::Type::ALL);
		// annotate the matrix multiplication loops with independence information
		addLoopAnnotation(markers[2].getAddressedNode(), true);
		addLoopAnnotation(markers[3].getAddressedNode(), true);
		addLoopAnnotation(markers[4].getAddressedNode(), false);

		// set the preferred device
		addDeviceAnnotation(markers[0].getAddressedNode(), Device::Type::ALL);
		// annotate with independence information
		addLoopAnnotation(markers[0].getAddressedNode(), true);
		// determine the enclosing lambda
		auto lambdaExpr = markers[0].getParentAddress(3).getAddressedNode().as<core::LambdaPtr>();
		// annotate with the requirement
		auto params = lambdaExpr->getParameterList();
		addVariableRequirement(markers[0].getAddressedNode(),
			params[0], VariableRequirement::AccessMode::WO, builder.deref(params[1]), builder.uintLit(0), builder.deref(params[1]));

		// use the backend to generate the target code & stringify it in the same step
		auto targetCode = toString(*(OpenCLBackend::getDefault()->convert(prog)));
		// check if the backend generated a kernel with 2D access
		EXPECT_NE(targetCode.find("get_global_id(1u)"), std::string::npos)
			<< "failed to generate kernel: " << targetCode;
		// check if we can compile the code, otherwise skip the execution
		if (!utils::compiler::isOpenCLAvailable()) {
			std::cerr << "skipping OpenCL MatrixMultiplication Runtime Test as CL.h is not avail!" << std::endl;
			return;
		}
		auto compiler = utils::compiler::Compiler::getOpenCLCompiler();
		// compile the code to binary
		auto targetPath = utils::compiler::compileToBinary(targetCode, compiler);
		EXPECT_TRUE(!targetPath.empty()) << "failed to compile to binary";
		// finally execute it and check if it runs correctly
		EXPECT_EQ(system(targetPath.c_str()), 0) << "opencl code failed runtime test";
	}

} // end namespace opencl
} // end namespace backend
} // end namespace insieme
