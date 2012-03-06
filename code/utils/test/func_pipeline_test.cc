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

#include <gtest/gtest.h>
#include "insieme/utils/func_pipeline.h"

using namespace insieme::utils;

TEST(FunctionPipeline, OneStage) {
	
	auto input = std::make_tuple(10,10);
	auto output = std::make_tuple(1,2,3);

	Function<2,0,1> f(
			input, output, std::function<int (const int&, const int&)>(std::plus<int>())
		);

	EXPECT_EQ(3, std::get<2>(output));
	f();
	EXPECT_EQ(20, std::get<2>(output));
}

TEST(FunctionPipeline, TwoStage) {

	auto input = std::make_tuple(std::string("hello world"), 10, 0.4);
	auto output = std::make_tuple(1, 2); 

	Function<1,0> f1(input, output, 
			std::function<size_t (const std::string&)>( std::bind(&std::string::size, std::placeholders::_1) )
		);
	
	Function<1,0,1> f2(output, output, std::function<int (const int&, const int&)>( std::minus<int>()) );

	Stage s(f1, f2);

	EXPECT_EQ(2, std::get<1>(output));
	s();
	EXPECT_EQ(-10, std::get<1>(output));
}



