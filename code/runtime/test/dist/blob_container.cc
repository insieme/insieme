/**
 * Copyright (c) 2002-2017 Distributed and Parallel Systems Group,
 *                Institute of Computer Science,
 *               University of Innsbruck, Austria
 *
 * This file is part of the INSIEME Compiler and Runtime System.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
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
 */
#include <stdlib.h>
#include <gtest/gtest.h>

extern "C" {
#include "dist/impl/blob_container.impl.h"
#include "standalone.h"
}

// TEST DEACTIVATED BECAUSE OF VALGRIND
// TEST(Blob, malfunction){
//	auto blob_cont = _dirt_init();
//
//	// avoid warning
//	::testing::FLAGS_gtest_death_test_style = "threadsafe";
//
//	// read when empty
//	ASSERT_EXIT( _dirt_read_from_blob_container(&blob_cont),::testing::KilledBySignal(10), "") ;
//
//	// clean when not consumed
//	_dirt_write_to_blob_container(&blob_cont, malloc(sizeof(int)), sizeof(int));
//	ASSERT_EXIT( _dirt_delete(&blob_cont),::testing::KilledBySignal(10), "") ;
//}

#define ADD_TYPE(type) types.push_back({(void*)malloc(sizeof(type)), sizeof(type)});
#define ADD_TYPE_N_VALUE(type, val)                                                                                                                            \
	{                                                                                                                                                          \
		type* v = (type*)malloc(sizeof(type));                                                                                                                 \
		*v = val;                                                                                                                                              \
		types.push_back({(void*)v, sizeof(type)});                                                                                                             \
	}
typedef int int_array[1024];
typedef struct {
	int a;
	double b;
} pod;

TEST(Blob, readWrite) {
	typedef std::pair<void*, size_t> blob_pair;
	std::vector<blob_pair> types;


	ADD_TYPE_N_VALUE(char, 'a');
	ADD_TYPE_N_VALUE(int, -689);
	ADD_TYPE_N_VALUE(long, 56789);
	ADD_TYPE_N_VALUE(long long, 87654);

	ADD_TYPE_N_VALUE(unsigned char, 1);
	ADD_TYPE_N_VALUE(unsigned int, 3);
	ADD_TYPE_N_VALUE(unsigned long, 6);
	ADD_TYPE_N_VALUE(unsigned long long, 9);

	ADD_TYPE_N_VALUE(float, 5.0f);
	ADD_TYPE_N_VALUE(double, 6.0);
	// ADD_TYPE_N_VALUE(long double, 78.0);

	// array
	{
		int* v = (int*)malloc(sizeof(int) * 10);
		for(int i = 0; i < 10; ++i) {
			v[i] = i;
		}
		types.push_back({(void*)v, sizeof(int) * 10});
	}

	// POD
	{
		pod* v = (pod*)calloc(1, sizeof(pod));
		v->a = 567;
		v->b = 98.0005;
		types.push_back({(void*)v, sizeof(pod)});
	}

	{
		auto blob_cont = _dirt_init();
		// load the types
		for(const auto& x : types) {
			_dirt_write_to_blob_container(&blob_cont, x.first, x.second);
		}

		// unload
		for(const auto& x : types) {
			dirt_blob blob = _dirt_read_from_blob_container(&blob_cont);
			EXPECT_EQ(x.second, blob.size); // compare the size of the field
			EXPECT_EQ(0, memcmp(x.first, blob.payload, blob.size)); // compare the value
		}

		// cleanup
		for(auto& x : types) {
			free(x.first);
		}

		_dirt_delete(&blob_cont);
	}
}


TEST(Blob, packUnpack) {
	typedef std::pair<void*, size_t> blob_pair;
	std::vector<blob_pair> types;

	ADD_TYPE_N_VALUE(char, 'a');
	ADD_TYPE_N_VALUE(int, -689);
	ADD_TYPE_N_VALUE(long, 56789);
	ADD_TYPE_N_VALUE(long long, 87654);

	ADD_TYPE_N_VALUE(unsigned char, 1);
	ADD_TYPE_N_VALUE(unsigned int, 3);
	ADD_TYPE_N_VALUE(unsigned long, 6);
	ADD_TYPE_N_VALUE(unsigned long long, 9);

	ADD_TYPE_N_VALUE(float, 5.0f);
	ADD_TYPE_N_VALUE(double, 6.0);
	// ADD_TYPE_N_VALUE(long double, 78.0);

	// array
	{
		int* v = (int*)malloc(sizeof(int) * 1024);
		for(int i = 0; i < 1024; ++i) {
			v[i] = i;
		}
		types.push_back({(void*)v, sizeof(int) * 1024});
	}

	// POD
	{
		pod* o = (pod*)calloc(1, sizeof(pod));
		o->a = 567;
		o->b = 98.0005;
		types.push_back({(void*)o, sizeof(pod)});
	}


	dirt_byte_stream stream;

	{
		auto blob_cont = _dirt_init();

		// load the types
		for(const auto& x : types) {
			_dirt_write_to_blob_container(&blob_cont, x.first, x.second);
		}

		stream = _dirt_blob_pack(&blob_cont);

		_dirt_delete(&blob_cont);
	}

	EXPECT_EQ(4262, stream.size);

	{
		auto blob_cont = _dirt_init();

		_dirt_blob_unpack(&blob_cont, stream);

		// unload
		for(const auto& x : types) {
			dirt_blob blob = _dirt_read_from_blob_container(&blob_cont);
			EXPECT_EQ(x.second, blob.size);                         // compare the size of the field
			EXPECT_EQ(0, memcmp(x.first, blob.payload, blob.size)); // compare the value
		}

		_dirt_delete(&blob_cont);
	}

	// cleanup
	for(auto& x : types) {
		free(x.first);
	}
}
