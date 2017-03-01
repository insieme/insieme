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
// TODO: add support for more types


#define INFO_STRUCT_BEGIN(__name)                                                                                                                              \
	void irt_meta_info_print_##__name(FILE* dest, irt_meta_info_table_entry* entry) {                                                                          \
		fprintf(dest, #__name ": ");                                                                                                                           \
		__name##_info* i = &entry->__name;                                                                                                                     \
		if(!i->available) {                                                                                                                                    \
			fprintf(dest, "Not available");                                                                                                                    \
			return;                                                                                                                                            \
		}                                                                                                                                                      \
		fprintf(dest, "Available: { ");

#define INFO_FIELD(__id, __type, __val)                                                                                                                        \
	{                                                                                                                                                          \
		fprintf(dest, #__id " = ");                                                                                                                            \
		const char* ts = #__type;                                                                                                                              \
		if(strcmp(ts, "int") == 0)                                                                                                                             \
			fprintf(dest, "%d", *(int*)(void*) & i->__id);                                                                                                     \
		else if(strcmp(ts, "unsigned") == 0)                                                                                                                   \
			fprintf(dest, "%u", *(unsigned*)(void*) & i->__id);                                                                                                \
		else if(strcmp(ts, "bool") == 0)                                                                                                                       \
			fprintf(dest, "%s", *(bool*)(void*) & i->__id ? "true" : "false");                                                                                 \
		else                                                                                                                                                   \
			fprintf(dest, "%s", (i->__id == __val) ? "default" : "set");                                                                                       \
		fprintf(dest, ", ");                                                                                                                                   \
	}

#define INFO_STRUCT_END()                                                                                                                                      \
	fprintf(dest, " } ");                                                                                                                                      \
	}
