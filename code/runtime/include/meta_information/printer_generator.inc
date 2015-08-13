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
