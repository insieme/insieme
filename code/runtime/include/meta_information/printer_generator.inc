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

// TODO: add support for more types


#define INFO_STRUCT_BEGIN(__name) \
void irt_meta_info_print_##__name(FILE* dest, irt_meta_info_table_entry* entry) { \
	fprintf(dest, #__name ": "); \
	__name##_info* i = &entry->__name; \
	if(!i->available) { fprintf(dest, "Not available"); return; } \
	fprintf(dest, "Available: { ");	\

#define INFO_FIELD(__id, __type, __val) { \
/* Need to ingore seemingly possible printf format specifier type mismatches because we do type-checking at run time */ \
/* Also, since we are inside a macro definition, we need to use _Pragma("<options>") instead of #pragma <options> */ \
_Pragma("GCC diagnostic push") \
_Pragma("GCC diagnostic ignored \"-Wformat\"") \
		fprintf(dest, #__id " = "); \
		const char *ts = #__type; \
		if(strcmp(ts, "int") == 0) fprintf(dest, "%d", i->__id); \
		else if(strcmp(ts, "unsigned") == 0) fprintf(dest, "%u", i->__id); \
		else if(strcmp(ts, "bool") == 0) fprintf(dest, "%s", i->__id ? "true" : "false"); \
		else fprintf(dest, "%s", (i->__id == __val) ? "default" : "set"); \
		fprintf(dest, ", "); \
_Pragma("GCC diagnostic pop") \
	}

#define INFO_STRUCT_END() \
	fprintf(dest, " } "); \
}
