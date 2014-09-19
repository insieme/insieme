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

#pragma once
#ifndef __GUARD_META_INFORMATION_META_INFOS_H
#define __GUARD_META_INFORMATION_META_INFOS_H

#include "insieme/common/common.h"

// build struct definitions
#include "meta_information/struct_generator.inc"
#include "insieme/common/meta_infos.def"

// build default table entry definition containing all structs
struct _irt_meta_info_table_entry {
#ifdef IRT_META_INFO_TABLE_ENTRY_FIELDS
	IRT_META_INFO_TABLE_ENTRY_FIELDS
#else // IRT_META_INFO_TABLE_ENTRY_FIELDS
	// generate default entry with all values
	#include "meta_information/default_generator.inc"
	#include "insieme/common/meta_infos.def"
#endif // IRT_META_INFO_TABLE_ENTRY_FIELDS
};

// build accessors for metainformation
#include "meta_information/accessor_generator.inc"
#include "insieme/common/meta_infos.def"

// build printer for metainformation
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wstrict-aliasing"
#include "meta_information/printer_generator.inc"
#include "insieme/common/meta_infos.def"
#pragma GCC diagnostic pop

#endif //#ifndef __GUARD_META_INFORMATION_META_INFOS_H
