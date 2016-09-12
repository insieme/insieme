/**
 * Copyright (c) 2002-2016 Distributed and Parallel Systems Group,
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

#include <ostream>
#include <istream>

#include "insieme/core/forward_decls.h"
#include "insieme/core/ir_address.h"
#include "insieme/core/dump/dump.h"

#include "insieme/utils/printable.h"


namespace insieme {
namespace core {
namespace dump {

namespace text {

	/**
	 * Writes a text-based encoding of the given IR node into the given output stream.
	 *
	 * @param out the stream to be writing to
	 * @param ir the code fragment to be written
	 */
	void dumpIR(std::ostream& out, const NodePtr& ir);

	/**
	 * Writes a text-based encoding of a given IR address into the given output stream.
	 *
	 * @param out the stream to be writing to
	 * @param address the address to be written
	 * @param printAddresses whether the addresses should be printed
	 */
	void dumpAddress(std::ostream& out, const NodeAddress& address, const bool printAddresses = false);

	/**
	 * Restores an IR code fragment from the given input stream. For constructing
	 * the resulting nodes, the given manager will be used. In case the stream contains
	 * an illegal encoding, an InvalidEncodingException will be thrown.
	 *
	 * @param in the stream to be reading from
	 * @param manager the node manager to be used for creating nodes
	 * @return the resolved node
	 */
	NodePtr loadIR(std::istream& in, NodeManager& manager);

	/**
	 * Restores a node address and the associated IR constructs from the given input
	 * stream. For constructing the resulting nodes, the given manager will be used.
	 * In case the stream contains an illegal encoding, an InvalidEncodingException
	 * will be thrown.
	 *
	 * @param in the stream to be reading from
	 * @param manager the node manager to be used for creating nodes
	 * @return the resolved address
	 */
	NodeAddress loadAddress(std::istream& in, NodeManager& manager);


	/**
	 * A wrapper to be streamed into an output stream when aiming on dumping some
	 * code.
	 */
	class TextDump : public utils::Printable {
		/**
		 * The address to be dumped.
		 */
		const NodeAddress address;

		/**
		 * Whether addresses should be printed too
		 */
		const bool printAddresses;

	  public:
		/**
		 * Creates a new instance dumping the given node.
		 *
		 * @param ir the node to be dumped.
		 * @param addr whether addresses should be printed
		 */
		TextDump(const NodePtr& ir, const bool addr = false) : address(NodeAddress(ir)), printAddresses(addr) {}

		/**
		 * Creates a new instance dumping the given node address.
		 *
		 * @param address the address to be dumped.
		 * @param addr whether addresses should be printed
		 */
		TextDump(const NodeAddress& address, const bool addr = false) : address(address), printAddresses(addr) {}

		/**
		 * Bridges the gap to the actual binary dump function.
		 */
		virtual std::ostream& printTo(std::ostream& out) const {
			dumpAddress(out, address, printAddresses);
			return out;
		}
	};

} // end namespace text

} // end namespace dump
} // end namespace core
} // end namespace insieme
