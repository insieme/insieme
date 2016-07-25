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

#include <string>
#include <set>
#include <numeric>

#include "insieme/utils/assert.h"
#include "insieme/utils/printable.h"
#include "insieme/utils/container_utils.h"

namespace insieme {
namespace driver {
namespace measure {

	/*
	* This struct represents a shared memory node of a distributed memory machine.
	*/
	struct Node : public insieme::utils::Printable {
	private:
		/*
		 * The network hostname of the node
		 */
		std::string hostname;

		/*
		* The number of cores this node holds
		*/
		unsigned numCores;
	public:
		/*
		* Constructs a compute node
		* @param hostname the network hostname of the node, "localhost" by default, cannot be empty
		* @param numCores the number of cores of this node, 1 by default, cannot be less than 1
		*/
	  Node(const std::string hostname = "localhost", const unsigned numCores = 1) : hostname(hostname), numCores(numCores) {
		  assert_false(hostname.empty()) << "Hostname must not be empty";
		  assert_gt(numCores, 0) << "Number of cores must be at least 1";
	  }

	  std::string getHostname() const { return hostname; }

	  unsigned getNumCores() const { return numCores; }

	  bool operator==(const Node& other) const { return hostname == other.hostname; }

	  bool operator<(const Node& other) const { return hostname < other.hostname; }

	  std::ostream& printTo(std::ostream& out) const {
		  return out << hostname << "(" << numCores << ")";
	  }

	};

	/*
	* This struct represents a distributed memory machine.
	*/
	struct Machine : public insieme::utils::Printable {
	private:
		/*
		* The name of the machine
		*/
		std::string name;

		/*
		* The list of nodes this machine holds
		*/
		std::set<Node> nodes;
	public:
		/*
		* Constructs a machine
		* @param name the name of the machine (not required to be a valid network hostname), "localhost" by default, cannot be empty
		* @param nodes the compute nodes this machine holds, must contain at least one node
		*/
		Machine(const std::string name = "localhost", const std::set<Node> nodes = { Node() }) : name(name), nodes(nodes) {
			assert_false(name.empty()) << "Name must not be empty";
			assert_gt(nodes.size(), 0) << "Machine must hold at least one node";
		}

		std::string getName() const { return name; }

		std::set<Node> getNodes() const { return nodes; }

		unsigned getNumNodes() const {
			return nodes.size();
		}

		/*
		* Returns the sum of all cores over all nodes of this machine
		*/
		unsigned getNumCores() const {
			return std::accumulate(nodes.begin(), nodes.end(), 0, [](int total, const Node& node) { return total + node.getNumCores(); });
		}

		std::ostream& printTo(std::ostream& out) const {
			return out << name << "(" << join(",", nodes) << ")";
		}

	};

} // end namespace measure
} // end namespace driver
} // end namespace insieme
