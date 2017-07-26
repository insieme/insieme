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
