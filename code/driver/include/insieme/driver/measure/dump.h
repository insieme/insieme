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

#include <boost/serialization/access.hpp>

#include "insieme/driver/measure/measure.h"
#include "insieme/utils/printable.h"

namespace insieme {
namespace driver {
namespace measure {

	/*
	 * A light metric class to serialize limited information of driver::measure::Metric
	 */
	class LightweightMetric : public utils::Printable {

	  public:

		std::string name;

		LightweightMetric(const MetricPtr& metric) : name(metric->getName()) {}
		LightweightMetric() : name() {}

		std::ostream& printTo(std::ostream& out) const { return out << name; }

		bool operator<(const LightweightMetric other) const { return name < other.name; }

	  private:

	  	friend class boost::serialization::access;

	  	template<typename Archive>
	  	void serialize(Archive& archive, const unsigned int version) {
			archive& name;
		}

	};

	/*
	 * A light quantity class to serialize limited information of driver::measure::Quantity
	 */
	class LightweightQuantity : public utils::Printable {

	  public:

		double value;

		LightweightQuantity(const Quantity& quantity) : value(quantity.getValue()) {}
		LightweightQuantity() : value() {}

		std::ostream& printTo(std::ostream& out) const { return out << value; }

	  private:

		friend class boost::serialization::access;

	  	template<typename Archive>
	  	void serialize(Archive& archive, const unsigned int version) {
			archive& value;
		}

	};

} // end namespace measure
} // end namespace driver
} // end namespace insieme

