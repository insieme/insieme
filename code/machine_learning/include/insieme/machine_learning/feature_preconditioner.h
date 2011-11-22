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

#include "Array/Array.h"

namespace insieme {
namespace ml {

class FeaturePreconditioner {
private:
    // the array holding the features
    Array<double>& data;

public:
    FeaturePreconditioner(Array<double>& features): data(features) {};

    /*
     * calculates the mean of column idx
     * @param
     * in param the parameters of the current feature set is stored. Param must be preallocated.
     * The size of param must exceed (3,data.dim(1))
     * param(0,i) is overwritten with the mean value of column i
     * param(1,i) is overwritten with the minimum of column i
     * param(2,i) is overwritten with the maximum of column i
     */
    void calcProp(Array<double>& param);

    /*
     * performs (((x - mean) / MAX(max - mean, mean - min) - (1 - lower) ) * (upper - lower)
     * @param
     * prop the properties of the dataset calculated with calcProp
     * lower the lower bound of the interval t normalize to
     * upper the upper bound of the interval to normalize to
     */
    void applyNormalize(Array<double>& prop, double lower, double upper);

    /*
     * normalizes the each column in the dataset and returns an array holding the means
     * @param
     * interval data will be normalized from lower to upper
     * @return
     * an array of size (3,data.dim(1)) containing the mean (in column 0), minimum (in column 1) and maximum (in column 2) of each column
     */
    Array<double> normalize( double lower, double upper);
};

} // end namespace ml
} // end namespace insieme
