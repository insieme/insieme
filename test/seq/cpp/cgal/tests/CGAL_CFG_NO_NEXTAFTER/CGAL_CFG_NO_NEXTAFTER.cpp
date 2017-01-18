// Copyright (c) 2006  
// Utrecht University (The Netherlands),
// ETH Zurich (Switzerland),
// INRIA Sophia-Antipolis (France),
// Max-Planck-Institute Saarbruecken (Germany),
// and Tel-Aviv University (Israel).  All rights reserved. 
//
// This file is part of CGAL (www.cgal.org); you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public License as
// published by the Free Software Foundation; either version 3 of the License,
// or (at your option) any later version.
//
// Licensees holding a valid commercial license may use this file in
// accordance with the commercial license agreement provided with the software.
//
// This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
// WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
//
// $URL: svn+ssh://scm.gforge.inria.fr/svn/cgal/branches/releases/CGAL-4.0-branch/Installation/config/testfiles/CGAL_CFG_NO_NEXTAFTER.cpp $
// $Id: CGAL_CFG_NO_NEXTAFTER.cpp 67093 2012-01-13 11:22:39Z lrineau $
// 
//
// Author(s)     : Andreas Fabri

//| If a compiler doesn't know nextafter() (or only knows _nextafter as VC++ 7.1).
//| nextafter() is part of ISO C99, but not ISO C++98 (hence <math.h> instead of <cmath>).
//| CGAL_CFG_NO_NEXTAFTER is set. 

#include <math.h>

int main()
{
  double d = nextafter(0,0);
  (void) d;
  return 0;
}
