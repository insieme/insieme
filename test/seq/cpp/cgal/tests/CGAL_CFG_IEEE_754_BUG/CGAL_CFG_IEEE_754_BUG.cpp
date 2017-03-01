// Copyright (c) 2004  
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
// $URL: svn+ssh://scm.gforge.inria.fr/svn/cgal/branches/releases/CGAL-4.0-branch/Installation/config/testfiles/CGAL_CFG_IEEE_754_BUG.cpp $
// $Id: CGAL_CFG_IEEE_754_BUG.cpp 67093 2012-01-13 11:22:39Z lrineau $
// 
// Author(s)     : Sylvain Pion

//| This flag is set if the compiler bugs with special features with IEEE 754
//| handling, concerning is_valid() and is_finite() testing of infinity and
//| nans.  The workaround is to use bitfield operations.
//| At least VC++, Borland and PGCC have the bug.

bool
is_valid(double d)
{ return (d == d); }
                                                                                                                             
bool
is_finite(double d)
{ return (d == d) && is_valid(d-d); }

int main()
{
  double zero = 0;
  double inf = 1/zero;
  double nan = zero*inf;

  bool b = true;
  b = b &&  is_valid(inf);
  b = b && !is_valid(nan);
  b = b && !is_finite(inf);
  b = b && !is_finite(nan);

  if (!b)
    return -1;
  return 0;
}
