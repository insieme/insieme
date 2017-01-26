// Copyright (c) 2010  INRIA Sophia-Antipolis (France).
// All rights reserved.
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
// $URL: svn+ssh://scm.gforge.inria.fr/svn/cgal/branches/releases/CGAL-4.0-branch/Installation/config/testfiles/CGAL_CFG_NO_CPP0X_INITIALIZER_LISTS.cpp $
// $Id: CGAL_CFG_NO_CPP0X_INITIALIZER_LISTS.cpp 67093 2012-01-13 11:22:39Z lrineau $
//
// Author(s)     : Sylvain Pion

//| If a compiler does not support C++0x initializer lists
//| CGAL_CFG_NO_CPP0X_INITIALIZER_LISTS is set. 

#include <initializer_list>
#include <vector>
#include <string>
#include <complex>

template < typename T >
void use(const T&) {}

struct S
{
  S(std::initializer_list<double> l)
  {
    std::vector<double> v(l.begin(), l.end());
    use(v);
  }
};

int main()
{
  int a = {1};
  std::complex<double> z{1,2};
  std::vector<std::string> v{"once", "upon", "a", "time"}; // 4 string elements
  use(a); use(z); use(v);
  return 0;
}
