// Copyright (c) 2009  INRIA Sophia-Antipolis (France).
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
// $URL: svn+ssh://scm.gforge.inria.fr/svn/cgal/branches/releases/CGAL-4.0-branch/Installation/config/testfiles/CGAL_CFG_ARRAY_MEMBER_INITIALIZATION_BUG.cpp $
// $Id: CGAL_CFG_ARRAY_MEMBER_INITIALIZATION_BUG.cpp 67093 2012-01-13 11:22:39Z lrineau $
//
// Author(s)     : Sylvain Pion

//| g++ 4.4.0 and 4.4.1 have a bug here.  Most probably VC++ as well.

// Note : it appears that this is a GNU Extension.  We might want to update
// this using C++0x features if there is anything related.

struct M
{
  M() {}
  ~M() {}
};

struct S
{
  M m[1];

  S ()
   : m ((M[1]) { M () }) {}
};

int main()
{
  S s;
  return 0;
}
