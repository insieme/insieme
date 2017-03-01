// Copyright (c) 2009  GeometryFactory Sarl (France).
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
// $URL: svn+ssh://scm.gforge.inria.fr/svn/cgal/branches/releases/CGAL-4.0-branch/Installation/config/testfiles/CGAL_CFG_NO_LOGICAL_OPERATORS_ALTERNATIVES.cpp $
// $Id: CGAL_CFG_NO_LOGICAL_OPERATORS_ALTERNATIVES.cpp 67093 2012-01-13 11:22:39Z lrineau $
//
// Author(s)     : Laurent Rineau

//| If a compiler does not support the alternative tokens for logicial
//| operators (section 2.5 Alternative tokens [lex.digraph] of the C++
//| norm, 2003), then CGAL_CFG_NO_LOGICAL_OPERATORS_ALTERNATIVES is set.

int main()
{
  if( true and (not false) )
    if(1 not_eq 2)
      if(false or true)
        return 0;

  return 1;
}
