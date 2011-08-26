/*********************************************************************
 *
 * Copyright (C) 2001-2002,  Simon Kagstrom
 *
 * Filename:      hash_functions.c
 * Description:   Hash functions
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
 * 02111-1307, USA.
 *
 * $Id: hash_functions.c 2174 2005-03-18 07:00:30Z ska $
 *
 ********************************************************************/
#include <assert.h>

#include "ght_hash_table.h"

/* One-at-a-time hash (found in a web article from ddj), this is the
 * standard hash function.
 *
 * See http://burtleburtle.net/bob/hash/doobs.html
 * for the hash functions used here.
 */
ght_uint32_t ght_one_at_a_time_hash(ght_hash_key_t *p_key)
{
  ght_uint32_t i_hash=0;
  unsigned int i;

  assert(p_key);

  for (i=0; i<p_key->i_size; ++i)
    {
      i_hash += ((unsigned char*)p_key->p_key)[i];
      i_hash += (i_hash << 10);
      i_hash ^= (i_hash >> 6);
    }
  i_hash += (i_hash << 3);
  i_hash ^= (i_hash >> 11);
  i_hash += (i_hash << 15);

  return i_hash;
}

