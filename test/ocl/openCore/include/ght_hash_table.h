/*-*-c-*- ************************************************************
 * Copyright (C) 2001-2005,  Simon Kagstrom
 *
 * Filename:      ght_hash_table.h.in
 * Description:   The definitions used in the hash table.
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
 * $Id: ght_hash_table.h.in 15761 2007-07-15 06:08:52Z ska $
 *
 ********************************************************************/

/**
 * @file
 * libghthash is a generic hash table used for storing arbitrary
 * data.
 *
 * Libghthash really stores pointers to data - the hash
 * table knows nothing about the actual type of the data.
 *
 * A simple example to get started can be found in the
 * <TT>example/simple.c</TT> file found in the distribution.
 * <TT>hash_test.c</TT> provides a more comlpete example.
 *
 * Some basic properties of the hash table are:
 *
 * - Both the data stored and the keys are of void type, which
 *   means that you can store any kind of data.
 *
 * - The only functions you probably will need to start is:
 *   - ght_create(), which creates a new hash table.
 *   - ght_insert(), which inserts a new entry into a table.
 *   - ght_get(), which searches for an entry.
 *   - ght_remove(), which removes and entry.
 *   - ght_finalize(), which destroys a hash table.
 *
 * - Inserting entries is done without first creating a key,
 *   i.e. you insert with the data, the datasize, the key and the
 *   key size directly.
 *
 * - The hash table copies the key data when inserting new
 *   entries. This means that you should <I>not</I> malloc() the key
 *   before inserting a new entry.
 *
 */
#ifndef GHT_HASH_TABLE_H
#define GHT_HASH_TABLE_H

#include <stdlib.h>                    /* size_t */

#ifdef __cplusplus
extern "C" {
#endif

#define GHT_HEURISTICS_NONE          0
#define GHT_HEURISTICS_TRANSPOSE     1
#define GHT_HEURISTICS_MOVE_TO_FRONT 2
#define GHT_AUTOMATIC_REHASH         4

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

/** unsigned 32 bit integer. */
typedef unsigned int ght_uint32_t;

/**
 * The structure for hash keys. You should not care about this
 * structure unless you plan to write your own hash functions.
 */
typedef struct s_hash_key
{
  unsigned int i_size;       /**< The size in bytes of the key p_key */
  const void *p_key;         /**< A pointer to the key. */
} ght_hash_key_t;

/*
 * The structure for hash entries.
 *
 * LOCK: Should be possible to do somewhat atomically
 */
typedef struct s_hash_entry
{
  void *p_data;

  struct s_hash_entry *p_next;
  struct s_hash_entry *p_prev;
  struct s_hash_entry *p_older;
  struct s_hash_entry *p_newer;
  ght_hash_key_t key;

} ght_hash_entry_t;

/*
 * The structure used in iterations. You should not care about the
 * contents of this, it will be filled and updated by ght_first() and
 * ght_next().
 */
typedef struct
{
  ght_hash_entry_t *p_entry; /* The current entry */
  ght_hash_entry_t *p_next;  /* The next entry */
} ght_iterator_t;

/**
 * Definition of the hash function pointers. @c ght_fn_hash_t should be
 * used when implementing new hash functions. Look at the supplied
 * hash functions, like @c ght_one_at_a_time_hash(), for examples of hash
 * functions.
 *
 * @param p_key the key to calculate the hash value for.
 *
 * @return a 32 bit hash value.
 *
 * @see @c ght_one_at_a_time_hash(), @c ght_rotating_hash(),
 *      @c ght_crc_hash()
 */
typedef ght_uint32_t (*ght_fn_hash_t)(ght_hash_key_t *p_key);

/**
 * Definition of the allocation function pointers. This is simply the
 * same definition as @c malloc().
 *
 * @param size the size to allocate. This will always be
 *        <TT>sizeof(ght_hash_entry_t) + key_size</TT>.
 *
 * @return a pointer to the allocated region, or NULL if the
 *         allocation failed.
 */
typedef void *(*ght_fn_alloc_t)(size_t size);

/**
 * Definition of the deallocation function pointers. This is simply the
 * same definition as @c free().
 *
 * @param ptr a pointer to the region to free.
 */
typedef void (*ght_fn_free_t)(void *ptr);

/**
 * Definition of bounded bucket free callback function pointers.
 *
 * The keys is passed back as const, since it was accepted by ght_insert()
 * as const, but if the callback function knows that a non-const pointer
 * was passed in, it can cast it back to non-const.
 */
typedef void (*ght_fn_bucket_free_callback_t)(void *data, const void *key);

/**
 * The hash table structure.
 */
typedef struct
{
  unsigned int i_items;              /**< The current number of items in the table */
  unsigned int i_size;               /**< The number of buckets */

  /* private: */
  ght_hash_entry_t **pp_entries;
  unsigned int *p_nr;                /* The number of entries in each bucket */
  int i_size_mask;                   /* The number of bits used in the size */
  unsigned int bucket_limit;

  ght_hash_entry_t *p_oldest;        /* The entry inserted the earliest. */
  ght_hash_entry_t *p_newest;        /* The entry inserted the latest. */
} ght_hash_table_t;

/**
 * Create a new hash table. The number of buckets should be about as
 * big as the number of elements you wish to store in the table for
 * good performance. The number of buckets is rounded to the next
 * higher power of two.
 *
 * The hash table is created with @c ght_one_at_a_time_hash() as hash
 * function, automatic rehashing disabled, @c malloc() as the memory
 * allocator and no heuristics.
 *
 * @param i_size the number of buckets in the hash table. Giving a
 *        non-power of two here will round the size up to the next
 *        power of two.
 *
 * @see ght_set_hash(), ght_set_heuristics(), ght_set_rehash(),
 * @see ght_set_alloc()
 *
 * @return a pointer to the hash table or NULL upon error.
 */
ght_hash_table_t *ght_create(unsigned int i_size);


/**
 * Get the size (the number of items) of the hash table.
 *
 * @param p_ht the hash table to get the size for.
 *
 * @return the number of items in the hash table.
 */
unsigned int ght_size(ght_hash_table_t *p_ht);

/**
 * Insert an entry into the hash table. Prior to inserting anything,
 * make sure that the table is created with ght_create(). If an
 * element with the same key as this one already exists in the table,
 * the insertion will fail and -1 is returned.
 *
 * A typical example is shown below, where the string "blabla"
 * (including the '\0'-terminator) is used as a key for the integer
 * 15.
 *
 * <PRE>
 * ght_hash_table_t *p_table;
 * char *p_key_data;
 * int *p_data;
 * int ret;
 *
 * [Create p_table etc...]
 * p_data = malloc(sizeof(int));
 * p_key_data = "blabla";
 * *p_data = 15;
 *
 * ret = ght_insert(p_table,
 *                  p_data,
 *                  sizeof(char)*(strlen(p_key_data)+1), p_key_data);
 * </PRE>
 *
 * @param p_ht the hash table to insert into.
 * @param p_entry_data the data to insert.
 * @param i_key_size the size of the key to associate the data with (in bytes).
 * @param p_key_data the key to use. The value will be copied, and it
 *                   is therefore OK to use a stack-allocated entry here.
 *
 * @return 0 if the element could be inserted, -1 otherwise.
 */
int ght_insert(ght_hash_table_t *p_ht,
	       void *p_entry_data,
	       unsigned int i_key_size, const void *p_key_data);


/**
 * Lookup an entry in the hash table. The entry is <I>not</I> removed from
 * the table.
 *
 * @param p_ht the hash table to search in.
 * @param i_key_size the size of the key to search with (in bytes).
 * @param p_key_data the key to search for.
 *
 * @return a pointer to the found entry or NULL if no entry could be found.
 */
void *ght_get(ght_hash_table_t *p_ht,
	      unsigned int i_key_size, const void *p_key_data);

/**
 * Return the first entry in the hash table. This function should be
 * used for iteration and is used together with ght_next(). The order
 * of the entries will be from the oldest inserted entry to the newest
 * inserted entry. If an entry is inserted during an iteration, the entry
 * might or might not occur in the iteration. Note that removal during
 * an iteration is only safe for the <I>current</I> entry or an entry
 * which has <I>already been iterated over</I>.
 *
 * The use of the ght_iterator_t allows for several concurrent
 * iterations, where you would use one ght_iterator_t for each
 * iteration. In threaded environments, you should still lock access
 * to the hash table for insertion and removal.
 *
 * A typical example might look as follows:
 * <PRE>
 * ght_hash_table_t *p_table;
 * ght_iterator_t iterator;
 * void *p_key;
 * void *p_e;
 *
 * [Create table etc...]
 * for(p_e = ght_first(p_table, &iterator, &p_key); p_e; p_e = ght_next(p_table, &iterator, &p_key))
 *   {
 *      [Do something with the current entry p_e and it's key p_key]
 *   }
 * </PRE>
 *
 * @param p_ht the hash table to iterate through.
 *
 * @param p_iterator the iterator to use. The value of the structure
 * is filled in by this function and may be stack allocated.
 *
 * @param pp_key a pointer to the pointer of the key (NULL if none).
 *
 * @return a pointer to the first entry in the table or NULL if there
 * are no entries.
 *
 *
 * @see ght_next()
 */
void *ght_first(ght_hash_table_t *p_ht, ght_iterator_t *p_iterator, const void **pp_key);

/**
 * Return the next entry in the hash table. This function should be
 * used for iteration, and must be called after ght_first().
 *
 * @warning calling this without first having called ght_first will
 * give undefined results (probably a crash), since p_iterator isn't
 * filled correctly.
 *
 * @param p_ht the hash table to iterate through.
 *
 * @param p_iterator the iterator to use.
 *
 * @param pp_key a pointer to the pointer of the key (NULL if none).
 *
 * @return a pointer to the next entry in the table or NULL if there
 * are no more entries in the table.
 *
 * @see ght_first()
 */
void *ght_next(ght_hash_table_t *p_ht, ght_iterator_t *p_iterator, const void **pp_key);

/**
 * Free the hash table. ght_finalize() should typically be called
 * at the end of the program. Note that only the metadata and the keys
 * of the table is freed, not the entries. If you want to free the
 * entries when removing the table, the entries will have to be
 * manually freed before ght_finalize() is called like:
 *
 * <PRE>
 * ght_iterator_t iterator;
 * void *p_key;
 * void *p_e;
 *
 * for(p_e = ght_first(p_table, &iterator, &p_key); p_e; p_e = ght_next(p_table, &iterator, &p_key))
 *   {
 *     free(p_e);
 *   }
 *
 * ght_finalize(p_table);
 * </PRE>
 *
 * @param p_ht the table to remove.
 */
void ght_finalize(ght_hash_table_t *p_ht);

/* exported hash functions */

/**
 * One-at-a-time-hash. One-at-a-time-hash is a good hash function, and
 * is the default when ght_create() is called with NULL as the
 * fn_hash parameter. This was found in a DrDobbs article, see
 * http://burtleburtle.net/bob/hash/doobs.html
 *
 * @warning Don't call this function directly, it is only meant to be
 * used as a callback for the hash table.
 *
 * @see ght_fn_hash_t
 * @see ght_rotating_hash(), ght_crc_hash()
 */
ght_uint32_t ght_one_at_a_time_hash(ght_hash_key_t *p_key);


#ifdef USE_PROFILING
/**
 * Print some statistics about the table. Only available if the
 * library was compiled with <TT>USE_PROFILING</TT> defined.
 */
void ght_print(ght_hash_table_t *p_ht);
#endif

#ifdef __cplusplus
}
#endif

#endif /* GHT_HASH_TABLE_H */
