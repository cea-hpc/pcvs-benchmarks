/******************************************************************************
 *
 *   Copyright © International Business Machines  Corp., 2009
 *
 *   This program is free software;  you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY;  without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See
 *   the GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program;  if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 *
 * NAME
 *      atomic.h
 *
 * DESCRIPTION
 *      GCC atomic builtin wrappers
 *      http://gcc.gnu.org/onlinedocs/gcc-4.1.0/gcc/Atomic-Builtins.html
 *
 * AUTHOR
 *      Darren Hart <dvhltc@us.ibm.com>
 *
 * HISTORY
 *      2009-Nov-17: Initial version by Darren Hart <dvhltc@us.ibm.com>
 *
 *****************************************************************************/

#ifndef _ATOMIC_H
#define _ATOMIC_H

typedef struct {
	volatile int val;
} atomic_t;

#define ATOMIC_INITIALIZER { 0 }

/**
 * atomic_cmpxchg() - Atomic compare and exchange
 * @uaddr:	The address of the futex to be modified
 * @oldval:	The expected value of the futex
 * @newval:	The new value to try and assign the futex
 *
 * Return the old value of addr->val.
 */
static inline int
atomic_cmpxchg(atomic_t *addr, int oldval, int newval)
{
	return __sync_val_compare_and_swap(&addr->val, oldval, newval);
}

/**
 * atomic_inc() - Atomic incrememnt
 * @addr:	Address of the variable to increment
 *
 * Return the new value of addr->val.
 */
static inline int
atomic_inc(atomic_t *addr)
{
	return __sync_add_and_fetch(&addr->val, 1);
}

/**
 * atomic_dec() - Atomic decrement
 * @addr:	Address of the variable to decrement
 *
 * Return the new value of addr-val.
 */
static inline int
atomic_dec(atomic_t *addr)
{
	return __sync_sub_and_fetch(&addr->val, 1);
}

/**
 * atomic_set() - Atomic set
 * @addr:	Address of the variable to set
 * @newval:	New value for the atomic_t
 *
 * Return the new value of addr->val.
 */
static inline int
atomic_set(atomic_t *addr, int newval)
{
	addr->val = newval;
	return newval;
}

#endif
