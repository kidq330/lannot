/**************************************************************************/
/*                                                                        */
/*  This file is part of Frama-C.                                         */
/*                                                                        */
/*  Copyright (C) 2007-2020                                               */
/*    CEA (Commissariat à l'énergie atomique et aux énergies              */
/*         alternatives)                                                  */
/*                                                                        */
/*  you can redistribute it and/or modify it under the terms of the GNU   */
/*  Lesser General Public License as published by the Free Software       */
/*  Foundation, version 2.1.                                              */
/*                                                                        */
/*  It is distributed in the hope that it will be useful,                 */
/*  but WITHOUT ANY WARRANTY; without even the implied warranty of        */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         */
/*  GNU Lesser General Public License for more details.                   */
/*                                                                        */
/*  See the GNU Lesser General Public License version 2.1                 */
/*  for more details (enclosed in the file licenses/LGPLv2.1).            */
/*                                                                        */
/**************************************************************************/

#ifndef __LABELS_LANNOT_H__
#define __LABELS_LANNOT_H__

#define MKFN(fn, ...) MKFN_N(fn, ##__VA_ARGS__, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0) \
(__VA_ARGS__)
#define MKFN_N(fn, n0, n1, n2, n3, n4, n5, n6, n7, n8, n, ...) fn##n
#define DEFAULT_ID 0

#define lannot_success(...) MKFN(lannot_success, ##__VA_ARGS__)
#define lannot_success_inter(...) MKFN(lannot_success_inter, ##__VA_ARGS__)
#define lannot_simple(...) MKFN(lannot_simple, ##__VA_ARGS__)
#define lannot_double(...) MKFN(lannot_double, ##__VA_ARGS__)

#define lannot_success0() __LANNOTATE_SUCCESS(1, DEFAULT_ID)
#define lannot_success1(id) __LANNOTATE_SUCCESS(1, id)
#define lannot_success_inter0() __LANNOTATE_SUCCESS(0, DEFAULT_ID)
#define lannot_success_inter1(id) __LANNOTATE_SUCCESS(0, id)
#define lannot_simple0() __LANNOTATE_SIMPLE(DEFAULT_ID)
#define lannot_simple1(id) __LANNOTATE_SIMPLE(id)
#define lannot_double0() __LANNOTATE_DOUBLE(DEFAULT_ID)
#define lannot_double1(id) __LANNOTATE_DOUBLE(id)

/*@assigns \nothing;*/
void __LANNOTATE_SUCCESS(unsigned int clean, unsigned int id) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __LANNOTATE_SIMPLE(unsigned id) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __LANNOTATE_DOUBLE(unsigned id) __attribute((FC_BUILTIN));

#endif
