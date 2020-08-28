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

#define lannot_start() __LANNOTATE_START()
#define lannot_start_inline() __LANNOTATE_START_INLINE(__func__)
#define lannot_end_inline() __LANNOTATE_END_INLINE(__func__)
#define lannot_double() __LANNOTATE_DOUBLE()
#define lannot_success() __LANNOTATE_SUCCESS(1)
#define lannot_success_inter() __LANNOTATE_SUCCESS(0)

/*@assigns \nothing;*/
void __LANNOTATE_START(void) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __LANNOTATE_START_INLINE(const char * fun_name) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __LANNOTATE_END_INLINE(const char * fun_name) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __LANNOTATE_SUCCESS(unsigned int clean) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __LANNOTATE_DOUBLE(void) __attribute((FC_BUILTIN));

#endif
