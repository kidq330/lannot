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

#ifndef __LABELS_H__
#define __LABELS_H__

#define lannot_success(id) __LANNOTATE_SUCCESS(id)
#define lannot_simple(id) __LANNOTATE_SIMPLE(id)
#define lannot_double(id) __LANNOTATE_DOUBLE(id)

/*@assigns \nothing;*/
void __LANNOTATE_SUCCESS(int id) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __LANNOTATE_SIMPLE(int id) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __LANNOTATE_DOUBLE(int id) __attribute((FC_BUILTIN));

#endif
