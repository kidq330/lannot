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

#define __lannot_start_inline() __LANNOTATE_START_INLINE(__func__)
#define __lannot_end_inline() __LANNOTATE_END_INLINE(__func__)
#define __cm_start() __CM_START()
#define __cm_double_if() __CM_DOUBLE_IF()
#define __cm_target() __CM_TARGET(0)
#define __cm_step() __CM_TARGET(1)

/*@assigns \nothing;*/
void __LANNOTATE_START_INLINE(const char * fun_name) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __LANNOTATE_END_INLINE(const char * fun_name) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __CM_START(void) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __CM_DOUBLE_IF(void) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __CM_TARGET(unsigned int step) __attribute((FC_BUILTIN));

#endif
