/**************************************************************************/
/*                                                                        */
/*  This file is part of the Frama-C's Lannotate plug-in.                 */
/*                                                                        */
/*  Copyright (C) 2012-2022                                               */
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
/*  for more details (enclosed in the file LICENSE)                       */
/*                                                                        */
/**************************************************************************/

#ifndef __LABELS_LANNOT_H__
#define __LABELS_LANNOT_H__

#define __cm_start() __CM_START()
#define __cm_end() __CM_END()
#define __cm_double_if() __CM_DOUBLE_IF()
#define __cm_ignore_if() __CM_IGNORE_IF()
#define __cm_target() __CM_TARGET()

/*@assigns \nothing;*/
void __CM_START(void) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __CM_END(void) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __CM_DOUBLE_IF(void) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __CM_IGNORE_IF(void) __attribute((FC_BUILTIN));
/*@assigns \nothing;*/
void __CM_TARGET(void) __attribute((FC_BUILTIN));

#endif
