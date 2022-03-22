(**************************************************************************)
(*                                                                        *)
(*  This file is part of the Frama-C's Lannotate plug-in.                 *)
(*                                                                        *)
(*  Copyright (C) 2012-2022                                               *)
(*    CEA (Commissariat à l'énergie atomique et aux énergies              *)
(*         alternatives)                                                  *)
(*                                                                        *)
(*  you can redistribute it and/or modify it under the terms of the GNU   *)
(*  Lesser General Public License as published by the Free Software       *)
(*  Foundation, version 2.1.                                              *)
(*                                                                        *)
(*  It is distributed in the hope that it will be useful,                 *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU Lesser General Public License for more details.                   *)
(*                                                                        *)
(*  See the GNU Lesser General Public License version 2.1                 *)
(*  for more details (enclosed in the file LICENSE)                       *)
(*                                                                        *)
(**************************************************************************)

(** options of the plugin. *)

include Plugin.S

(** value of option [-lannot] *)
module Annotators: Parameter_sig.String_set

(** value of option [-lannot-o] *)
module Output: Parameter_sig.String

(** value of option [-lannot-simplify] *)
module Simplify: Parameter_sig.Bool

(** value of option [-lannot-functions] *)
module DoFunctionNames: Parameter_sig.Kernel_function_set

(** value of option [-lannot-skip-functions] *)
module SkipFunctionNames: Parameter_sig.Kernel_function_set

(** value of option [-lannot-vars] *)
module DoVariableNames: Parameter_sig.String_set

(** value of option [-lannot-skip-vars] *)
module SkipVariableNames: Parameter_sig.String_set

(** value of option [-lannot-list] *)
module ListAnnotators: Parameter_sig.Bool

(** value of option [-lannot-allbool] *)
module AllBoolExps: Parameter_sig.Bool

(** value of option [-lannot-n] *)
module N: Parameter_sig.Int

(** value of option [-lannot-mutators] *)
module Mutators: Parameter_sig.String_set

(** value of option [-lannot-maxwidth] *)
module MaxWidth: Parameter_sig.Int

(** value of option [-lannot-maxdepth] *)
module MaxDepth: Parameter_sig.Int

(** value of option [-lannot-allfuns] *)
module AllFuns: Parameter_sig.Bool

(** value of option [-lannot-globals] *)
module GlobalsAsInput: Parameter_sig.Bool

(** value of option [-lannot-limit-delta] *)
module LimitDelta: Parameter_sig.Int

(** value of option [-lannot-bound-postpone] *)
module BoundPostpone: Parameter_sig.Bool

(** value of option [-lannot-inline] *)
module Inline: Parameter_sig.Bool

(** value of option [-lannot-inline-block] *)
module InlinedBlock: Parameter_sig.Bool

(** value of option [-lannot-clean] *)
module CleanDataflow: Parameter_sig.Bool

(** value of option [-lannot-clean-equiv] *)
module CleanEquiv: Parameter_sig.Bool

(** value of option [-lannot-maxpath] *)
module MaxContextPath: Parameter_sig.Int

(** value of option [-lannot-visibility] *)
module Visibility: Parameter_sig.Bool

(** value of option [-lannot-handle-dowhile] *)
module HandleDoWhile: Parameter_sig.Bool

(** value of option [-lannot-handle-struct] *)
module HandleStruct: Parameter_sig.Bool

(** value of option [-lannot-max-mutation] *)
module MaxMutation: Parameter_sig.Int
