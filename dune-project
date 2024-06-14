(lang dune 3.7)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                                                        ;;
;;  This file is part of the Frama-C's Lannotate plug-in.                 ;;
;;                                                                        ;;
;;  Copyright (C) 2012-2022                                               ;;
;;    CEA (Commissariat à l'énergie atomique et aux énergies              ;;
;;         alternatives)                                                  ;;
;;                                                                        ;;
;;  you can redistribute it and/or modify it under the terms of the GNU   ;;
;;  Lesser General Public License as published by the Free Software       ;;
;;  Foundation, version 2.1.                                              ;;
;;                                                                        ;;
;;  It is distributed in the hope that it will be useful,                 ;;
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of        ;;
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         ;;
;;  GNU Lesser General Public License for more details.                   ;;
;;                                                                        ;;
;;  See the GNU Lesser General Public License version 2.1                 ;;
;;  for more details (enclosed in the file LICENSE)                       ;;
;;                                                                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(using dune_site 0.1)
(generate_opam_files true)

(name frama-c-lannotate)
(package (name frama-c-lannotate)
  (depends
    (ocaml (>= 4.13.1))
    (dune (<> 3.13.0)) ; performance problem
    (frama-c (and (>= 29.0~) (< 30.0~)))
  )
)
