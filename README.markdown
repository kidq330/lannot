Frama-C/LTest: LAnnotate
========================

Frama-C/LTest (or LTest for short) is a generic and integrated toolkit for
automation of white-box testing of C programs. This toolkit provides a unified
support of many different testing criteria as well as an easy integration of
new criteria. *LAnnotate* is the module of LTest in charge of adding coverage
objectives into program in the form of annotations.

LAnnotate is a Frama-C plugin. It requires a patched version of Frama-C. The
patch, as well as an already patched Frama-C, are available for download online
at http://micdel.fr/ltest.html .

Installation
------------

Once Frama-C is installed, compile and install GenLabels:

    make
    make install

The former command may need to be run as root (or sudo) depending on your
Frama-C installation.

Usage
-----

    frama-c -lannot=CRITERIA file.c

where CRITERIA is a comma-separated list of criteria. It outputs a new
annotated file named `file_labels.c`, with labels for each selected criterion.

Implemented criteria are CC, MCC, WM, IDP, F and D.

### CC (Conditition Coverage)

    frama-c -lannot=CC file.c

This command creates an annoted file with condition coverage labels only.

For instance, if `file.c` contains:

    if (a <= b && c >= b) {

`file_labels.c` may contains:

    pc_label(a <= b,1,"CC");
    pc_label(! (a <= b),2,"CC");
    pc_label(c >= b,3,"CC");
    pc_label(! (c >= b),4,"CC");
    if (a <= b && c >= b) {

Note that the second parameter of `pc_label` may vary, it's a unique identifier
for each label.

By default, only boolean expression present in conditional constructs and loop
headers are taken into account. However with the parameter `-lannot-allbool`,
boolean expression embedded into any other statements (assignments or function
calls) are also considered.

### MCC (Multiple Condition Coverage)

    frama-c -lannot=MCC file.c

The following branch:

    if (a <= b && c >= b) {

becomes:

    pc_label(! (a <= b) && ! (c >= b),1,"MCC");
    pc_label(! (a <= b) && c >= b,2,"MCC");
    pc_label(a <= b && ! (c >= b),3,"MCC");
    pc_label(a <= b && c >= b,4,"MCC");
    if (a <= b && c >= b) {

This criterion also supports the `-lannot-allbool` flag.

### nCC (n-wise Condition Coverage)

Pragmatic multiple condition coverage, the number of atomic condition in a
single label is limited to some n (by default 2).

    frama-c -lannot=NCC -lannot-n 2 file.c

The following branch:

    if (a && b || c) {

becomes:

    pc_label(a && b,1,"NCC");
    pc_label(a && ! b,2,"NCC");
    pc_label(! a && b,3,"NCC");
    pc_label(! a && ! b,4,"NCC");
    pc_label(a && c,5,"NCC");
    pc_label(a && ! c,6,"NCC");
    pc_label(! a && c,7,"NCC");
    pc_label(! a && ! c,8,"NCC");
    pc_label(b && c,9,"NCC");
    pc_label(b && ! c,10,"NCC");
    pc_label(! b && c,11,"NCC");
    pc_label(! b && ! c,12,"NCC");
    if (a && b || c) {

The criterion also supports the `-lannot-allbool` flag.

### WM (Weak Mutation)

    frama-c -lannot=WM file.c

This command creates an annoted file with labels corresponding to every
available mutators. One can select more precisely mutators, like so:

    frama-c -lannot=WM -lannot-mutators=AOR,COR file.c

Available mutators are ABS, AOR, COR and ROR.


### IDP (Input Domain Partition)

TODO

### FC (Function Coverage)

The following example:

    void f() {
      ...
    }

will be instrumented as follows:

    void f() {
      pc_label(TRUE,1,"F");
      ...
    }


### DC (Decision Coverage)

The following example:

    if (a <=b && c >= b) {
      ...
    } else {
      ...
    }

becomes:

    if (a <=b && c >= b) {
      pc_label(TRUE,1,"D");
      ...
    } else {
      pc_label(TRUE,2,"D");
      ...
    }

Authors
-------

Omar Chebaro
Mickaël Delahaye
Nikolai Kosmatov
Sébastien Bardin

License
-------

This file is part of Frama-C

Copyright (C) 2013-2014
  CEA (Commissariat à l'énergie atomique et aux énergies alternatives)

You may redistribute it and/or modify it under the terms of the GNU
Lesser General Public License as published by the Free Software
Foundation, version 2.1.

It is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

See the GNU Lesser General Public License version 2.1
for more details (enclosed in the file LICENSE).
