GenLabels
=========

GenLabels is a Frama-C plugin. It requires a patched version of Frama-C.

Installation
------------

Once Frama-C is installed, compile and install GenLabels:

    make
    make install

The former command may need to be run as root (or sudo) depending on your Frama-C installation.

Usage
-----

    frama-c -genlabels=CRITERIA file.c

where CRITERIA is a comma-separated list of criteria. It outputs a new annotated file named `file_labels.c`, with labels for each selected criterion.

Implemented criteria are CC, MCC, WM, IDP, F and D.

### CC (Conditition Coverage)

    frama-c -genlabels=CC file.c

This command creates an annoted file with condition coverage labels only.

For instance, if `file.c` contains:

    if (a <= b && c >= b) {

`file_labels.c` may contains:

    pc_label(a <= b,1,"CC");
    pc_label(! (a <= b),2,"CC");
    pc_label(c >= b,3,"CC");
    pc_label(! (c >= b),4,"CC");
    if (a <= b && c >= b) {

Note that the second parameter of `pc_label` may vary, it's a unique identifier for each label.

### MCC (Multiple Condition Coverage)

    frama-c -genlabels=MCC file.c

The following branch:

    if (a <= b && c >= b) {

becomes:

    pc_label(! (a <= b) && ! (c >= b),1,"MCC");
    pc_label(! (a <= b) && c >= b,2,"MCC");
    pc_label(a <= b && ! (c >= b),3,"MCC");
    pc_label(a <= b && c >= b,4,"MCC");
    if (a <= b && c >= b) {

### WM (Weak Mutation)

    frama-c -genlabels=WM file.c

This command creates an annoted file with labels corresponding to every available mutators.
One can select more precisely mutators, like so:

    frama-c -genlabels=WM -genlabels-mutators=AOR,COR file.c

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

### Authors

Omar Chebaro
Mickaël Delahaye, CEA

### License

TODO
