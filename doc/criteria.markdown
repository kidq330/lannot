LAnnotate's Criteria
====================

LAnnotate implements a wide range of coverage criteria.

Logical criteria
----------------

Here are the coverage criteria that play with Boolean expressions.

By default, only Boolean expression present in conditional constructs and loop
headers are taken into account. However with the parameter `-lannot-allbool`,
boolean expression embedded into any other statement (assignment or function
call) are also considered.

### DC (Decision Coverage)

The following example:

```c
if (a && || c) ...
```

becomes:

```c
pc_label(a, 1, "DC");
pc_label(a, 2, "DC");
if (a && || c) ...
```

Note that the second parameter of `pc_label` may vary, it's a unique identifier
for each label.

This criterion supports the `-lannot-allbool` flag.

### CC (Conditition Coverage)


The following example:
```c
if (a <= b && c >= b) {
```

becomes:

```c
pc_label(a <= b,1,"CC");
pc_label(! (a <= b),2,"CC");
pc_label(c >= b,3,"CC");
pc_label(! (c >= b),4,"CC");
if (a <= b && c >= b) {
```

This criterion supports the `-lannot-allbool` flag.

### MCC (Multiple Condition Coverage)

The following branch:

```c
if (a <= b && c >= b) {
```

becomes:

```c
pc_label(! (a <= b) && ! (c >= b),1,"MCC");
pc_label(! (a <= b) && c >= b,2,"MCC");
pc_label(a <= b && ! (c >= b),3,"MCC");
pc_label(a <= b && c >= b,4,"MCC");
if (a <= b && c >= b) {
```

This criterion supports the `-lannot-allbool` flag.

### NCC (n-wise Condition Coverage)

Pragmatic multiple condition coverage, the number of atomic condition in a
single label is limited to some n (by default 2).

    frama-c -lannot=NCC -lannot-n 2 file.c

The following branch:

```c
if (a && b || c) ...
```

becomes:

```c
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
if (a && b || c) ...
```

The criterion also supports the `-lannot-allbool` flag.


### GACC (General Active Clause Coverage)

Weak MCDC, requires two labels by atomic condition in every decision.
See [Ammann & Offut, p109].

The following branch:

```c
if (a && || c) ...
```

becomes:

```c
pc_label(a && ((! (b || c) && c) || ((b || c) && ! c)),1,"GACC");
pc_label(! a && ((! (b || c) && c) || ((b || c) && ! c)),2,"GACC");
pc_label(b && ((! (a || c) && c) || ((a || c) && ! c)),3,"GACC");
pc_label(! b && ((! (a || c) && c) || ((a || c) && ! c)),4,"GACC");
pc_label(c && ! (a && b),5,"GACC");
pc_label(! c && ! (a && b),6,"GACC");
if (a && || c) ...
```

Each label includes two parts:
  - the atomic condition or its negation;
  - the activity condition (inequality of positive and negative
    Shannon's cofactors w.r.t. to the atom).

Note that the Boolean inequality of the activity condition `F⁺ ≠ F⁻` is
encoded as `(F⁺&&!F⁻) || (!F⁺&&F⁻)` to allow for more simplifications with
`-lannot-simplify`.

Also supports `-lannot-allbool` in addition to `-lannot-simplify`.

### GICC (General Inactive Clause Coverage)

Requires four labels by atomic condition.
See [Ammann & Offut, p112].

The following branch:

```c
if (a && || c) ...
```

becomes:

```c
int __retres;
pc_label((a && ((! (b || c) || (b || c)) && (! (b || c) || (b || c)))) &&
  ((a && b) || c),1,"GICC");
pc_label((a && ((! (b || c) || (b || c)) && (! (b || c) || (b || c)))) &&
  ! ((a && b) || c),2,"GICC");
pc_label((! a && ((! (b || c) || (b || c)) && (! (b || c) || (b || c)))) &&
  ((a && b) || c),3,"GICC");
pc_label((! a && ((! (b || c) || (b || c)) && (! (b || c) || (b || c)))) &&
  ! ((a && b) || c),4,"GICC");
pc_label((b && ((! (a || c) || (a || c)) && (! (a || c) || (a || c)))) &&
  ((a && b) || c),5,"GICC");
pc_label((b && ((! (a || c) || (a || c)) && (! (a || c) || (a || c)))) &&
  ! ((a && b) || c),6,"GICC");
pc_label((! b && ((! (a || c) || (a || c)) && (! (a || c) || (a || c)))) &&
  ((a && b) || c),7,"GICC");
pc_label((! b && ((! (a || c) || (a || c)) && (! (a || c) || (a || c)))) &&
  ! ((a && b) || c),8,"GICC");
pc_label(c && ((a && b) || c),9,"GICC");
pc_label(c && ! ((a && b) || c),10,"GICC");
pc_label(! c && ((a && b) || c),11,"GICC");
pc_label(! c && ! ((a && b) || c),12,"GICC");
if (a && || c) ...
```

Each label includes two parts:
  - the atomic condition or its negation;
  - the inactivity condition (equality of positive and negative
    Shannon's cofactors w.r.t. to the atom).

Note that the Boolean inequality of the inactivity condition `F⁺ = F⁻` is
encoded as `(F⁺||!F⁻) && (F⁻||!F⁺)` to allow for more simplifications with
`-lannot-simplify`.

Also supports `-lannot-allbool` in addition to `-lannot-simplify`.


Others
------

### WM (Weak Mutation)

    frama-c -lannot=WM file.c

This command creates an annoted file with labels corresponding to every
available mutators. One can select more precisely mutators, like so:

    frama-c -lannot=WM -lannot-mutators=AOR,COR file.c

Available mutators are ABS, AOR, COR and ROR.


### IDP (Input Domain Partition)

TODO

  - `-lannot-maxwidth width`
  - `-lannot-maxheight height`
  - `-lannot-allfuns`
  - `-lannot-globals`

### FC (Function Coverage)

The following example:

```c
void f() {
  ...
}
```

will be instrumented as follows:

```c
void f() {
  pc_label(TRUE,1,"F");
  ...
}
```
