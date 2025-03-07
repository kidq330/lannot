(**************************************************************************)
(*                                                                        *)
(*  This file is part of Frama-C.                                         *)
(*                                                                        *)
(*  Copyright (C) 2007-2020                                               *)
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
(*  for more details (enclosed in the file licenses/LGPLv2.1).            *)
(*                                                                        *)
(**************************************************************************)

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
if (a && b || c) ...
```
becomes:
```c
pc_label((a && b) || c,2,"DC");
pc_label(! ((a && b) || c),1,"DC");
if (a && b || c) ...
```
Note that the second parameter of `pc_label` may vary, it's a unique identifier
for each label.

This criterion supports the `-lannot-allbool` flag.

### CC (Condition Coverage)

The following example:
```c
if (a <= b && c >= b) ...
```
becomes:
```c
pc_label(a <= b,1,"CC");
pc_label(! (a <= b),2,"CC");
pc_label(c >= b,3,"CC");
pc_label(! (c >= b),4,"CC");
if (a <= b && c >= b) ...
```
This criterion supports the `-lannot-allbool` flag.

### MCC (Multiple Condition Coverage)

The following example:
```c
if (a <= b && c >= b) ...
```
becomes:
```c
pc_label(a <= b && c >= b,1,"MCC");
pc_label(a <= b && ! (c >= b),2,"MCC");
pc_label(! (a <= b) && c >= b,3,"MCC");
pc_label(! (a <= b) && ! (c >= b),4,"MCC");
if (a <= b && c >= b) ...
```
This criterion supports the `-lannot-allbool` flag.

### NCC (n-wise Condition Coverage)

Pragmatic multiple condition coverage, the number of atomic condition in a
single label is limited to some n (by default 2).
`frama-c -lannot=NCC -lannot-n 2 file.c`

The following example:
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

The following example:
```c
if (a && b || c) ...
```
becomes:
```c
pc_label(a && ((! (b || c) && c) || ((b || c) && ! c)),1,"GACC");
pc_label(! a && ((! (b || c) && c) || ((b || c) && ! c)),2,"GACC");
pc_label(b && ((! (a || c) && c) || ((a || c) && ! c)),3,"GACC");
pc_label(! b && ((! (a || c) && c) || ((a || c) && ! c)),4,"GACC");
pc_label(c && ! (a && b),5,"GACC");
pc_label(! c && ! (a && b),6,"GACC");
if (a && b || c) ...
```
Each label includes two parts:
* the atomic condition or its negation;
* the activity condition (inequality of positive and negative
    Shannon's cofactors w.r.t. to the atom).

Note that the Boolean inequality of the activity condition `F⁺ ≠ F⁻` is
encoded as `(F⁺&&!F⁻) || (!F⁺&&F⁻)` to allow for more simplifications with
`-lannot-simplify`.

Also supports `-lannot-allbool` in addition to `-lannot-simplify`.

### GICC (General Inactive Clause Coverage)

Requires four labels by atomic condition.
See [Ammann & Offut, p112].

The following example:
```c
if (a && b || c) ...
```
becomes:
```c
pc_label((a && ((! (b || c) || (b || c)) && (! (b || c) || (b || c)))) && ((a && b) || c),1,"GICC");
pc_label((a && ((! (b || c) || (b || c)) && (! (b || c) || (b || c)))) && ! ((a && b) || c),2,"GICC");
pc_label((! a && ((! (b || c) || (b || c)) && (! (b || c) || (b || c)))) && ((a && b) || c),3,"GICC");
pc_label((! a && ((! (b || c) || (b || c)) && (! (b || c) || (b || c)))) && ! ((a && b) || c),4,"GICC");
pc_label((b && ((! (a || c) || (a || c)) && (! (a || c) || (a || c)))) && ((a && b) || c),5,"GICC");
pc_label((b && ((! (a || c) || (a || c)) && (! (a || c) || (a || c)))) && ! ((a && b) || c),6,"GICC");
pc_label((! b && ((! (a || c) || (a || c)) && (! (a || c) || (a || c)))) && ((a && b) || c),7,"GICC");
pc_label((! b && ((! (a || c) || (a || c)) && (! (a || c) || (a || c)))) && ! ((a && b) || c),8,"GICC");
pc_label(c && ((a && b) || c),9,"GICC");
pc_label(c && ! ((a && b) || c),10,"GICC");
pc_label(! c && ((a && b) || c),11,"GICC");
pc_label(! c && ! ((a && b) || c),12,"GICC");
if (a && b || c) ...
```
Each label includes two parts:
* the atomic condition or its negation;
* the inactivity condition (equality of positive and negative
    Shannon's cofactors w.r.t. to the atom).

Note that the Boolean inequality of the inactivity condition `F⁺ = F⁻` is
encoded as `(F⁺||!F⁻) && (F⁻||!F⁺)` to allow for more simplifications with
`-lannot-simplify`.

Also supports `-lannot-allbool` in addition to `-lannot-simplify`.

### LIMIT (Limit coverage)

The following example:
```c
if (A < B) ...
```
becomes:
```c
pc_label(A < B && ((A - B) + 1 <= N && - ((A - B) + 1) <= N),1,"LIMIT");
if (i < 10) ...
```
`X <= N && -X <= N)` in an equivalent to `abs(X) <= N` when N is positive,
`A (<|<=|=>|>) B` becomes `(A - B) ((+|-) 1)?`, so it is equal to 0 when `i`'s value is the limit.
In this example the condition is true only if `i` equal 9

N can be set using -lannot-limit-delta (default : 0)

Function
--------

### FC (Function Coverage)

The following example:
```c
void function() {
    ...
}
```
will be instrumented as follows:
```c
void function() {
    pc_label(1,1,"FC");
    ...
}
```

### FCC (Function Call Coverage)

The following example:
```c
void main(){
    call_function();
}
```
will be instrumented as follows:
```c
void main() {
    pc_label(1,1,"FCC");
    call_function();
}
```

Loop
----

Here are the criterias relative to loops.

### ELO

This criteria check if we enter at least once in each loop in the program.
the following example:

```c
for(int i = 0; i < 10; i++){
	...
}
```
becomes:
```c
int i = 0;
while (1) {
	if (i < 10) pc_label(1,1,"ELO"); else break;
    i++;
}
```

### SLO

This criteria check for each loop in the program if we skip it at least once. It uses sequences which break if we enter in the loop.
the following example:

```c
for(int i = 0; i < 10; i++){
	...
}
```
becomes:
```c
int i = 0;
int __SEQ_STATUS_1 = 1;
while (1) {
	if (i < 10) __SEQ_STATUS_1 = 0; else break;
	i++;
}
pc_label(__SEQ_STATUS_1 == 1,1,"SLO");
```

Dataflow
--------

in hyperlabels:
* `.` means `/\` (logical and)
* `+` means `\/` (logical or)


### DUC (Def-Use pairs coverage)

```
Objectives : Cover independently each def-use pair
```
the following example :
```c
int main(){
    int a = 0;
    if (a){
        a = 1;
    }
  return a;
}
```
becomes:
```c
int main(void) {
    int __SEQ_STATUS_a_1 = 0;
    int __SEQ_STATUS_a_2 = 0;
    int __SEQ_STATUS_a_3 = 0;
    int a = 0;
    __SEQ_STATUS_a_1 = 1;
    __SEQ_STATUS_a_2 = 1;
    pc_label(__SEQ_STATUS_a_1 == 1,1,"DUC");
    if (a) {
       __SEQ_STATUS_a_1 = 0;
       __SEQ_STATUS_a_2 = 0;
       a = 1;
       __SEQ_STATUS_a_3 = 1;
    }
    pc_label(__SEQ_STATUS_a_2 == 1,2,"DUC");
    pc_label(__SEQ_STATUS_a_3 == 1,3,"DUC");
    return a;
}
```
and hyperlabels:
```
<s1|; ;>,
<s2|; ;>,
<s3|; ;>,
```

### ADC (All-definitions coverage)

```
Objectives : cover at least one def-clear path between a definition and one of its uses
```
the following example :
```c
int main(){
    int a = 0;
    if (a){
        a = 1;
    }
    return a;
}
```
becomes:
```c
int main(void) {
    int __SEQ_STATUS_a_1 = 0;
    int __SEQ_STATUS_a_2 = 0;
    int __SEQ_STATUS_a_3 = 0;
    int a = 0;
    __SEQ_STATUS_a_1 = 1;
    __SEQ_STATUS_a_2 = 1;
    pc_label(__SEQ_STATUS_a_1 == 1,1,"DUC");
    if (a) {
       __SEQ_STATUS_a_1 = 0;
       __SEQ_STATUS_a_2 = 0;
       a = 1;
       __SEQ_STATUS_a_3 = 1;
    }
    pc_label(__SEQ_STATUS_a_2 == 1,2,"DUC");
    pc_label(__SEQ_STATUS_a_3 == 1,3,"DUC");
    return a;
}
```
and hyperlabels:
```
<s1+s2|; ;>,
<s3|; ;>,
```

### AUC (All-uses coverage)

```
Objectives : cover a def-clear path between a definition to each of its uses
```
the following example:
```c
int main(){
    int a = 0;
    if (a){
        a = 1;
    }
    return a;
}
```
becomes:
```c
int main(void) {
    int __SEQ_STATUS_a_1 = 0;
    int __SEQ_STATUS_a_2 = 0;
    int __SEQ_STATUS_a_3 = 0;
    int a = 0;
    __SEQ_STATUS_a_1 = 1;
    __SEQ_STATUS_a_2 = 1;
    pc_label(__SEQ_STATUS_a_1 == 1,1,"DUC");
    if (a) {
       __SEQ_STATUS_a_1 = 0;
       __SEQ_STATUS_a_2 = 0;
       a = 1;
       __SEQ_STATUS_a_3 = 1;
    }
    pc_label(__SEQ_STATUS_a_2 == 1,2,"DUC");
    pc_label(__SEQ_STATUS_a_3 == 1,3,"DUC");
    return a;
}
```
and hyperlabels:
```
<s1.s2|; ;>,
<s3|; ;>,
```

Others
------

### Stmt (Statement coverage)

A pc_label is placed before each goto/return, at the start of each block (except the main block in a switch), and after each label (Case, Default and custom label).

The following example:
```c
int a = 0;

if(a){
	return 0;
}
else{
	return 1;
}
```
becomes:
```c
int __retres;
pc_label(1,1,"STMT");
int a = 0;
if (a) {
	pc_label(1,2,"STMT");
	__retres = 0;
	pc_label(1,3,"STMT");
	goto return_label;
}
else {
	pc_label(1,4,"STMT");
	__retres = 1;
	pc_label(1,5,"STMT");
	goto return_label;
}
```


### WM (Weak Mutation)

    frama-c -lannot=WM file.c

This command creates an annoted file with labels corresponding to every
available mutators. One can select more precisely mutators by removing some of them, like so:

    frama-c -lannot=WM -lannot-mutators=-AOR,-COR file.c

Available mutators are ABS, AOR, COR and ROR.

The following example (with ABS and AOR):
```c
int main(int a, int b){
  return a + b;
}
```
will be instrumented as follows:
```c
int main(int a, int b) {
  int __retres;
  pc_label(a * b != a + b,1,"WM AOR");
  pc_label(a / b != a + b,2,"WM AOR");
  pc_label(a - b != a + b,3,"WM AOR");
  pc_label(a < 0,4,"WM ABS");
  pc_label(b < 0,5,"WM ABS");
  __retres = a + b;
  pc_label(__retres < 0,6,"WM ABS");
  return __retres;
}
```


### IOB (Input Output Bound Coverage)

Test boundaries of each function input and output (i.e. formals and returns).
Boundaries are Min and Max (depending on variable's type). Unsigned's min is zero.
With Frama-C's normalization, each function will contain only one return statement.

The following example:
```c
int main(int a){
  return a;
}
```
will be instrumented as follows:
```c
int main(int a) {
  int __retres;
  pc_label(a == (-2147483647-1),1,"IOB");
  pc_label(a == 2147483647,2,"IOB");
  __retres = a + a;
  pc_label(__retres == (-2147483647-1),3,"IOB");
  pc_label(__retres == 2147483647,4,"IOB");
  return __retres;
}
```


### CB (Condition Bound Coverage)

Create bound objectives for each variable inside condition. Also create an objective
to compare both side of each atomic expression for Lt/Gt/Le/Ge/Eq/Ne


The following example:
```c
int maintest(int a, int b, int c){
    if(a < b) return 1;
    if(c + 12 != 42) return 2;
	return 0;
}
```
will be instrumented as follows:
```c
int maintest(int a, int b, int c)
 {
   int __retres;
   pc_label(a == (-2147483647-1),1,"CB");
   pc_label(a == 2147483647,2,"CB");
   pc_label(b == (-2147483647-1),3,"CB");
   pc_label(b == 2147483647,4,"CB");
   pc_label(a == b,5,"CB");
   if (a < b) {
     __retres = 1;
     goto return_label;
   }
   pc_label(c == (-2147483647-1),6,"CB");
   pc_label(c == 2147483647,7,"CB");
   pc_label(c + 12 == 42,8,"CB");
   if (c <= 42) {
     __retres = 2;
     goto return_label;
   }
   __retres = 0;
   return_label: return __retres;
 }
```


### BC (Bound Coverage (IOB + CB)

This criteria simply regroup Input/Output Bound Coverage and Condition Bound Coverage


### IDP (Input Domain Partition)

DONE?

  - `-lannot-maxwidth width`
  - `-lannot-maxheight height`
  - `-lannot-allfuns`
  - `-lannot-globals`
