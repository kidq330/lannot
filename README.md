GenLabels
=========

GenLabels est un greffon pour la version spéciale «label» de Frama-C.

Installation
------------

Après avoir installer Frama-C, compiler et installer Frama-C de la façon
suivante:

    make
    sudo make install

Utilisation
-----------

### Labels à conditions simples

Pour générer des labels à conditions simples, il faut utiliser la commande:

    frama-c -genlabels file.c

Cette commande crée un nouveau fichier `file_labels.c` qui contient le code du fichier `file.c` auquel on a ajouté des labels

Par exemple :

      if (a <= b && c >= b) {

devient :

    pc_label(a <= b);
    pc_label(! (a <= b));
    pc_label(c >= b);
    pc_label(! (c >= b));
    if (a <= b && c >= b) {

### Labels à conditions multiples

Pour générer des labels à conditions multiple, il faut utiliser la commande:

    frama-c -genlabels -genlabels-multi file.c

Cette commande crée un nouveau fichier `file_multilabels.c` qui contient le code du fichier `file.c` auquel on a ajouté des labels

Exemple :

    if (a <= b && c >= b) {

devient :

    pc_label(! (a <= b) && ! (c >= b));
    pc_label(! (a <= b) && c >= b);
    pc_label(a <= b && ! (c >= b));
    pc_label(a <= b && c >= b);
    if (a <= b && c >= b) {

