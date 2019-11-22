# ptb2cf
This code approximates Penn Treebank (PTB) graph-structured representations by trees.
For more detail about the method, see the following paper:

* Yoshihide Kato, Shigeki Matsubara: PTB Graph Parsing with Tree Approximation, ACL 2019 (short paper).

To use this code, install [SBCL](http://www.sbcl.org/) and [cl-ppcre](https://edicl.github.io/cl-ppcre/).

The following code is a sample of converting PTB graphs into PTB augmented trees.
```
(require :asdf)
(asdf:load-system :ptb2cf)
(use-package :ptb2cf)

(ptb->cf :ptb "/corpus/02-21.mrg" :cf "/data/02-21.aug" :dictionary "/data/dict")
```
This code reads PTB graphs from the file specified by :ptb, and writes PTB augmented trees to the file specified by :cf.
Empty elements are encoded based on the file specified by :dictionary.
If the file does not exist, ptb->cf automatically generates it.

To recover PTB graphs from PTB augmented trees, run the following code.
```
(cf->ptb :cf "/data/23.aug" :ptb "/data/23.mrg" :dictionary "/data/dict")
```
