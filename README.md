# Melodizer

Melodizer is an OpenMusic objet allowing you to get innovating melody ideas from a sequence of chords that you give. 
The latest version can be found here : 
https://github.com/clemsky/TFE-Composition- Musicale

## How to use
see file ***Melodizer-how-to-use.pdf***

## Structure explanation
In Melodizer/sources, there are 4 files.

### melodizer-csp.lisp
This file contains the Constraint Satisfaction Problem, including the creation of the variables, the search space, the posting of the constraints, the branching, and the search.

### melodizer-csts.lisp
This file contains all the constraints available for the CSP.

### melodizer-utils.lisp
This file contains all the auxiliary functions used in the project.

### melody-finder.lisp
This file contains the code for the OM object.

## How to improve Melodizer
Explain later
