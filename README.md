# Melodizer

Melodizer is an OpenMusic objet allowing composers to increase their expression and composition abilities. There are currently 2 versions of Melodizer.

Melodizer v1.0 takes a sequence of chords and either a melody or a rhythm as input, and uses constraint programming to generate melodies or variations of melodies.

Melodizer v2.0 allows composers to create blocks and combine them together to create polyphonic pieces without providing any input.

Melodizer is an OpenSource project, any contribution is welcomed. Please email me at damien.sprockeels@uclouvain.be if you wish to contribute.

## How to use

In order to use Melodizer, you need to install GiL (https://github.com/sprockeelsd/GiLv2.0) and OpenMusic (https://openmusic-project.github.io/openmusic/). Once this step is done, you are ready to load the library in OpenMusic.

### Melodizer v1.0

The **Melodizer-how-to-use** pdf gives a tutorial on how to use Melodizer. The **Melodizer-examples** pdf gives a few concrete examples of the use of Melodizer for a variety of musical scenarios, and gives videos to listen to the result.

### Structure explanation
In Melodizer/sources, there are 4 files. They are detailed below. More information can be found in the **Melodizer-implementation** pdf.

#### melodizer-csp.lisp
This file contains the Constraint Satisfaction Problem, including the creation of the variables, the search space, the posting of the constraints, the branching, and the search.

#### melodizer-csts.lisp
This file contains all the constraints available for the CSP.

#### melodizer-utils.lisp
This file contains all the auxiliary functions used in the project.

#### melodizer.lisp
This file contains the code for the OM object.

### Melodizer v2.0
The **Melodizer_v2.0** contains a tutorial on how to use it in chapter 6 and a set of examples in chapter 7.
