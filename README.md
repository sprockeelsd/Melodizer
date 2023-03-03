# Melodizer

This repository is home to three computer-aided composition projects in the form of Common Lisp libraries.

The first one, Melodizer, is an OpenMusic object allowing you to generate innovating melodic ideas from a sequence of chords that you give.

The second one, Melodizer2.0, is based on Melodizer but can generate polyphonic music. It has a specific data structure, called blocks, that can be combined both vertically and horizontally and constraints can be added on and between blocks.

The third one is still a work in progress. It is a formalization of counterpoint in the style of Johann Joseph Fux. Currently all five species of two-voice counterpoint are formalized and the library functions allow to generate counterpoint from user-specified cantus firmi. It does not currently have a user-friendly interface, but is already useable. Future improvements will be pushed regularly when available.

## How to use
All three libraries require GiL, a library linking Gecode to Lisp that can be found [here](https://github.com/sprockeelsd/GiL). All instructions on how to install it can be found on its github page.

Melodizer and Melodizer2.0 have pdf files explaining how they work and how to use them in their respective ***UserGuide*** folder. Since the Fux formalization is not finished yet, the instructions on how to use it are detailed below.

### Melodizer

#### Structure explanation
In Melodizer/sources, there are 4 files.

***melodizer-csp.lisp***
This file contains the Constraint Satisfaction Problem, including the creation of the variables, the search space, the posting of the constraints, the branching, and the search.

***melodizer-csts.lisp***
This file contains all the constraints available for the CSP.

***melodizer-utils.lisp***
This file contains all the auxiliary functions used in the project.

***melody-finder.lisp***
This file contains the code for the OM object.

### Melodizer2.0

### Fux formalization (Work in progress)
Since the functional part of this library has only just been completed, no interface is already available as mentioned above.

#### Structure explanation
In FuxCP/sources, there is 1 file:
***fux-counterpoint.lisp***
This file contains all the logic and constraints. If you are a bit of a Lisp connoisseur, it is possible to change some values in the code but be aware that it is a bit messy at the moment. For example, the VOICE_TYPE value at the beginning is 1 by default but it is possible to change this value to change the range of the counterpoint produced relative to the first note of the cantus firmus.

#### Voice type shifting
Shifting the voice type by 1 shifts the range by half an octave. Please note that the third kind of counterpoint does not currently work with a voice type lower than 1.

#### Functions
To use this library, you must load the ***fux-cp*** and ***search-next-fux-cp*** functions in the patch. The first one sets the harmonic and melodic constraints with respect to the cantus firmus while the second one gives a voice object with the solution. Press ">" and "<" to make (dis)appear optional parameters to a function in OM. 

***fux-cp***:
1st input - voice object: the cantus firmus with only full notes in 4/4 with a minimum of 3 measures.
2nd input - integer (default=1): the number of the species {1, 2, 3, 4, 5}.

***search-next-fux-cp***
1st input - the fux-cp output: directly connect the output of the previous function. 
output - voice object: directly connect the first input of a voice object.
