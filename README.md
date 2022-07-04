# Melodizer

Melodizer is an OpenMusic objet allowing composers to increase their expression and composition abilities.

Melodizer v1.0 takes a sequence of chords and either a melody or a rhythm as input, and uses constraint programming to generate melodies.

Melodizer v2.0 allows composers to create blocks and combine them together to create polyphonic pieces.

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
## How to improve Melodizer
There are many ways that Melodizer could be improved. A few ideas are listed below.
- Diversify the input. For now, only sequences of major/minor/diminished chords are supported. Other types of chords could be added, and the input could be changed so that it does not only accept chords.
- Increase the number of CSPs (and therefore modes) so that an even wider range of problems can be solved using Melodizer.
- The ability to select a group of notes and post constraints only on the selected notes would be a good feature as well.
- Adding support in Melodizer for Rhythm-Box (https://github.com/blapiere/Rhythm-Box) would be useful as well. Currently Rhythm-Box can be used with Melodizer but only inside the patch by connecting the objects through their inlets/outlets.
- Melodizer currently offers a simple way of combining musical ideas and solutions created by the CSP, but it could also be improved to make it easier for composers to combine their ideas. One possible approach would be based on PowerPoint. Each instance of Melodizer would be like a PowerPoint slide, with all its memorized musical elements. It could be possible to copy motifs, phrases, periods from one instance to the other, and combine them into a musical network. The way of exporting melodies from Melodizer to be used with other OpenMusic components could also be improved.

