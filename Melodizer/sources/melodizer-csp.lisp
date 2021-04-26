(in-package :om)
;taken from rhythm box
(defclass constraint ()
    ((cst-function :initform nil :initarg :cstf :accessor cstf) 
    ;a constraint function that always have a space as 1st arg and a list as 2nd arg
    ;should this function have return values, they HAVE TO under the form of a list 
    ;of var-ref objects.

     (args :initform nil :initarg :args :accessor args)) 
    ;the list args to be the 2nd argument of the cst-function
)

(defun post-constraint (sp cst pos drt drt* N Dn s l duration nmin nmax input)
    "Call the constraint function with the rhythm-box data and the constraint arglist."
    (funcall (cstf cst) sp pos drt drt* N Dn s l duration nmin nmax input (args cst))
)

; <csts> is a list of <constraint> object
; <input> is either a voice object or a rhythm tree
; <s> is a fraction - the shortest authorized event duration
; <l> is a fraction - the longest authorized event duration
; <hd> is a float between 0 and 1 that indicates the maximum rhythmic density
;constraints note-starts note-durations chords
(defmethod! melodizer (&optional (key 60.0) (mode 0.0))
    ;:initvals (60.0 0.0) 
    :indoc '("a list of constraints" "the duration of the notes that will be produced" 
            "the starting positions of the notes that will be produced"
             "the chords on top of which the notes have to be placed" "the key" "the mode")
    :icon 921
    :doc "
TODO : add documentation once the code has been modified
"
    (let ((sp (new-space))
        (N (get-N note-starts))
        pitch dfs)

        ;first, create the variables
        (setq pitch (gil::add-int-var-array sp N 21 108))

        ;then, post the constraints
        (gil::g-distinct sp pitch)

        ;branching
        (gil::g-branch sp pitch 0 0)

        ;search engine
        (setq se (gil::search-engine sp nil))

        ;return
        (list pitch)
    )
)

(defmethod! search-next (l)
    :initvals (list nil) 
    :indoc '("a rhythm-space")
    :icon 330
    :doc "
Get the next solution for the rhythm csp described in the input rhythm-space.
"
    (let ((se (first l))
         (drt* (second l))
         (input (third l))
         (s* (nth 3 l))
         sol drt rtree)
        
        ;Get the values of the solution and build the tree
        (setq sol (gil::search-next se))
        (if (null sol) (error "No solution or no more solution."))
        (setq drt (gil::g-values sol drt*))
        (if (typep input 'voice)
            (setq rtree (build-rtree (tree input) drt s*))
            (setq rtree (build-rtree input drt s*)))

        ;return a voice object based on the input
        (if (typep input 'voice)
            (make-instance 'voice
                :tree rtree
                :chords (chords input)
                :tempo (tempo input)
                :legato (legato input)
                :ties (ties input))
            (make-instance 'voice
                :tree rtree
                :chords '((7200)))
    )
))