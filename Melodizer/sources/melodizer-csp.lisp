(in-package :om)
;taken from rhythm box
;https://github.com/blapiere/Rhythm-Box
(defclass constraint ()
    ((cst-function :initform nil :initarg :cstf :accessor cstf) 
    ;a constraint function that always have a space as 1st arg and a list as 2nd arg
    ;should this function have return values, they HAVE TO under the form of a list 
    ;of var-ref objects.

     (args :initform nil :initarg :args :accessor args)) 
    ;the list args to be the 2nd argument of the cst-function
)

;taken from rhythm box
;https://github.com/blapiere/Rhythm-Box
(defun post-constraint (sp cst N pitch)
    "Call the constraint function with the melodizer data and the constraint arglist."
    (funcall (cstf cst) sp pitch (args cst))
)

; <constraints> is a list of <constraint> object
; <chords> is a chord sequence
; <note-starts> is a list of integers representing the start of each note
; <note-durations> is a list of integers representing the duration of each note
; <key> is the key in which the melody is
; <mode> is the mode the melody is in (major, minor)
; add constraints to signature later
(defmethod! melodizer ( chords note-starts note-durations &optional (key 60.0) (mode 0.0))
    :initvals (list (make-instance 'chord-seq) nil nil 60.0 0.0)
    :indoc '("a list of constraints" "the chords on top of which notes have to be placed" 
            "the starting position of the notes that will be produced" 
            "the durations of the notes that will be produced"
            "the key" 
            "the mode"
            )
    :icon 921
    :doc "TODO : add documentation once the code does something interesting"
    (let ((sp (gil::new-space))
        pitch dfs)

        ; first, create the variables
        (setq pitch (gil::add-int-var-array sp 10 21 108))

        ; then, post the constraints
        ;(all-different-notes sp pitch)
        (in-tonality sp pitch 60 0)
        
        ; branching
        (gil::g-branch sp pitch 0 0)

        ; search engine
        (setq se (gil::search-engine sp nil))

        ; return
        (list se pitch note-starts note-durations)
    )
)

(defmethod! search-next (l)
    :initvals (list nil) 
    :indoc '("a musical-space")
    :icon 330
    :doc "
Get the next solution for the csp described in the input musical-space.
"
    (let ((se (first l))
         (pitch* (second l))
         (starts (third l))
         (durations (fourth l))
         sol pitches)
        
        ;Get the values of the solution
        (setq sol (gil::search-next se))
        (if (null sol) (error "No solution or no more solution."))
        ;(print gil::g-values pitch*)
        (setq pitches (to-midicent (gil::g-values sol pitch*)))
        (print pitches)

        ;return a chord-seq object
        (make-instance 'chord-seq
            :lmidic pitches
            :lonset starts
            :ldur durations
        )

    )
)
