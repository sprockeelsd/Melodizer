(in-package :om)

; MELODY-FINDER
; <input> is a list of lists with each list representing the midicent values of the chords on top of which the melody will be played
; <rhythm> the rhythm of the melody to be found in the form of a rhythmtree
; <key> is the key in which the melody is
; <mode> is the mode of the tonality (major, minor)
; This function creates the CSP by creating the space and the variables, posting the constraints and the branching, specifying
; the search options and creating the search engine. 
(defmethod melody-finder ( input rhythm &optional (key 60.0) (mode "major"))
    (let ((sp (gil::new-space)); create the space; 
        pitch intervals dfs tstop sopts)

        ;initialize the variables
        (setq pitch (gil::add-int-var-array sp (get-events-from-rtree rhythm) 60 84))
        ; set the intervals value to everything up to an octave, not including tritones, major seventh and minor seventh
        (setq intervals (gil::add-int-var-array sp (- (length pitch) 1) -24 24)); this can be as large as possible given the domain of pitch, to keep all the constraints in the constraint part.

        ; then, post the constraints
        (in-tonality sp pitch key mode)

        ;(precedence sp pitch 72 71)

        (all-different-notes sp pitch)

        (interval-between-adjacent-notes sp pitch intervals)
        
        ; branching
        ;(gil::g-branch sp pitch gil::INT_VAR_SIZE_MIN gil::INT_VAL_MIN)
        (gil::g-branch sp pitch gil::INT_VAR_RND gil::INT_VAL_RND)

        ;time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (gil::time-stop-init tstop 1000); initialize it (time is expressed in ms)

        ;search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        (gil::set-n-threads sopts 0); set the number of threads to be used during the search (default is 1, 0 means as many as available)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ; search engine
        (setq se (gil::search-engine sp (gil::opts sopts)))

        (print "CSP constructed")
        ; return
        (list se pitch tstop sopts)
    )
)

; SEARCH-NEXT-MELODY-FINDER
; <l> is a list containing in that order the search engine for the problem and the variables
; <rhythm> is the input rhythm as given by the user 
; this function finds the next solution of the CSP using the search engine given as an argument
(defmethod search-next-melody-finder (l rhythm)
    (let ((se (first l))
         (pitch* (second l))
         (tstop (third l))
         (sopts (fourth l))
         sol pitches)
        
        (gil::time-stop-reset tstop);reset the tstop timer before launching the search
        (setq sol (gil::search-next se)); search the next solution
        (if (null sol) (error "There are no more solution or the solver couldn't find one in time."))
        (setq pitches (to-midicent (gil::g-values sol pitch*))); store the values of the solution
        (print "pitches")
        (print pitches)

        ;return a voice object that is the solution we just found
        (make-instance 'voice
            :tree rhythm
            :chords pitches
            :tempo 60
        )
    )
)







#| ; gets all the chords from a voice object
(defmethod! getvoice (inputvoice)
    (let ((chords (chords inputvoice)))
        (dolist (c chords)
            (print (lmidic c))
        )
    )
) |#

;;; The following code is the same as above but for the previous representation using chordseq objects
#| (defmethod! melodizer ( chords note-starts note-durations &optional (key 60.0) (mode 0.0))
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
        (setq pitch (gil::add-int-var-array sp 10 60 72))

        ; then, post the constraints
        (in-tonality sp pitch 60 0)

        ;(all-different-notes sp pitch)

        ;(interval-between-adjacent-notes sp pitch)
        
        ; branching
        ; in order branching
        (gil::g-branch sp pitch 0 0)
        ;random branching
        ;(gil::g-branch-random sp pitch 1 2)

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
) |#
