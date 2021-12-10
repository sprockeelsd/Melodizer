(in-package :mldz)

; MELODY-FINDER
; <input> is a voice object with the chords on top of which the melody will be played
; <rhythm> the rhythm of the melody to be found in the form of a voice object
; <optional-constraints> is a list of optional constraint names that have to be applied to the problem
; <global interval> is the global interval that the melody should cover if the mostly increasing/decreasing constraint is selected
; <key> is the key in which the melody is
; <mode> is the mode of the tonality (major, minor)
; This function creates the CSP by creating the space and the variables, posting the constraints and the branching, specifying
; the search options and creating the search engine. 
(defmethod melody-finder (input rhythm optional-constraints &optional (global-interval nil) (key 60.0) (mode "major"))
    (let ((sp (gil::new-space)); create the space; 
        pitch intervals dfs tstop sopts)

        ;initialize the variables
        (setq pitch (gil::add-int-var-array sp (om::n-pulses rhythm) 60 84))
        ;(setq pitch (gil::add-int-var-array sp 20000 1 10)) ; to test if we can stop during the search
        ; set the intervals value to everything up to an octave, not including tritones, major seventh and minor seventh
        (setq intervals (gil::add-int-var-array sp (- (length pitch) 1) -24 24)); this can be as large as possible given the domain of pitch, to keep all the constraints in the constraint part.

        ; connect pitches to intervals
        (loop :for j :from 0 :below (length intervals) :do ;for each interval
            (let (temp)
                (setq temp (gil::add-int-var-array sp 3 -12 108)); temporary variables to make it easier to apply the linear constraint

                (gil::g-rel sp (first temp) gil::IRT_EQ (nth j pitch)); temp[0] = pitch[j]
                (gil::g-rel sp (second temp) gil::IRT_EQ (nth (+ j 1) pitch)); temp[1] = pitch[j+1]
                (gil::g-rel sp (third temp) gil::IRT_EQ (nth j intervals)); temp[2] = intervals[j]

                (gil::g-linear sp '(-1 1 -1) temp gil::IRT_EQ 0); -pitch[j] + pitch[j+1] - intervals[j] = 0
            )
        )

        ; then, post the constraints

        ; mandatory constraints

        (interval-between-adjacent-notes sp pitch intervals)

        (in-tonality sp pitch key mode)

        ;(precedence sp pitch 72 71)

        (note-on-chord sp pitch rhythm input)

        (harmonic-interval-chord sp pitch rhythm input)

        ; optional constraints
        (post-optional-constraints optional-constraints sp pitch intervals global-interval)
        
        ; branching
        (gil::g-branch sp pitch gil::INT_VAR_DEGREE_MAX gil::INT_VAL_RND)

        (gil::g-branch sp intervals gil::INT_VAR_SIZE_MIN gil::INT_VAL_SPLIT_MAX)

        ;time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (gil::time-stop-init tstop 500); initialize it (time is expressed in ms)

        ;search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        (gil::set-n-threads sopts 1); set the number of threads to be used during the search (default is 1, 0 means as many as available)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ; search engine
        (setq se (gil::search-engine sp (gil::opts sopts) gil::DFS))

        (print "CSP constructed")
        ; return
        (list se pitch tstop sopts intervals)
    )
)

;posts the optional constraints specified in the list
; TODO CHANGE LATER SO THE FUNCTION CAN BE CALLED FROM THE STRING IN THE LIST AND NOT WITH A SERIES OF IF STATEMENTS
(defun post-optional-constraints (optional-constraints sp notes intervals global-interval)
    (if (find "all-different-notes" optional-constraints :test #'equal)
        (all-different-notes sp notes)
    )
    (if (find "strictly-increasing-pitch" optional-constraints :test #'equal)
        (strictly-increasing-pitch sp notes)
    )
    (if (find "strictly-decreasing-pitch" optional-constraints :test #'equal)
        (strictly-decreasing-pitch sp notes)
    )
    (if (find "increasing-pitch" optional-constraints :test #'equal)
        (increasing-pitch sp notes)
    )
    (if (find "decreasing-pitch" optional-constraints :test #'equal)
        (decreasing-pitch sp notes)
    )
    (if (find "mostly-increasing-pitch" optional-constraints :test #'equal)
        (mostly-increasing-pitch sp notes intervals global-interval)
    )
    (if (find "mostly-decreasing-pitch" optional-constraints :test #'equal)
        (mostly-decreasing-pitch sp notes intervals global-interval)
    )
)

; SEARCH-NEXT-MELODY-FINDER
; <l> is a list containing in that order the search engine for the problem and the variables
; <rhythm> is the input rhythm as given by the user 
; <melodizer-object> is a melodizer object
; this function finds the next solution of the CSP using the search engine given as an argument
(defmethod search-next-melody-finder (l rhythm melodizer-object)
    (let ((se (first l))
         (pitch* (second l))
         (tstop (third l))
         (sopts (fourth l))
         (intervals (fifth l))
         (check t); for the while loop
         sol pitches)

        (om::while check :do
            (gil::time-stop-reset tstop);reset the tstop timer before launching the search
            (setq sol (gil::search-next se)); search the next solution
            (if (null sol) 
                (stopped-or-ended (gil::stopped se) (stop-search melodizer-object) tstop); check if there are solutions left and if the user wishes to continue searching
                (setf check nil); we have found a solution so break the loop
            )
        )

        (setq pitches (to-midicent (gil::g-values sol pitch*))); store the values of the solution
        (print "pitches")
        (print pitches)
        (print (gil::g-values sol intervals))

        ;return a voice object that is the solution we just found
        (make-instance 'voice
            :tree rhythm
            :chords pitches
            :tempo 60
        )
    )
)

; determines if the search has been stopped by the solver because there are no more solutions or if the user has stopped the search
(defun stopped-or-ended (stopped-se stop-user tstop)
    (if (= stopped-se 0); if the search has not been stopped by the TimeStop object, there is no more solutions
        (error "There are no more solutions.")
    )
    ;otherwise, check if the user wants to keep searching or not
    (if stop-user
        (error "The search has been stopped. Press next to continue the search.")
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
