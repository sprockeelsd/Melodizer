(in-package :mldz)

; DUMMY-PROBLEM
; This function creates a CSP by creating the space and the variables, posting the branching, specifying
; the search options and creating the search engine.
(defun dummy-problem ()
    (let ((sp (gil::new-space)); create the space; 
        vars se tstop sopts max id-list)

        ;initialize the variables
        (setq vars (gil::add-int-var-array sp 3 1 4))

        ; constraints
        (gil::g-count-array sp vars (list 1 1 1 1) gil::IRT_EQ 2)
        ; branching
        (gil::g-branch sp vars gil::INT_VAR_SIZE_MIN gil::INT_VAL_MIN)

        ;time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (gil::time-stop-init tstop 500000); initialize it (time is expressed in ms)

        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ; search engine
        (setq se (gil::search-engine sp (gil::opts sopts) gil::BAB)); branch and bound search-engine, remove t for dfs
        (print se)

        (print "CSP constructed")
        ; return
        (list se vars tstop sopts)
    )
)

; SEARCH-NEXT-DUMMY-PROBLEM
; <l> is a list containing in that order the search engine for the problem, the variables
; this function finds the next solution of the CSP using the search engine given as an argument
(defun search-next-dummy-problem (l)
    (let ((se (first l))
         (pitch* (second l))
         (tstop (third l))
         (sopts (fourth l))
         sol pitches)

        (gil::time-stop-reset tstop);reset the tstop timer before launching the search
        (setq sol (gil::search-next se)); search the next solution, sol is the space of the solution
        (if (null sol) 
            (error "No more solutions")
        )
        ; print the solution from GiL
        (setq pitches (gil::g-values sol pitch*)); store the values of the solution
        (print "pitches")
        (print pitches)
    )
)