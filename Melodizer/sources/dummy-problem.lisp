(in-package :mldz)

; DUMMY-PROBLEM
; This function creates a CSP by creating the space and the variables, posting the branching, specifying
; the search options and creating the search engine.
(defun dummy-problem ()
    (let ((sp (gil::new-space)); create the space; 
        pitch dfs tstop sopts)

        ;initialize the variables
        (setq pitch (gil::add-int-var-array sp 2 1 4))

        ; then, post the constraints
        
        ; branching
        (gil::g-branch sp pitch gil::INT_VAR_SIZE_MIN gil::INT_VAL_MIN)

        ;time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (gil::time-stop-init tstop 1000); initialize it (time is expressed in ms)

        ;search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        (gil::set-n-threads sopts 1); set the number of threads to be used during the search (default is 1, 0 means as many as available)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ; search engine
        (setq se (gil::search-engine sp (gil::opts sopts)))

        (print "CSP constructed")
        ; return
        (list se pitch tstop sopts)
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
        (setq sol (gil::search-next se)); search the next solution
        (if (null sol) 
            (stopped-or-ended-dummy (gil::stopped se))
            ;(print (gil::stopped se))
            ;; (if (gil::stopped se)
            ;;     (error "Search has been stopped")
            ;;     (error "No more solutions")
            ;; )
            ;(error "There are no more solution or the solver couldn't find one in time.")
        )
        (setq pitches (gil::g-values sol pitch*)); store the values of the solution
        (print "pitches")
        (print pitches)
    )
)

(defun stopped-or-ended-dummy (stopped)
    (if (= stopped 1)
        (error "Search has been stopped")
        (error "No more solutions")
    )
)