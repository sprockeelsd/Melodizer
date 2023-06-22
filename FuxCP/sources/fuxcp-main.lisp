(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains the functions that:
;   - dispatch to the right species functions
;   - set the global variables of the CSP
;   - manage the search for solutions

(print "Loading fux-cp...")

; get the value at key @k in the hash table @h as a list
(defun geth-dom (h k)
    (list (gethash k h))
)

; get the value at key @k in the parameters table as a list
(defun getparam-val (k)
    (geth-dom *params* k)
)

; get the value at key @k in the parameters table as a domain
(defun getparam-dom (k)
    (list 0 (getparam k))
)

; get the value at key @k in the parameters table
(defun getparam (k)
    (gethash k *params*)
)

; get if borrow-mode param is allowed
(defun is-borrow-allowed ()
    (not (equal (getparam 'borrow-mode) "None"))
)

; re/define all the variables the CSP needs
(defun set-space-variables ()
    ; THE CSP SPACE 
    (defparameter *sp* (gil::new-space))

    ;; CONSTANTS
    ; Number of costs added
    (defparameter *n-cost-added 0)
    ; Motion types
    (defparameter DIRECT 2)
    (defparameter OBLIQUE 1)
    (defparameter CONTRARY 0)

    ;; COSTS
    ;; Melodic costs
    (defparameter *m-step-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-step-cost)))
    (defparameter *m-third-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-third-cost)))
    (defparameter *m-fourth-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-fourth-cost)))
    (defparameter *m-tritone-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-tritone-cost)))
    (defparameter *m-fifth-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-fifth-cost)))
    (defparameter *m-sixth-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-sixth-cost)))
    (defparameter *m-seventh-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-seventh-cost)))
    (defparameter *m-octave-cost* (gil::add-int-var-dom *sp* (getparam-val 'm-octave-cost)))
    ;; General costs
    (defparameter *borrow-cost* (gil::add-int-var-dom *sp* (getparam-val 'borrow-cost)))
    (defparameter *h-fifth-cost* (gil::add-int-var-dom *sp* (getparam-val 'h-fifth-cost)))
    (defparameter *h-octave-cost* (gil::add-int-var-dom *sp* (getparam-val 'h-octave-cost)))
    (defparameter *con-motion-cost* (gil::add-int-var-dom *sp* (getparam-val 'con-motion-cost)))
    (defparameter *obl-motion-cost* (gil::add-int-var-dom *sp* (getparam-val 'obl-motion-cost)))
    (defparameter *dir-motion-cost* (gil::add-int-var-dom *sp* (getparam-val 'dir-motion-cost)))
    ;; Species specific costs
    (defparameter *penult-sixth-cost* (gil::add-int-var-dom *sp* (getparam-val 'penult-sixth-cost)))
    (defparameter *non-cambiata-cost* (gil::add-int-var-dom *sp* (getparam-val 'non-cambiata-cost)))
    (defparameter *two-beats-apart-cost* (gil::add-int-var-dom *sp* (getparam-val 'two-beats-apart-cost)))
    (defparameter *two-bars-apart-cost* (gil::add-int-var-dom *sp* (getparam-val 'two-bars-apart-cost)))
    (defparameter *no-syncopation-cost* (gil::add-int-var-dom *sp* (getparam-val 'no-syncopation-cost)))

    ;; Params domains
    (defparameter *motions-domain*
        (remove-duplicates (mapcar (lambda (x) (getparam x))
            (list 'con-motion-cost 'obl-motion-cost 'dir-motion-cost)
        ))
    )

    ; Integer constants (to represent costs or intervals)
    ; 0 in IntVar
    (defparameter ZERO (gil::add-int-var-dom *sp* (list 0)))
    ; 1 in IntVar
    (defparameter ONE (gil::add-int-var-dom *sp* (list 1)))
    ; 3 in IntVar (minor third)
    (defparameter THREE (gil::add-int-var-dom *sp* (list 3)))
    ; 9 in IntVar (major sixth)
    (defparameter NINE (gil::add-int-var-dom *sp* (list 9)))

    ; Boolean constants
    ; 0 in BoolVar
    (defparameter FALSE (gil::add-bool-var *sp* 0 0))
    ; 1 in BoolVar
    (defparameter TRUE (gil::add-bool-var *sp* 1 1))

    ; Intervals constants
    ; perfect consonances intervals
    (defparameter P_CONS (list 0 7))
    ; imperfect consonances intervals
    (defparameter IMP_CONS (list 3 4 8 9))
    ; all consonances intervals
    (defparameter ALL_CONS (union P_CONS IMP_CONS))
    ; dissonances intervals
    (defparameter DIS (list 1 2 5 6 10 11))
    ; penultimate intervals, i.e. minor third and major sixth
    (defparameter PENULT_CONS (list 3 9))
    ; penultimate thesis intervals, i.e. perfect fifth and sixth
    (defparameter PENULT_THESIS (list 7 8 9))
    ; penultimate 1st quarter note intervals, i.e. minor third, major sixth and octave/unisson
    (defparameter PENULT_1Q (list 0 3 8))
    ; penultimate syncope intervals, i.e. seconds and sevenths
    (defparameter PENULT_SYNCOPE (list 1 2 10 11))

    ; P_CONS in IntVar
    (defparameter P_CONS_VAR (gil::add-int-var-const-array *sp* P_CONS))
    ; IMP_CONS in IntVar
    (defparameter IMP_CONS_VAR (gil::add-int-var-const-array *sp* IMP_CONS))
    ; ALL_CONS in IntVar
    (defparameter ALL_CONS_VAR (gil::add-int-var-const-array *sp* ALL_CONS))
    ; PENULT_CONS in IntVar
    (defparameter PENULT_CONS_VAR (gil::add-int-var-const-array *sp* PENULT_CONS))
    ; PENULT_THESIS in IntVar
    (defparameter PENULT_THESIS_VAR (gil::add-int-var-const-array *sp* PENULT_THESIS))
    ; PENULT_1Q in IntVar
    (defparameter PENULT_1Q_VAR (gil::add-int-var-const-array *sp* PENULT_1Q))
    ; PENULT_SYNCOPE in IntVar
    (defparameter PENULT_SYNCOPE_VAR (gil::add-int-var-const-array *sp* PENULT_SYNCOPE))

    ; *cf-brut-intervals is the list of brut melodic intervals in the cantus firmus
    (setq *cf-brut-m-intervals (gil::add-int-var-array *sp* *cf-last-index -127 127))

    ;; FIRST SPECIES COUNTERPOINT GLOBAL VARIABLES
    (defparameter *cp (list nil nil nil nil))
    (defparameter *h-intervals (list nil nil nil nil))
    (defparameter *m-intervals-brut (list nil nil nil nil))
    (defparameter *m-intervals (list nil nil nil nil))
    (defvar *m2-intervals-brut)
    (defvar *m2-intervals)
    (defvar *cf-brut-m-intervals)
    (defvar *is-p-cons-arr)
    (defparameter *motions (list nil nil nil nil))
    (defparameter *motions-cost (list nil nil nil nil))
    (defvar *is-cf-bass)
    (defparameter *is-cf-bass-arr (list nil nil nil nil))
    (defvar *is-cp-off-key-arr)
    (defvar *N-COST-FACTORS)
    (defvar *cost-factors)
    (defvar *total-cost)
    (defvar *p-cons-cost)
    (defvar *fifth-cost)
    (defvar *octave-cost)
    (defvar *m-degrees-cost)
    (defvar *m-degrees-type)
    (defvar *off-key-cost)

    ;; SECOND SPECIES COUNTERPOINT GLOBAL VARIABLES
    (defvar *h-intervals-abs)
    (defvar *h-intervals-brut)
    (defparameter *m-succ-intervals (list nil nil nil))
    (defparameter *m-succ-intervals-brut (list nil nil nil))
    (defvar *m2-len)
    (defvar *total-m-len)
    (defvar *m-all-intervals)
    (defvar *m-all-intervals-brut)
    (defvar *real-motions)
    (defvar *real-motions-cost)
    (defvar *is-ta-dim-arr)
    (defvar *is-nbour-arr)
    (defvar *penult-thesis-cost)
    (defvar *total-cp)

    ;; THIRD SPECIES COUNTERPOINT GLOBAL VARIABLES
    (defvar *is-5qn-linked-arr)
    (defvar *total-cp-len)
    (defparameter *is-cons-arr (list nil nil nil nil))
    (defparameter *cons-cost (list nil nil nil nil))
    (defvar *is-not-cambiata-arr)
    (defvar *not-cambiata-cost)
    (defvar *m2-eq-zero-cost)

    ;; FOURTH SPECIES COUNTERPOINT GLOBAL VARIABLES
    (defvar *is-no-syncope-arr)
    (defvar *no-syncope-cost)

    ;; FIFTH SPECIES COUNTERPOINT GLOBAL VARIABLES
    (defvar *species-arr) ; 0: no constraint, 1: first species, 2: second species, 3: third species, 4: fourth species
    (defvar *sp-arr) ; represents *species-arr by position in the measure
    (defparameter *is-nth-species-arr (list nil nil nil nil nil)) ; if *species-arr is n, then *is-nth-species-arr is true
    (defparameter *is-3rd-species-arr (list nil nil nil nil)) ; if *species-arr is 3, then *is-3rd-species-arr is true
    (defparameter *is-4th-species-arr (list nil nil nil nil)) ; if *species-arr is 4, then *is-4th-species-arr is true
    (defvar *is-2nd-or-3rd-species-arr) ; if *species-arr is 2 or 3, then *is-2nd-or-3rd-species-arr is true
    (defvar *m-ta-intervals) ; represents the m-intervals between the thesis note and the arsis note of the same measure
    (defvar *m-ta-intervals-brut) ; same but without the absolute reduction
    (defvar *is-mostly-3rd-arr) ; true if second, third and fourth notes are from the 3rd species
    (defvar *is-constrained-arr) ; represents !(*is-0th-species-arr) i.e. there are species constraints
    (defparameter *is-cst-arr (list nil nil nil nil)) ; represents *is-constrained-arr for all beats of the measure

    ; array representing the brut melodic intervals of the cantus firmus
    (create-cf-brut-m-intervals *cf *cf-brut-m-intervals)
)



;; DISPATCHER FUNCTION
(defun fux-cp (species)
    "Dispatches the counterpoint generation to the appropriate function according to the species."
    ; re/set global variables
    (set-space-variables)
    
    (print (list "Choosing species: " species))
    (case species ; [1, 2, 3, 4, 5]
        (1 (progn
            (setq *N-COST-FACTORS 5)
            (fux-cp-1st)
        ))
        (2 (progn
            (setq *N-COST-FACTORS 6)
            (fux-cp-2nd)
        ))
        (3 (progn
            (setq *N-COST-FACTORS 7)
            (fux-cp-3rd)
        ))
        (4 (progn
            (setq *N-COST-FACTORS 6)
            (fux-cp-4th)
        ))
        (5 (progn
            (setq *N-COST-FACTORS 8)
            (fux-cp-5th)
        ))
        (otherwise (error "Species ~A not implemented" species))
    )
)

(defun fux-search-engine (the-cp &optional (species 1))
    (let (se tstop sopts)
        ; TOTAL COST
        (gil::g-sum *sp* *total-cost *cost-factors) ; sum of all the cost factors
        (gil::g-cost *sp* *total-cost) ; set the cost function

        ;; SPECIFY SOLUTION VARIABLES
        (print "Specifying solution variables...")
        (gil::g-specify-sol-variables *sp* the-cp)
        (gil::g-specify-percent-diff *sp* 0)
        
        ;; BRANCHING
        (print "Branching...")
        (setq var-branch-type gil::INT_VAR_DEGREE_SIZE_MAX)
        (setq val-branch-type gil::INT_VAL_RANGE_MIN)

        ; 5th species specific
        (if (eq species 5) ; otherwise there is no species array
            (gil::g-branch *sp* *species-arr var-branch-type gil::INT_VAL_RND)
        )

        ; 3rd and 5th species specific
        (if (member species (list 3 5))(progn
            (gil::g-branch *sp* *m-degrees-cost var-branch-type val-branch-type)
            (gil::g-branch *sp* *off-key-cost var-branch-type val-branch-type)
        )
        (progn ; else
            (if (eq species 2)(progn
                ; (gil::g-branch *sp* *real-motions-cost var-branch-type val-branch-type)
                ; (gil::g-branch *sp* *m-degrees-cost var-branch-type gil::INT_VAL_SPLIT_MIN)
                ; (gil::g-branch *sp* *off-key-cost var-branch-type val-branch-type)
            ))
        )
        )

        ; 5th species specific
        (if (and (eq species 5) (>= VOICE_TYPE 0)) ; otherwise there is no species array
        (progn
            (gil::g-branch *sp* *no-syncope-cost var-branch-type val-branch-type)
            (gil::g-branch *sp* *not-cambiata-cost var-branch-type val-branch-type)
        )
        )

        ; branching *total-cost
        (gil::g-branch *sp* *total-cost var-branch-type val-branch-type)
        (if (eq species 2)
            (gil::g-branch *sp* *cost-factors var-branch-type val-branch-type)
        )
    
        ;; Solution variables branching
        (gil::g-branch *sp* the-cp var-branch-type val-branch-type)

        ; time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (setq timeout 5)
        (gil::time-stop-init tstop (* timeout 1000)); initialize it (time is expressed in ms)

        ; search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        ; (gil::set-n-threads sopts 1)
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ;; SEARCH ENGINE
        (print "Search engine...")
        (setq se (gil::search-engine *sp* (gil::opts sopts) gil::DFS));
        (print se)

        (print "CSP constructed")
        (list se the-cp tstop sopts)
    )
)



; SEARCH-NEXT-SOLUTION
; <l> is a list containing in that order the search engine for the problem, the variables
; this function finds the next solution of the CSP using the search engine given as an argument
(defun search-next-fux-cp (l)
    (print "Searching next solution...")
    (let (
        (se (first l))
        (the-cp (second l))
        (tstop (third l))
        (sopts (fourth l))
        (species (fifth l))
        (check t)
        sol sol-pitches sol-species
        )

        (time (om::while check :do
            ; reset the tstop timer before launching the search
            (gil::time-stop-reset tstop)
            ; try to find a solution
            (time (setq sol (try-find-solution se)))
            (if (null sol)
                ; then check if there are solutions left and if the user wishes to continue searching
                (stopped-or-ended (gil::stopped se) (getparam 'is-stopped))
                ; else we have found a solution so break the loop
                (setf check nil)
            )
        ))

        ; print the solution from GiL
        (print "Solution: ")
        #| (case species
            (1 (progn
                (print "PRINT 1st species")
                (print (list "(first *m-intervals-brut)" (gil::g-values sol (first *m-intervals-brut))))
                (print (list "*cf-brut-m-intervals     " (gil::g-values sol *cf-brut-m-intervals)))
                (print (list "(first *motions)       " (gil::g-values sol (first *motions))))
                (print (list "(first *h-intervals)     " (gil::g-values sol (first *h-intervals))))
            ))
            (2 (progn
                (print "PRINT 2nd species")
                (print (list "(first *cp)         " (gil::g-values sol (first *cp))))
                (print (list "(third *cp)         " (gil::g-values sol (third *cp))))
                (print (list "(third *h-intervals)" (gil::g-values sol (third *h-intervals))))
                (print (list "*m-all-intervals" (gil::g-values sol *m-all-intervals)))
                (print (list "*real-motions" (gil::g-values sol *real-motions)))
                (print (list "*penult-thesis-cost" (gil::g-values sol *penult-thesis-cost)))
            ))
            (3 (progn
                (print "PRINT 3rd species")
                (print (list "(first *cp) " (gil::g-values sol (first *cp))))
                (print (list "(second *cp)" (gil::g-values sol (second *cp))))
                (print (list "(third *cp) " (gil::g-values sol (third *cp))))
                (print (list "(fourth *cp)" (gil::g-values sol (fourth *cp))))
                (print (list "*extended-cp-domain" *extended-cp-domain))
                (print (list "(first *h-intervals) " (gil::g-values sol (first *h-intervals))))
                (print (list "(second *h-intervals)" (gil::g-values sol (second *h-intervals))))
                (print (list "(third *h-intervals) " (gil::g-values sol (third *h-intervals))))
                (print (list "(fourth *h-intervals)" (gil::g-values sol (fourth *h-intervals))))
                (print (list "*m-all-intervals" (gil::g-values sol *m-all-intervals)))
                ; (print (list "(fourth *m-intervals-brut)" (gil::g-values sol (fourth *m-intervals-brut))))
                ; (print (list "(first *motions) " (gil::g-values sol (first *motions))))
                (print (list "(fourth *motions)" (gil::g-values sol (fourth *motions))))
                (print (list "*not-cambiata-cost " (gil::g-values sol *not-cambiata-cost)))
                (print (list "*m2-eq-zero-cost   " (gil::g-values sol *m2-eq-zero-cost)))
                ; (print (list "(first *cons-cost)  " (gil::g-values sol (first *cons-cost))))
                ; (print (list "(second *cons-cost) " (gil::g-values sol (second *cons-cost))))
                ; (print (list "(third *cons-cost)  " (gil::g-values sol (third *cons-cost))))
                ; (print (list "(fourth *cons-cost) " (gil::g-values sol (fourth *cons-cost))))
            ))
            (4 (progn
                (print "PRINT 4th species")
                (print (list "(first *cp)         " (gil::g-values sol (first *cp))))
                (print (list "(third *cp)         " (gil::g-values sol (third *cp))))
                (print (list "(first *h-intervals)" (gil::g-values sol (first *h-intervals))))
                (print (list "(third *h-intervals)" (gil::g-values sol (third *h-intervals))))
                (print (list "*m-all-intervals         " (gil::g-values sol *m-all-intervals)))
                (print (list "(third *m-intervals)     " (gil::g-values sol (third *m-intervals))))
                (print (list "(first *m-succ-intervals) " (gil::g-values sol (first *m-succ-intervals))))
                (print (list "*no-syncope-cost" (gil::g-values sol *no-syncope-cost)))
            ))
            (5 (progn
                (print "PRINT 5th species")
                (print (list "(first *cp) " (gil::g-values sol (first *cp))))
                (print (list "(second *cp)" (gil::g-values sol (second *cp))))
                (print (list "(third *cp) " (gil::g-values sol (third *cp))))
                (print (list "(fourth *cp)" (gil::g-values sol (fourth *cp))))
                (print (list "(first *h-intervals) " (gil::g-values sol (first *h-intervals))))
                (print (list "(second *h-intervals)" (gil::g-values sol (second *h-intervals))))
                (print (list "(third *h-intervals) " (gil::g-values sol (third *h-intervals))))
                (print (list "(fourth *h-intervals)" (gil::g-values sol (fourth *h-intervals))))
                (print (list "*m-all-intervals" (gil::g-values sol *m-all-intervals)))
                ; (print (list "(fourth *m-intervals-brut)" (gil::g-values sol (fourth *m-intervals-brut))))
                ; (print (list "(first *motions) " (gil::g-values sol (first *motions))))
                (print (list "(fourth *motions)" (gil::g-values sol (fourth *motions))))
                (print (list "*not-cambiata-cost " (gil::g-values sol *not-cambiata-cost)))
                (print (list "*m2-eq-zero-cost   " (gil::g-values sol *m2-eq-zero-cost)))
                ; (print (list "(first *cons-cost)  " (gil::g-values sol (first *cons-cost))))
                (print (list "(second *cons-cost) " (gil::g-values sol (second *cons-cost))))
                (print (list "(third *cons-cost)  " (gil::g-values sol (third *cons-cost))))
                (print (list "(fourth *cons-cost) " (gil::g-values sol (fourth *cons-cost))))
                (print (list "*species-arr" sol-species))
                (print (list "*sp-arr1" (gil::g-values sol (first *sp-arr))))
                (print (list "*sp-arr2" (gil::g-values sol (second *sp-arr))))
                (print (list "*sp-arr3" (gil::g-values sol (third *sp-arr))))
                (print (list "*sp-arr4" (gil::g-values sol (fourth *sp-arr))))
            ))
        )
        (print (list "*m-degrees-cost    " (gil::g-values sol *m-degrees-cost)))
        (print (list "*m-degrees-type    " (gil::g-values sol *m-degrees-type)))
        (print (list "*off-key-cost     " (gil::g-values sol *off-key-cost)))
        (print (list "*fifth-cost  " (gil::g-values sol *fifth-cost)))
        (print (list "*octave-cost " (gil::g-values sol *octave-cost)))
        (print (list "*cost-factors" (gil::g-values sol *cost-factors)))
        (print (list "### COST ### " (gil::g-values sol *total-cost)))
        (print (list "scale         " *scale))
        (print (list "borrowed-scale" *borrowed-scale))
        (print (list "off-scale     " (reverse *off-scale))) |#
        (setq sol-pitches (gil::g-values sol the-cp)) ; store the values of the solution
        (print sol-pitches)
        (case species
            (4 (progn
                (setq rythmic+pitches (get-basic-rythmic 4 *cf-len sol-pitches)) ; get the rythmic correpsonding to the species
                (setq rythmic-om (first rythmic+pitches))
                (setq pitches-om (second rythmic+pitches))
            ))
            (5 (progn
                (setq sol-species (gil::g-values sol *species-arr)) ; store the values of the solution
                (setq rythmic+pitches (parse-species-to-om-rythmic sol-species sol-pitches))
                (setq rythmic-om (first rythmic+pitches))
                ; (print (list "rythmic-om" rythmic-om))
                (setq pitches-om (second rythmic+pitches))
                ; (print (list "pitches-om" pitches-om))
                (setq check (checksum-sol pitches-om rythmic-om))
                ; (print (list "check" check))
                (if (not (null *prev-sol-check))
                    ; then compare the pitches of the previous solution with the current one
                    ; if they are the same launch a new search
                    (if (member check *prev-sol-check)
                        (progn
                            (search-next-fux-cp l)
                        )
                        (progn
                            (print *prev-sol-check)
                            (setq *prev-sol-check (append *prev-sol-check (list check)))
                        )
                    )
                    ; else register the pitches of the current solution
                    (progn
                        (setq *prev-sol-check (list check))
                    )
                )
            ))
            (otherwise (progn
                (setq rythmic-om (get-basic-rythmic species *cf-len)) ; get the rythmic correpsonding to the species
                (setq pitches-om sol-pitches)
            ))
        )
        (make-instance 'voice :chords (to-midicent pitches-om) :tree (om::mktree rythmic-om '(4 4)) :tempo *cf-tempo)
    )
)

; try to find a solution, catch errors from GiL and Gecode and restart the search
(defun try-find-solution (se)
    (handler-case
        (gil::search-next se) ; search the next solution, sol is the space of the solution
        (error (c)
            (print "gil::ERROR")
            (try-find-solution se)
        )
    )
)

; determines if the search has been stopped by the solver because there are no more solutions or if the user has stopped the search
(defun stopped-or-ended (stopped-se stop-user)
    (print (list "stopped-se" stopped-se "stop-user" stop-user))
    (if (= stopped-se 0); if the search has not been stopped by the TimeStop object, there is no more solutions
        (error "There are no more solutions.")
    )
    ;otherwise, check if the user wants to keep searching or not
    (if stop-user
        (error "The search has been stopped. Press next to continue the search.")
    )
)
