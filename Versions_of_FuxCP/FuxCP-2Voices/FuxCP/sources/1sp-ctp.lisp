(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains the function that adds all the necessary constraints to the first species.

;;==========================#
;; FIRST SPECIES            #
;;==========================#
(defun fux-cp-1st (&optional (species 1))
    "Create the CSP for the first species of Fux's counterpoint."

    ;============================================ CREATING GIL ARRAYS =============================
    ;; initialize the variables
    (print "Initializing variables...")
    
    ; add the counterpoint array to the space with the domain *cp-domain
    (setf (first *cp) (gil::add-int-var-array-dom *sp* *cf-len *extended-cp-domain))
    
    (if (and (eq species 1) (is-borrow-allowed))
        ; then add to the penultimate note more possibilities
        (setf (nth *cf-penult-index (first *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain))
    )
    ; creating harmonic intervals array
    (print "Creating harmonic intervals array...")

    ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the counterpoint
    (setf (first *h-intervals) (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first *cp) *cf (first *h-intervals))

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the absolute intervals between two notes in a row of the counterpoint
    (setf (first *m-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 12))
    (setf (first *m-intervals-brut) (gil::add-int-var-array *sp* *cf-last-index -12 12))
    (create-m-intervals-self (first *cp) (first *m-intervals) (first *m-intervals-brut))

    (if (eq species 1) ; only for the first species
        ; then
        (progn
            ; creating melodic intervals array between the note n and n+2
            (setq *m2-intervals (gil::add-int-var-array *sp* *cf-penult-index 0 12))
            (setq *m2-intervals-brut (gil::add-int-var-array *sp* *cf-penult-index -12 12))
            (create-m2-intervals (first *cp) *m2-intervals *m2-intervals-brut)
            
            ; creating boolean is counterpoint off key array
            (print "Creating is counterpoint off key array...")
            (setq *is-cp-off-key-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
            (create-is-member-arr (first *cp) *is-cp-off-key-arr *off-domain)
        )
    )

    ; creating perfect consonances boolean array
    (print "Creating perfect consonances boolean array...")
    ; array of BoolVar representing if the interval between the cantus firmus and the counterpoint is a perfect consonance
    (setq *is-p-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first *h-intervals) *is-p-cons-arr)

    ; creating order/role of pitch array (if cantus firmus is higher or lower than counterpoint)
    ; 0 for being the bass, 1 for being above
    (print "Creating order of pitch array...")
    (setf (first *is-cf-bass-arr) (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-cf-bass-arr (first *cp) *cf (first *is-cf-bass-arr))

    ; creating motion array
    (print "Creating motion array...")
    (setf (first *motions) (gil::add-int-var-array *sp* *cf-last-index 0 2)) ; 0 = contrary, 1 = oblique, 2 = direct/parallel
    (setf (first *motions-cost) (gil::add-int-var-array-dom *sp* *cf-last-index *motions-domain*))
    (create-motions (first *m-intervals-brut) *cf-brut-m-intervals (first *motions) (first *motions-cost))


    ;============================================ HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    ; for all intervals between the cantus firmus and the counterpoint, the interval must be a consonance
    (print "Harmonic consonances...")
    (case species
        (1 (add-h-cons-cst *cf-len *cf-penult-index (first *h-intervals)))
        (2 (add-h-cons-cst *cf-len *cf-penult-index (first *h-intervals) PENULT_THESIS_VAR))
        (3 (add-h-cons-cst *cf-len *cf-penult-index (first *h-intervals) PENULT_1Q_VAR))
        (otherwise (error "Species not supported"))
    )

    ; no unisson between the cantus firmus and the counterpoint unless it is the first note or the last note
    (print "No unisson...")
    (add-no-unisson-cst (first *cp) *cf)

    (if (/= species 3)
        ; then
        (progn
        ; must start with a perfect consonance
        (print "Perfect consonance at the beginning...")
        (add-p-cons-start-cst (first *h-intervals))

        ; must end with a perfect consonance
        (print "Perfect consonance at the end...")
        (add-p-cons-end-cst (first *h-intervals))
        )
    )

    ; if penultimate measure, a major sixth or a minor third must be used
    ; depending if the cantus firmus is at the bass or on the top part
    (print "Penultimate measure...")
    (if (eq species 1)
        ; then
        (add-penult-cons-cst (penult (first *is-cf-bass-arr)) (penult (first *h-intervals)))
    )


    ;============================================ MELODIC CONSTRAINTS =============================
    
    ; NOTE: with the degree iii in penultimate *cf measure -> no solution bc there is a *tritone between I#(minor third) and V.
    (print "Melodic constraints...")
    (if (eq species 1)
        ; then
        (progn
            ; no more than minor sixth melodic interval
            (print "No more than minor sixth...")
            (add-no-m-jump-cst (first *m-intervals))

            ; no *chromatic motion between three consecutive notes
            (print "No chromatic motion...")
            (add-no-chromatic-m-cst (first *m-intervals-brut) *m2-intervals-brut)

            ;==================================== MOTION CONSTRAINTS ============================
            (print "Motion constraints...")

            ; no direct motion to reach a perfect consonance
            (print "No direct motion to reach a perfect consonance...")
            (add-no-direct-move-to-p-cons-cst (first *motions) *is-p-cons-arr)

            ; no battuta kind of motion
            ; i.e. contrary motion to an *octave, lower voice up, higher voice down, counterpoint melodic interval < -4
            (print "No battuta kind of motion...")
            (add-no-battuta-cst (first *motions) (first *h-intervals) (first *m-intervals-brut) (first *is-cf-bass-arr))
        )
    )
    

    ;============================================ COST FACTORS ====================================
    (print "Cost function...")

    (if (eq species 1)
        ; then
        (progn
            (setq *m-all-intervals (first *m-intervals))
            (set-cost-factors)
            ; 1, 2) imperfect consonances are preferred to perfect consonances
            (print "Imperfect consonances are preferred to perfect consonances...")
            (add-p-cons-cost-cst)
            ; 3, 4) add off-key cost, m-degrees cost and tritons cost
            (set-general-costs-cst *cf-len)

            ; 5) motion costs
            (add-cost-to-factors (first *motions-cost))
        )
    )


    ; RETURN
    (if (eq species 1)
        ; then create the search engine
        (append (fux-search-engine (first *cp)) (list species))
        ; else
        nil
    )
)