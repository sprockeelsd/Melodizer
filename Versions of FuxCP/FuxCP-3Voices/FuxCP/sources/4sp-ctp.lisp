(in-package :fuxcp)

; Author: Thibault Wafflard, adapted by Anton Lamotte
; Date: June 3, 2023, adapted January 2024
; This file contains the function that adds all the necessary constraints to the fourth species.

;;==========================#
;; FOURTH SPECIES           #
;;==========================#
;; Note: fux-cp-4th execute the first species algorithm without some constraints.
;; In this function, the first notes are in Arsis because of the syncopation.
(defun fux-cp-4th (counterpoint &optional (species 4))
    "Create the CSP for the 2nd species of Fux's counterpoint, with the cantus firmus as input"

    (print "########## FOURTH SPECIES ##########")

    ;======================================== CREATION OF GIL ARRAYS ==========================
    (print "Initializing variables...")

    
    ; merging cp and cp-arsis into one array
    (setf (solution-len counterpoint) (* *cf-last-index 2))
    (setf (solution-array counterpoint) (gil::add-int-var-array *sp* (solution-len counterpoint) 0 127)) ; array of IntVar representing thesis and arsis notes combined
    (merge-cp-same-len (list (third (notes counterpoint)) (first (notes counterpoint))) (solution-array counterpoint)) ; merge the two counterpoint arrays into one
    
    ; creating harmonic intervals array
    (print "Creating harmonic intervals array...")
    ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the counterpoint (arsis notes)
    (setf (third (h-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 11))
    (setf (first (h-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 11))
    (create-h-intervals (third (notes counterpoint)) (butlast (first (notes *lowest))) (third (h-intervals counterpoint)))
    (create-h-intervals (first (notes counterpoint)) (rest (first (notes *lowest))) (first (h-intervals counterpoint)))
    
    (setf (third (h-intervals-to-cf counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 11))
    (create-h-intervals (third (notes counterpoint)) (butlast *cf) (third (h-intervals-to-cf counterpoint)))
    

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the melodic intervals between arsis and next thesis note of the counterpoint
    (setf (third (m-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 8))
    
    #| next line defined in init-counterpoint |#
    ; (setf (third (m-intervals-brut counterpoint)) (gil::add-int-var-array *sp* *cf-last-index -12 12)) ; same without absolute reduction
    (create-intervals (third (notes counterpoint)) (first (notes counterpoint)) (third (m-intervals counterpoint)) (third (m-intervals-brut counterpoint)))
    ; array of IntVar representing the melodic intervals between a thesis and an arsis note of the same measure the counterpoint
    (setf (first (m-succ-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-penult-index 1 12))
    (setf (first (m-succ-intervals-brut counterpoint)) (gil::add-int-var-array *sp* *cf-penult-index -12 12))
    (create-m-intervals-in-meas (first (notes counterpoint)) (rest (third (notes counterpoint))) (first (m-succ-intervals counterpoint)) (first (m-succ-intervals-brut counterpoint)))

    
    ; creating melodic intervals array between the note n and n+2 for the whole counterpoint
    (setf (m2-len counterpoint) (- (* *cf-last-index 2) 2)) ; number of melodic intervals between n and n+2 for thesis and arsis notes combined
    (setf (m2-intervals counterpoint) (gil::add-int-var-array *sp* (m2-len counterpoint) 0 12))
    (setf (m2-intervals-brut counterpoint) (gil::add-int-var-array *sp* (m2-len counterpoint) -12 12))
    (create-m2-intervals (solution-array counterpoint) (m2-intervals counterpoint) (m2-intervals-brut counterpoint))
    
    ; creating melodic intervals array between the note n and n+1 for the whole counterpoint
    (setf (total-m-len counterpoint) (- (* *cf-last-index 2) 1)) ; number of melodic intervals between n and n+1 for thesis and arsis notes combined
    (setf (m-all-intervals counterpoint) (gil::add-int-var-array *sp* (total-m-len counterpoint) 0 12))
    (setf (m-all-intervals-brut counterpoint) (gil::add-int-var-array *sp* (total-m-len counterpoint) -12 12))
    (create-m-intervals-self (solution-array counterpoint) (m-all-intervals counterpoint) (m-all-intervals-brut counterpoint))

    ; creating perfect consonances boolean array
    (print "Creating perfect consonances boolean array...")
    ; array of BoolVar representing if the interval between the cantus firmus and the counterpoint is a perfect consonance
    (setf (is-p-cons-arr counterpoint) (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first (h-intervals counterpoint)) (is-p-cons-arr counterpoint))

    ; creating boolean is cantus firmus bass array
    (print "Creating is cantus firmus bass array...")
    ; array of BoolVar representing if the cantus firmus is lower than the arsis counterpoint
    (setf (third (is-cf-lower-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-cf-lower-arr (third (notes counterpoint)) (butlast *cf) (third (is-cf-lower-arr counterpoint)))

    ; creating boolean is counterpoint off key array
    (print "Creating is counterpoint off key array...")
    (setf (is-cp-off-key-arr counterpoint) (gil::add-bool-var-array *sp* (solution-len counterpoint) 0 1))
    (create-is-member-arr (solution-array counterpoint) (is-cp-off-key-arr counterpoint) (off-domain counterpoint))

    ; creating boolean is consonant array
    (print "Creating is consonant array...")
    ; array of BoolVar representing if the interval is consonant
    (setf (first (is-cons-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-member-arr (first (h-intervals counterpoint)) (first (is-cons-arr counterpoint)))

    ; creation boolean is no syncope array
    (print "Creating is no syncope array...")
    ; array of BoolVar representing if the thesis note is note related to the previous one
    (setf (is-no-syncope-arr counterpoint) (gil::add-bool-var-array *sp* *cf-penult-index 0 1))
    (create-is-no-syncope-arr (third (m-intervals counterpoint)) (is-no-syncope-arr counterpoint))


    ;======================================== HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    ; for all harmonic intervals between the cantus firmus and the thesis notes, the interval must be a consonance
    (print "Harmonic consonances...")
    ; here the penultimate thesis note must be a seventh or a second and the arsis note must be a major sixth or a minor third
    ;(add-penult-dom-cst (penult (first (h-intervals counterpoint))) PENULT_SYNCOPE_VAR)
    
    (add-h-cons-cst *cf-last-index *cf-penult-index (third (h-intervals counterpoint)) PENULT_CONS_VAR 4 (is-not-lowest counterpoint))
    (add-no-sync-h-cons (first (h-intervals counterpoint)) (is-no-syncope-arr counterpoint))

    ; no seventh dissonance if the cantus firmus is at the top
    (print "No seventh dissonance if the cantus firmus is at the top...")
    (add-no-seventh-cst (first (h-intervals counterpoint)) (is-not-lowest counterpoint))

    (if (eq *N-PARTS 2) (progn 
        ; must start with a perfect consonance
        (print "Perfect consonance at the beginning...")
        (add-p-cons-start-cst (third (h-intervals counterpoint)))

        ; must end with a perfect consonance
        (print "Perfect consonance at the end...")
        (add-p-cons-end-cst (first (h-intervals counterpoint)))

        ; if penultimate measure, a major sixth or a minor third must be used
        ; depending if the cantus firmus is at the bass or on the top part
        (print "Penultimate measure...")
        (add-penult-cons-cst (lastone (third (is-cf-lower-arr counterpoint))) (lastone (third (h-intervals-to-cf counterpoint))))
    ))

    (if (eq *N-PARTS 3) (progn
        (print "Penultimate measure...")
        (gil::g-member *sp* PENULT_SYNCOPE_VAR (lastone (third (h-intervals counterpoint))))
    ))

    ;======================================== MELODIC CONSTRAINTS =============================
    (print "Melodic constraints...")

    ; melodic intervals cannot be greater than a minor sixth expect the octave
    (print "No more than minor sixth melodic interval between arsis and thesis notes...")
    (add-no-m-jump-extend-cst (first (m-succ-intervals counterpoint)))

    ; no *chromatic motion between three consecutive notes
    (print "No chromatic motion...")
    (add-no-chromatic-m-cst (m-all-intervals-brut counterpoint) (m2-intervals-brut counterpoint))


    ;======================================== MOTION CONSTRAINTS ============================
    (print "Motion constraints...")

    ; dissonant notes must be followed by the consonant note below
    (print "Dissonant notes must be followed by the consonant note below...")

    (add-h-dis-imp-cons-below-cst (first (m-succ-intervals-brut counterpoint)) (first (is-cons-arr counterpoint)))

    ; no second dissonance if the cantus firmus is at the bass and a octave/unison precedes it
    (print "No second dissonance if the cantus firmus is at the bass...")
    (add-no-second-cst (third (h-intervals counterpoint)) (first (h-intervals counterpoint)) (is-not-lowest counterpoint))


    ;======================================== COST FACTORS ====================================
    (print "Cost factors...")    
    ; 1, 2) imperfect consonances are preferred to perfect consonances
    (add-p-cons-cost-cst (h-intervals counterpoint) (is-not-lowest counterpoint) t)
    
    ; 3, 4) add off-key cost, m-degrees cost and tritons cost
    (set-general-costs-cst counterpoint (solution-len counterpoint))

    ; 5) add no syncopation cost
    (print "No syncopation cost...")
    (setf (no-syncope-cost counterpoint) (gil::add-int-var-array-dom *sp* *cf-penult-index (getparam-dom 'no-syncopation-cost)))
    (add-cost-cst (butlast (third (m-intervals counterpoint))) gil::IRT_NQ 0 (no-syncope-cost counterpoint) *no-syncopation-cost*)
    (add-cost-to-factors (no-syncope-cost counterpoint) 'no-syncope-cost)

    ; 6) add m2-intervals equal to 0 cost
    (print "Monotonia...")
    (setf (m2-eq-zero-cost counterpoint) (gil::add-int-var-array-dom *sp* (- *cf-len 3) (getparam-dom 'm2-eq-zero-cost)))
    (add-cost-multi-cst (third (notes counterpoint)) gil::IRT_EQ (cddr (third (notes counterpoint))) (m2-eq-zero-cost counterpoint) *m2-eq-zero-cost*)
    (add-cost-to-factors (m2-eq-zero-cost counterpoint) 'm2-eq-zero-cost)

    ;======================================== COST FUNCTION ===================================
    (print "Cost function...")

    ; RETURN
    (if (eq species 4)
        ; then create the search engine
        (append (fux-search-engine (solution-array counterpoint) '(4)) (list (list 4)))
        ; else if 3v
        nil
    )
)