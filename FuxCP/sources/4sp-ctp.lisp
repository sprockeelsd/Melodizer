(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains the function that adds all the necessary constraints to the fourth species.

;;==========================#
;; FOURTH SPECIES           #
;;==========================#
;; Note: fux-cp-4th execute the first species algorithm without some constraints.
;; In this function, the first notes are in Arsis because of the syncopation.
(defun fux-cp-4th (&optional (species 4))
    "Create the CSP for the 2nd species of Fux's counterpoint, with the cantus firmus as input"

    (print "########## FOURTH SPECIES ##########")

    ;======================================== CREATION OF GIL ARRAYS ==========================
    (print "Initializing variables...")
    ; add the arsis counterpoint array (of [*cf-len - 1] length) to the space with the domain *cp-domain
    (setf (third *cp) (gil::add-int-var-array-dom *sp* *cf-last-index *extended-cp-domain))
    (setf (first *cp) (gil::add-int-var-array-dom *sp* *cf-last-index *extended-cp-domain))
    ; add to the penultimate note more possibilities
    (if (is-borrow-allowed)
        (progn
        (setf (nth *cf-penult-index (third *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain))
        (setf (nth *cf-penult-index (first *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain))
        )
    )
    
    ; merging cp and cp-arsis into one array
    (setq *total-cp-len (* *cf-last-index 2))
    (setq *total-cp (gil::add-int-var-array *sp* *total-cp-len 0 127)) ; array of IntVar representing thesis and arsis notes combined
    (merge-cp-same-len (list (third *cp) (first *cp)) *total-cp) ; merge the two counterpoint arrays into one
    
    ; creating harmonic intervals array
    (print "Creating harmonic intervals array...")
    ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the counterpoint (arsis notes)
    (setf (third *h-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 11))
    (setf (first *h-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 11))
    (create-h-intervals (third *cp) (butlast *cf) (third *h-intervals))
    (create-h-intervals (first *cp) (rest *cf) (first *h-intervals))
    

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the melodic intervals between arsis and next thesis note of the counterpoint
    (setf (third *m-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 8))
    (setf (third *m-intervals-brut) (gil::add-int-var-array *sp* *cf-last-index -12 12)) ; same without absolute reduction
    (create-intervals (third *cp) (first *cp) (third *m-intervals) (third *m-intervals-brut))
    ; array of IntVar representing the melodic intervals between a thesis and an arsis note of the same measure the counterpoint
    (setf (first *m-succ-intervals) (gil::add-int-var-array *sp* *cf-penult-index 1 12))
    (setf (first *m-succ-intervals-brut) (gil::add-int-var-array *sp* *cf-penult-index -12 12))
    (create-m-intervals-in-meas (first *cp) (rest (third *cp)) (first *m-succ-intervals) (first *m-succ-intervals-brut))

    
    ; creating melodic intervals array between the note n and n+2 for the whole counterpoint
    (setq *m2-len (- (* *cf-last-index 2) 2)) ; number of melodic intervals between n and n+2 for thesis and arsis notes combined
    (setq *m2-intervals (gil::add-int-var-array *sp* *m2-len 0 12))
    (setq *m2-intervals-brut (gil::add-int-var-array *sp* *m2-len -12 12))
    (create-m2-intervals *total-cp *m2-intervals *m2-intervals-brut)
    
    ; creating melodic intervals array between the note n and n+1 for the whole counterpoint
    (setq *total-m-len (- (* *cf-last-index 2) 1)) ; number of melodic intervals between n and n+1 for thesis and arsis notes combined
    (setq *m-all-intervals (gil::add-int-var-array *sp* *total-m-len 0 12))
    (setq *m-all-intervals-brut (gil::add-int-var-array *sp* *total-m-len -12 12))
    (create-m-intervals-self *total-cp *m-all-intervals *m-all-intervals-brut)

    ; creating perfect consonances boolean array
    (print "Creating perfect consonances boolean array...")
    ; array of BoolVar representing if the interval between the cantus firmus and the counterpoint is a perfect consonance
    (setq *is-p-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first *h-intervals) *is-p-cons-arr)

    ; creating boolean is cantus firmus bass array
    (print "Creating is cantus firmus bass array...")
    ; array of BoolVar representing if the cantus firmus is lower than the arsis counterpoint
    (setf (third *is-cf-bass-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (setf (first *is-cf-bass-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-cf-bass-arr (third *cp) (butlast *cf) (third *is-cf-bass-arr))
    (create-is-cf-bass-arr (first *cp) (rest *cf) (first *is-cf-bass-arr))

    ; creating boolean is counterpoint off key array
    (print "Creating is counterpoint off key array...")
    (setq *is-cp-off-key-arr (gil::add-bool-var-array *sp* *total-cp-len 0 1))
    (create-is-member-arr *total-cp *is-cp-off-key-arr *off-domain)

    ; creating boolean is consonant array
    (print "Creating is consonant array...")
    ; array of BoolVar representing if the interval is consonant
    (setf (first *is-cons-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-member-arr (first *h-intervals) (first *is-cons-arr))

    ; creation boolean is no syncope array
    (print "Creating is no syncope array...")
    ; array of BoolVar representing if the thesis note is note related to the previous one
    (setq *is-no-syncope-arr (gil::add-bool-var-array *sp* *cf-penult-index 0 1))
    (create-is-no-syncope-arr (third *m-intervals) *is-no-syncope-arr)


    ;======================================== HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    ; for all harmonic intervals between the cantus firmus and the thesis notes, the interval must be a consonance
    (print "Harmonic consonances...")
    ; here the penultimate thesis note must be a seventh or a second and the arsis note must be a major sixth or a minor third
    (add-penult-dom-cst (penult (first *h-intervals)) PENULT_SYNCOPE_VAR)
    (add-h-cons-cst *cf-len *cf-penult-index (third *h-intervals))
    (add-no-sync-h-cons (first *h-intervals) *is-no-syncope-arr)

    ; must start with a perfect consonance
    (print "Perfect consonance at the beginning...")
    (add-p-cons-start-cst (third *h-intervals))

    ; must end with a perfect consonance
    (print "Perfect consonance at the end...")
    (add-p-cons-end-cst (first *h-intervals))

    ; no seventh dissonance if the cantus firmus is at the top
    (print "No seventh dissonance if the cantus firmus is at the top...")
    (add-no-seventh-cst (first *h-intervals) (first *is-cf-bass-arr))

    ; if penultimate measure, a major sixth or a minor third must be used
    ; depending if the cantus firmus is at the bass or on the top part
    (print "Penultimate measure...")
    (add-penult-cons-cst (lastone (third *is-cf-bass-arr)) (lastone (third *h-intervals)))


    ;======================================== MELODIC CONSTRAINTS =============================
    (print "Melodic constraints...")

    ; melodic intervals cannot be greater than a minor sixth expect the octave
    (print "No more than minor sixth melodic interval between arsis and thesis notes...")
    (add-no-m-jump-extend-cst (first *m-succ-intervals))

    ; no *chromatic motion between three consecutive notes
    (print "No chromatic motion...")
    (add-no-chromatic-m-cst *m-all-intervals-brut *m2-intervals-brut)


    ;======================================== MOTION CONSTRAINTS ============================
    (print "Motion constraints...")

    ; dissonant notes must be followed by the consonant note below
    (print "Dissonant notes must be followed by the consonant note below...")
    (add-h-dis-imp-cons-below-cst (first *m-succ-intervals-brut) (first *is-cons-arr))

    ; no second dissonance if the cantus firmus is at the bass and a octave/unisson precedes it
    (print "No second dissonance if the cantus firmus is at the bass...")
    (add-no-second-cst (third *h-intervals) (first *h-intervals) (first *is-cf-bass-arr))


    ;======================================== COST FACTORS ====================================
    (print "Cost factors...")
    (set-cost-factors)
    ; 1, 2) imperfect consonances are preferred to perfect consonances
    (add-p-cons-cost-cst t)
    
    ; 3, 4) add off-key cost, m-degrees cost and tritons cost
    (set-general-costs-cst)

    ; 5) add no syncopation cost
    (print "No syncopation cost...")
    (setq *no-syncope-cost (gil::add-int-var-array-dom *sp* *cf-penult-index (getparam-dom 'no-syncopation-cost)))
    (add-cost-cst (butlast (third *m-intervals)) gil::IRT_NQ 0 *no-syncope-cost *no-syncopation-cost*)
    (add-cost-to-factors *no-syncope-cost)

    ; 6) add m2-intervals equal to 0 cost
    (print "Monotonia...")
    (setq *m2-eq-zero-cost (gil::add-int-var-array-dom *sp* (- *cf-len 3) (getparam-dom 'two-bars-apart-cost)))
    (add-cost-multi-cst (third *cp) gil::IRT_EQ (cddr (third *cp)) *m2-eq-zero-cost *two-bars-apart-cost*)
    (add-cost-to-factors *m2-eq-zero-cost)

    ;======================================== COST FUNCTION ===================================
    (print "Cost function...")

    ; RETURN
    (if (eq species 4)
        ; then create the search engine
        (append (fux-search-engine *total-cp 4) (list species))
        ; else
        nil
    )
)