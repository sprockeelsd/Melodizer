(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains the function that adds all the necessary constraints to the third species.

;;==========================#
;; THIRD SPECIES            #
;;==========================#
;; Note: fux-cp-3rd execute the first species algorithm without some constraints.
;; In this function, 4 quarter notes by measure are assumed.
(defun fux-cp-3rd (&optional (species 3))
    "Create the CSP for the 3rd species of Fux's counterpoint, with the cantus firmus as input"
    (print "Creating the CSP for the 3rd species of Fux's counterpoint...")

    ;; ADD FIRST SPECIES CONSTRAINTS
    (fux-cp-1st 3)

    (print "########## THIRD SPECIES ##########")

    ;======================================== CREATION OF GIL ARRAYS ==========================
    (print "Initializing variables...")
    
    (loop for i from 1 to 3 do
        ; add all quarter notes to the space with the domain *cp-domain
        (setf (nth i *cp) (gil::add-int-var-array-dom *sp* *cf-last-index *extended-cp-domain))
        
        (if (and (eq i 3) (is-borrow-allowed))
            ; then add to the penultimate note more possibilities
            (setf (nth *cf-penult-index (nth i *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain))
        )
    )

    (loop for i from 1 to 3 do
        (setq i-1 (- i 1))
        ; creating harmonic intervals array
        ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the counterpoint
        (setf (nth i *h-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 11))
        (create-h-intervals (nth i *cp) (butlast *cf) (nth i *h-intervals))

        ; array of IntVar representing the absolute intervals between a thesis and an arsis note of the same measure the counterpoint
        (setf (nth i-1 *m-succ-intervals) (gil::add-int-var-array *sp* *cf-last-index 1 12))
        (setf (nth i-1 *m-succ-intervals-brut) (gil::add-int-var-array *sp* *cf-last-index -12 12))
        (create-intervals (nth i-1 *cp) (nth i *cp) (nth i-1 *m-succ-intervals) (nth i-1 *m-succ-intervals-brut))
    )
    
    ; merging cp and cp-arsis into one array
    (print "Mergin cps...")
    (setq *total-cp-len (+ *cf-len (* *cf-last-index 3))) ; total length of the counterpoint array
    (setq *total-cp (gil::add-int-var-array *sp* *total-cp-len 0 127)) ; array of IntVar representing thesis and arsis notes combined
    (merge-cp *cp *total-cp) ; merge the four counterpoint arrays into one

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the absolute intervals
    ; between the last note of measure m and the first note of measure m+1 of the counterpoint
    (setf (fourth *m-intervals) (gil::add-int-var-array *sp* *cf-last-index 1 12))
    (setf (fourth *m-intervals-brut) (gil::add-int-var-array *sp* *cf-last-index -12 12)) ; same without absolute reduction
    (create-m-intervals-next-meas (fourth *cp) (first *cp) (fourth *m-intervals) (fourth *m-intervals-brut))
    
    ; creating melodic intervals array between the note n and n+2 for the whole counterpoint
    (setq *m2-len (- (* *cf-last-index 4) 1)) ; number of melodic intervals between n and n+2 for the total counterpoint
    (setq *m2-intervals (gil::add-int-var-array *sp* *m2-len 0 12))
    (setq *m2-intervals-brut (gil::add-int-var-array *sp* *m2-len -12 12))
    (create-m2-intervals *total-cp *m2-intervals *m2-intervals-brut)
    
    ; creating melodic intervals array between the note n and n+1 for the whole counterpoint
    (setq *total-m-len (* *cf-last-index 4)) ; number of melodic intervals between n and n+1 for the total counterpoint
    (setq *m-all-intervals (gil::add-int-var-array *sp* *total-m-len 0 12))
    (setq *m-all-intervals-brut (gil::add-int-var-array *sp* *total-m-len -12 12))
    (create-m-intervals-self *total-cp *m-all-intervals *m-all-intervals-brut)

    ; creating motion array
    ; 0 = contrary, 1 = oblique, 2 = direct/parallel
    (print "Creating motion array...")
    (setf (fourth *motions) (gil::add-int-var-array *sp* *cf-last-index 0 2))
    (setf (fourth *motions-cost) (gil::add-int-var-array-dom *sp* *cf-last-index *motions-domain*))
    (create-motions (fourth *m-intervals-brut) *cf-brut-m-intervals (fourth *motions) (fourth *motions-cost))

    ; creating boolean is cantus firmus bass array
    (print "Creating is cantus firmus bass array...")
    ; array of BoolVar representing if the cantus firmus is lower than the arsis counterpoint
    (setf (fourth *is-cf-bass-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-cf-bass-arr (fourth *cp) (butlast *cf) (fourth *is-cf-bass-arr))

    ; creating boolean are five consecutive notes by joint degree array
    (print "Creating are five consecutive notes by joint degree array...")
    ; array of BoolVar representing if the five consecutive notes are by joint degree
    (setq *is-5qn-linked-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-5qn-linked-arr *m-all-intervals *m-all-intervals-brut *is-5qn-linked-arr)

    ; creating boolean diminution array
    (print "Creating diminution array...")
    ; Note: a diminution is the intermediate note that exists between two notes separated by a jump of a third
    ; i.e. E -> D (dim) -> C
    (setq *is-ta-dim-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-ta-dim-arr (second *m-succ-intervals) (collect-by-4 *m2-intervals 1 T) (third *m-succ-intervals) *is-ta-dim-arr)

    ; creating boolean is consonant array
    (print "Creating is consonant array...")
    (loop for i from 0 to 3 do
        ; array of BoolVar representing if the interval is consonant
        (if (eq i 0)
            (setf (nth i *is-cons-arr) (gil::add-bool-var-array *sp* *cf-len 0 1))
            (setf (nth i *is-cons-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
        )
        (create-is-member-arr (nth i *h-intervals) (nth i *is-cons-arr))
    )

    ; creating boolean is not cambiata array
    (print "Creating is not cambiata array...")
    (setq *is-not-cambiata-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-not-cambiata-arr (second *is-cons-arr) (third *is-cons-arr) (second *m-succ-intervals) *is-not-cambiata-arr)

    ; creating boolean is counterpoint off key array
    (print "Creating is counterpoint off key array...")
    (setq *is-cp-off-key-arr (gil::add-bool-var-array *sp* *total-cp-len 0 1))
    (create-is-member-arr *total-cp *is-cp-off-key-arr *off-domain)


    ;======================================== HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")
    ; must start with a perfect consonance
    (print "Perfect consonance at the beginning...")
    (add-p-cons-start-cst (first *h-intervals))

    ; must end with a perfect consonance
    (print "Perfect consonance at the end...")
    (add-p-cons-end-cst (first *h-intervals))
    
    ; if penultimate measure, a major sixth or a minor third must be used
    ; depending if the cantus firmus is at the bass or on the top part
    (print "Penultimate measure...")
    (add-penult-cons-cst (lastone (fourth *is-cf-bass-arr)) (lastone (fourth *h-intervals)))
    ; the third note of the penultimate measure must be below the fourth one.
    (gil::g-rel *sp* (lastone (third *m-succ-intervals-brut)) gil::IRT_GR 1)
    ; the second note and the third note of the penultimate measure must be distant by greater than 1 semi-tone from the fourth note
    (gil::g-rel *sp* (penult *m2-intervals) gil::IRT_NQ 1)
    

    ; five consecutive notes by joint degree implies that the first and the third note are consonants
    (print "Five consecutive notes by joint degree...")
    (add-linked-5qn-cst (third *is-cons-arr) *is-5qn-linked-arr)

    ; any dissonant note implies that it is surrounded by consonant notes
    (print "Any dissonant note...")
    (add-h-dis-or-cons-3rd-cst (second *is-cons-arr) (third *is-cons-arr) (fourth *is-cons-arr) *is-ta-dim-arr)


    ;======================================== MELODIC CONSTRAINTS =============================
    (print "Melodic constraints...")

    ; no melodic interval between 9 and 11
    (loop for m in *m-succ-intervals do
        (add-no-m-jump-extend-cst m)
    )
    (add-no-m-jump-extend-cst (fourth *m-intervals))

    ; no *chromatic motion between three consecutive notes
    (print "No chromatic motion...")
    (add-no-chromatic-m-cst *m-all-intervals-brut *m2-intervals-brut)

    ; Marcel's rule: contrary melodic step after skip
    (print "Marcel's rule...")
    (add-contrary-step-after-skip-cst *m-all-intervals *m-all-intervals-brut)

    ;======================================== MOTION CONSTRAINTS ============================
    (print "Motion constraints...")

    ; no direct motion to reach a perfect consonance
    (print "No direct motion to reach a perfect consonance...")
    (add-no-direct-move-to-p-cons-cst (fourth *motions) *is-p-cons-arr)

    ; no battuta kind of motion
    ; i.e. contrary motion to an *octave, lower voice up, higher voice down, counterpoint melodic interval < -4
    (print "No battuta kind of motion...")
    (add-no-battuta-cst (fourth *motions) (first *h-intervals) (fourth *m-intervals-brut) (fourth *is-cf-bass-arr)) ; TODO

    ;======================================== COST FACTORS ====================================
    (set-cost-factors)
    ; 1, 2) imperfect consonances are preferred to perfect consonances
    (print "Imperfect consonances are preferred to perfect consonances...")
    (add-p-cons-cost-cst)

    ; 3, 4) add off-key cost, m-degrees cost and tritons cost
    (set-general-costs-cst)
    
    ; 5) contrary motion is preferred
    (add-cost-to-factors (fourth *motions-cost))

    ; 6) cambiata notes are preferred (cons - dis - cons > cons - cons - cons)
    (print "Cambiata notes are preferred...")
    ; IntVar array representing the cost to have cambiata notes
    (setq *not-cambiata-cost (gil::add-int-var-array-dom *sp* *cf-last-index (getparam-dom 'non-cambiata-cost)))
    (add-cost-bool-cst *is-not-cambiata-arr *not-cambiata-cost *non-cambiata-cost*)
    (add-cost-to-factors *not-cambiata-cost)

    ; 7) intervals between notes n and n+2 are prefered greater than zero
    (print "Intervals between notes n and n+2 are prefered different than zero...")
    ; IntVar array representing the cost to have intervals between notes n and n+2 equal to zero
    (setq *m2-eq-zero-cost (gil::add-int-var-array-dom *sp* *m2-len (getparam-dom 'two-beats-apart-cost)))
    (add-cost-cst *m2-intervals gil::IRT_EQ 0 *m2-eq-zero-cost *two-beats-apart-cost*)
    (add-cost-to-factors *m2-eq-zero-cost)


    ;======================================== COST FUNCTION ===================================
    (print "Cost function...")


    ; RETURN
    (if (eq species 3)
        ; then create the search engine
        (append (fux-search-engine *total-cp 3) (list species))
        ; else
        nil
    )
)