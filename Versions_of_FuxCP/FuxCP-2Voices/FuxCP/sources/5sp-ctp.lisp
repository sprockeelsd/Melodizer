(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains the function that adds all the necessary constraints to the fifth species.

;;==========================#
;; FIFTH SPECIES            #
;;==========================#
;; Note: fux-cp-5th execute the first species algorithm without some constraints.
;; In this function, 4 notes by measure are assumed.
(defun fux-cp-5th (&optional (species 5))
    "Create the CSP for the 3rd species of Fux's counterpoint, with the cantus firmus as input"
    (print "Creating the CSP for the 3rd species of Fux's counterpoint...")

    ;; CLEANING PREVIOUS SOLUTIONS
    (setq *prev-sol-check nil)
    (setq rythmic+pitches nil)
    (setq rythmic-om nil)
    (setq pitches-om nil)

    (print "########## FIFTH SPECIES ##########")

    ;======================================== CREATION OF BOOLEAN SPECIES ARRAYS ==============
    (print "Creation of boolean species arrays...")
    ; total length of the counterpoint array
    (setq *total-cp-len (+ *cf-len (* *cf-last-index 3)))
    ; array representing the species type [0: no constraint, 1: 1st species, 2: 2nd species, 3: 3rd species, 4: 4th species]
    (setq *species-arr (gil::add-int-var-array *sp* *total-cp-len 0 4))
    (create-species-arr *species-arr)
    ; arrays representing if a note is constraint by a species
    (setf (nth 0 *is-nth-species-arr) (gil::add-bool-var-array *sp* *total-cp-len 0 1))
    (create-simple-boolean-arr *species-arr gil::IRT_EQ 0 (nth 0 *is-nth-species-arr))
    (setf (nth 1 *is-nth-species-arr) (gil::add-bool-var-array *sp* *total-cp-len 0 1))
    (create-simple-boolean-arr *species-arr gil::IRT_EQ 1 (nth 1 *is-nth-species-arr))
    (setf (nth 2 *is-nth-species-arr) (gil::add-bool-var-array *sp* *total-cp-len 0 1))
    (create-simple-boolean-arr *species-arr gil::IRT_EQ 2 (nth 2 *is-nth-species-arr))
    (setf (nth 3 *is-nth-species-arr) (gil::add-bool-var-array *sp* *total-cp-len 0 1))
    (create-simple-boolean-arr *species-arr gil::IRT_EQ 3 (nth 3 *is-nth-species-arr))
    (setf (nth 4 *is-nth-species-arr) (gil::add-bool-var-array *sp* *total-cp-len 0 1))
    (create-simple-boolean-arr *species-arr gil::IRT_EQ 4 (nth 4 *is-nth-species-arr))
    
    ; creating boolean is constrained array
    (print "Creating is constrained array...")
    ; array of BoolVar representing if the interval is constrained
    (setq *is-constrained-arr (collect-not-array (nth 0 *is-nth-species-arr)))


    ;======================================== CREATION OF GIL ARRAYS ==========================
    (print "Initializing variables...")
    
    (loop for i from 0 to 3 do
        (if (eq i 0)
            (progn
                ; add all quarter notes to the space with the domain *cp-domain
                (setf (nth i *cp) (gil::add-int-var-array-dom *sp* *cf-len *extended-cp-domain))
                ; then add to the penultimate note more possibilities
                (if (is-borrow-allowed)
                    (setf (nth *cf-penult-index (nth i *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain))
                )
                ; creating harmonic intervals array
                (print "Creating harmonic intervals array...")
                ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the counterpoint
                (setf (nth i *h-intervals) (gil::add-int-var-array *sp* *cf-len 0 11))
                (create-h-intervals (nth i *cp) *cf (nth i *h-intervals))
            )
            (progn
                ; same as above but 1 note shorter
                (setf (nth i *cp) (gil::add-int-var-array-dom *sp* *cf-last-index *extended-cp-domain))
                (if (is-borrow-allowed)
                    (setf (nth *cf-penult-index (nth i *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain))
                )
                (setf (nth i *h-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 11))
                (create-h-intervals (nth i *cp) (butlast *cf) (nth i *h-intervals))
            )
        )
    )

    (loop for i from 0 to 2 do
        (setq i+1 (+ i 1))
        (setf (nth i *m-succ-intervals-brut) (gil::add-int-var-array *sp* *cf-last-index -12 12))
        (if (eq i 1)
            ; then melodic interval could be 0 if there was a dissonant syncope before (see that later)
            (setf (nth i *m-succ-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 12))
            ; else no melodic interval of 0
            (setf (nth i *m-succ-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 12))
        )
        (create-intervals (nth i *cp) (nth i+1 *cp) (nth i *m-succ-intervals) (nth i *m-succ-intervals-brut))
    )

    
    ; merging all cp arrays into one
    (print "Mergin cps...")
    (setq *total-cp (gil::add-int-var-array *sp* *total-cp-len 0 127)) ; array of IntVar representing thesis and arsis notes combined
    (merge-cp *cp *total-cp) ; merge the four counterpoint arrays into one

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the melodic intervals between arsis and next thesis note of the counterpoint
    (setf (third *m-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 16))
    (setf (third *m-intervals-brut) (gil::add-int-var-array *sp* *cf-last-index -16 16)) ; same without absolute reduction
    (create-m-intervals-next-meas (third *cp) (first *cp) (third *m-intervals) (third *m-intervals-brut))
    ; array of IntVar representing the absolute intervals
    ; between the last note of measure m and the first note of measure m+1 of the counterpoint
    (setf (fourth *m-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 12)) ; can be 0 if this is replace by 2 eight note
    (setf (fourth *m-intervals-brut) (gil::add-int-var-array *sp* *cf-last-index -12 12)) ; same without absolute reduction
    (create-m-intervals-next-meas (fourth *cp) (first *cp) (fourth *m-intervals) (fourth *m-intervals-brut))
    
    ; array of IntVar representing the melodic intervals between the thesis note and the arsis note of the same measure
    (setq *m-ta-intervals (gil::add-int-var-array *sp* *cf-last-index 0 16))
    (setq *m-ta-intervals-brut (gil::add-int-var-array *sp* *cf-last-index -16 16)) ; same without absolute reduction
    (create-intervals (first *cp) (third *cp) *m-ta-intervals *m-ta-intervals-brut)
    
    ; creating melodic intervals array between the note n and n+2 for the whole counterpoint
    (setq *m2-len (- (* *cf-last-index 4) 1)) ; number of melodic intervals between n and n+2 for the total counterpoint
    (setq *m2-intervals (gil::add-int-var-array *sp* *m2-len 0 16))
    (setq *m2-intervals-brut (gil::add-int-var-array *sp* *m2-len -16 16))
    (create-m2-intervals *total-cp *m2-intervals *m2-intervals-brut)
    
    ; creating melodic intervals array between the note n and n+1 for the whole counterpoint
    (setq *total-m-len (* *cf-last-index 4)) ; number of melodic intervals between n and n+1 for the total counterpoint
    (setq *m-all-intervals (gil::add-int-var-array *sp* *total-m-len 0 12))
    (setq *m-all-intervals-brut (gil::add-int-var-array *sp* *total-m-len -12 12))
    (create-m-intervals-self *total-cp *m-all-intervals *m-all-intervals-brut *is-constrained-arr)

    ; creating motion array
    ; 0 = contrary, 1 = oblique, 2 = direct/parallel
    (print "Creating motion array...")
    (setf (fourth *motions) (gil::add-int-var-array *sp* *cf-last-index 0 2))
    (setf (fourth *motions-cost) (gil::add-int-var-array-dom *sp* *cf-last-index *motions-domain*))
    (create-motions (fourth *m-intervals-brut) *cf-brut-m-intervals (fourth *motions) (fourth *motions-cost))

    ; creating boolean is cantus firmus bass array
    (print "Creating is cantus firmus bass array...")
    ; array of BoolVar representing if the cantus firmus is lower than the arsis counterpoint
    (setf (first *is-cf-bass-arr) (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-cf-bass-arr (first *cp) (rest *cf) (first *is-cf-bass-arr)) ; 5th
    (setf (third *is-cf-bass-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-cf-bass-arr (third *cp) (butlast *cf) (third *is-cf-bass-arr)) ; 5th
    (setf (fourth *is-cf-bass-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-cf-bass-arr (fourth *cp) (butlast *cf) (fourth *is-cf-bass-arr))

    ; creating boolean are five consecutive notes by joint degree array
    (print "Creating are five consecutive notes by joint degree array...")
    ; array of BoolVar representing if the five consecutive notes are by joint degree
    (setq *is-5qn-linked-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-5qn-linked-arr *m-all-intervals *m-all-intervals-brut *is-5qn-linked-arr)
    (setq *is-mostly-3rd-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1)) ; 5th
    (create-is-mostly-3rd-arr (nth 3 *is-nth-species-arr) *is-mostly-3rd-arr)

    ; creating boolean is consonant array + species array
    (print "Creating is consonant array and species array...")
    (loop for i from 0 to 3 do
        ; array of BoolVar representing if the interval is consonant
        (if (eq i 0)
            (progn
                (setf (nth i *is-cons-arr) (gil::add-bool-var-array *sp* *cf-len 0 1))
                (setf (nth i *is-3rd-species-arr) (gil::add-bool-var-array *sp* *cf-len 0 1))
                (setf (nth i *is-4th-species-arr) (gil::add-bool-var-array *sp* *cf-len 0 1))
                (setf (nth i *is-cst-arr) (gil::add-bool-var-array *sp* *cf-len 0 1))
            )
            (progn
                (setf (nth i *is-cons-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
                (setf (nth i *is-3rd-species-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
                (setf (nth i *is-4th-species-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
                (setf (nth i *is-cst-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
            )
        )
        (create-is-member-arr (nth i *h-intervals) (nth i *is-cons-arr))
        (create-by-4 (nth 3 *is-nth-species-arr) (nth i *is-3rd-species-arr) i)
        (create-by-4 (nth 4 *is-nth-species-arr) (nth i *is-4th-species-arr) i)
        (create-by-4 *is-constrained-arr (nth i *is-cst-arr) i)
    )

    ; creating boolean diminution array
    (print "Creating diminution array...")
    ; Note: a diminution is the intermediate note that exists between two notes separated by a jump of a third
    ; i.e. E -> D (dim) -> C
    (setq *is-ta-dim-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-ta-dim-arr (second *m-succ-intervals) (collect-by-4 *m2-intervals 1 T) (third *m-succ-intervals) *is-ta-dim-arr)

    ; creating boolean is not cambiata array
    (print "Creating is not cambiata array...")
    (setq *is-not-cambiata-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-not-cambiata-arr (second *is-cons-arr) (third *is-cons-arr) (second *m-succ-intervals) *is-not-cambiata-arr)

    ; creating boolean is counterpoint off key array
    (print "Creating is counterpoint off key array...")
    (setq *is-cp-off-key-arr (gil::add-bool-var-array *sp* *total-cp-len 0 1))
    (create-is-member-arr *total-cp *is-cp-off-key-arr *off-domain)

    ; creating perfect consonances boolean array
    (print "Creating perfect consonances boolean array...")
    ; array of BoolVar representing if the interval between the cantus firmus and the counterpoint is a perfect consonance
    (setq *is-p-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first *h-intervals) *is-p-cons-arr)

    ; creation boolean is no syncope array
    (print "Creating is no syncope array...")
    ; array of BoolVar representing if the thesis note is note related to the previous one
    (setq *is-no-syncope-arr (gil::add-bool-var-array *sp* *cf-penult-index 0 1))
    (create-is-no-syncope-arr (third *m-intervals) *is-no-syncope-arr)


    ;======================================== HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    ; one possible value for non-constrained notes
    (print "One possible value for non-constrained notes...")
    (add-one-possible-value-cst *total-cp (nth 0 *is-nth-species-arr))

    ; perfect consonances should be used at the start and at the end of the piece
    (print "Perfect consonances at the start and at the end...")
    ; if first note is constrained then it must be a perfect consonance
    (add-p-cons-cst-if (first (first *h-intervals)) (first *is-constrained-arr))
    ; if first note is not constrained then the third note must be a perfect consonance
    (add-p-cons-cst-if (first (third *h-intervals)) (first (nth 0 *is-nth-species-arr)))
    ; no matter what species it is, the last harmonic interval must be a perfect consonance
    (add-p-cons-end-cst (first *h-intervals))
    
    ; if penultimate measure, a major sixth or a minor third must be used
    ; depending if the cantus firmus is at the bass or on the top part
    (print "Penultimate measure...")
    (add-penult-cons-cst (lastone (fourth *is-cf-bass-arr)) (lastone (fourth *h-intervals))
        (penult (nth 3 *is-nth-species-arr))
    ) ; 3rd species
    ; the third note of the penultimate measure must be below the fourth one. (3rd species)
    (gil::g-rel-reify *sp* (lastone (third *m-succ-intervals-brut)) gil::IRT_GR 1
        (penult (nth 3 *is-nth-species-arr)) gil::RM_IMP
    ) ; 3rd species
    ; the second note and the third note of the penultimate measure must be
    ; distant by greater than 1 semi-tone from the fourth note (3rd species)
    (gil::g-rel-reify *sp* (penult *m2-intervals) gil::IRT_NQ 1
        (nth (total-index *cf-penult-index 1) (nth 3 *is-nth-species-arr)) gil::RM_IMP
    ) ; 3rd species
    
    ; for the 4th species, the thesis note must be a seventh or a second and the arsis note must be a major sixth or a minor third
    ; major sixth or minor third
    (add-penult-cons-cst (lastone (third *is-cf-bass-arr)) (lastone (third *h-intervals))
        (penult (butlast (nth 4 *is-nth-species-arr)))
    ) ; 4th species
    ; seventh or second
    ; (note: a => !b <=> !(a ^ b)), so here we use the negation of the conjunction
    (gil::g-op *sp* (penult (first *is-4th-species-arr)) gil::BOT_AND (penult (first *is-cons-arr)) 0) ; 4th species

    ; every thesis note should be consonant if it does not belong to the fourth species (or not constrained at all)
    (print "Every thesis note should be consonant...")
    (add-h-cons-cst-if (first *is-cons-arr) (collect-by-4 (nth 1 *is-nth-species-arr))) ; 1st species
    (add-h-cons-cst-if (first *is-cons-arr) (collect-by-4 (nth 2 *is-nth-species-arr))) ; 2nd species
    (add-h-cons-cst-if (first *is-cons-arr) (first *is-3rd-species-arr)) ; 3rd species
    (add-h-cons-cst-if (third *is-cons-arr) (third *is-4th-species-arr)) ; 4th species
    (add-h-cons-cst-if (first *is-cons-arr) (collect-bot-array (rest (first *is-4th-species-arr)) *is-no-syncope-arr)) ; 4th species

    ; five consecutive notes by joint degree implies that the first and the third note are consonants
    (print "Five consecutive notes by joint degree...") ; 3rd species
    (add-linked-5qn-cst (third *is-cons-arr) (collect-bot-array *is-5qn-linked-arr *is-mostly-3rd-arr))

    ; any dissonant note implies that it is surrounded by consonant notes
    (print "Any dissonant note...") ; 3rd species
    (add-h-dis-or-cons-3rd-cst
        (second *is-cons-arr)
        (collect-t-or-f-array (third *is-cons-arr) (third *is-3rd-species-arr))
        (fourth *is-cons-arr)
        *is-ta-dim-arr
    )

    ; no seventh dissonance if the cantus firmus is at the top
    (print "No seventh dissonance if the cantus firmus is at the top...")
    (add-no-seventh-cst (first *h-intervals) (first *is-cf-bass-arr) (first *is-4th-species-arr)) ; 4th species


    ;======================================== MELODIC CONSTRAINTS =============================
    (print "Melodic constraints...")

    ; no melodic interval between 9 and 11
    (add-no-m-jump-extend-cst *m-all-intervals (collect-bot-array (butlast *is-constrained-arr) (rest *is-constrained-arr)))

    ; no unisson between two consecutive notes
    ; exept for in the second part or the fourth part of the measure
    (print "No unisson between two consecutive notes...")
    ; if 1st note and 2nd note exists (it means it belongs to a species)
    (add-no-unisson-at-all-cst
        (first *cp) (second *cp)
        (collect-bot-array (first *is-cst-arr) (second *is-cst-arr))
    ) ; 5th
    (add-no-unisson-at-all-cst
        (third *cp) (fourth *cp)
        (collect-bot-array (third *is-cst-arr) (fourth *is-cst-arr))
    ) ; 5th

    ; melodic intervals between thesis and arsis note from the same measure
    ; can't be greater than a minor sixth expect the octave (just for the fourth species)
    (print "No more than minor sixth melodic interval between arsis and thesis notes...")
    ; only applied if the the second note is not constrained
    (add-no-m-jump-extend-cst *m-ta-intervals (collect-by-4 (nth 0 *is-nth-species-arr) 1)) ; 4th species

    ; no same syncopation if 4th species
    (add-no-same-syncopation-cst (first *cp) (third *cp) (collect-bot-array (first *is-4th-species-arr) (third *is-cst-arr)))


    ;======================================== MOTION CONSTRAINTS ============================
    (print "Motion constraints...")

    ; no direct motion to reach a perfect consonance
    (print "No direct motion to reach a perfect consonance...")
    (add-no-direct-move-to-p-cons-cst (fourth *motions) (collect-bot-array *is-p-cons-arr (fourth *is-3rd-species-arr)) nil) ; 3rd species

    ; no battuta kind of motion
    ; i.e. contrary motion to an *octave, lower voice up, higher voice down, counterpoint melodic interval < -4
    (print "No battuta kind of motion...")
    (add-no-battuta-cst
        (fourth *motions) (first *h-intervals) (fourth *m-intervals-brut) (fourth *is-cf-bass-arr) (fourth *is-3rd-species-arr)
    ) ; 3rd species

    ; dissonant notes must be followed by the consonant note below
    (print "Dissonant notes must be followed by the consonant note below...")
    (add-h-dis-imp-cons-below-cst *m-ta-intervals-brut (first *is-cons-arr) (first *is-4th-species-arr)) ; TODO 4th species

    ; no second dissonance if the cantus firmus is at the bass and a octave/unisson precedes it
    (print "No second dissonance if the cantus firmus is at the bass...")
    (add-no-second-cst
        (third *h-intervals) (rest (first *h-intervals)) (rest (first *is-cf-bass-arr))
        (rest (first *is-4th-species-arr))
    ) ; TODO 4th species

    ; Marcel's rule
    (add-contrary-step-after-skip-cst *m-all-intervals *m-all-intervals-brut)


    ;======================================== COST FACTORS ====================================
    (set-cost-factors)
    (print "Imperfect consonances are preferred to perfect consonances...")
    (setq *fifth-cost  (gil::add-int-var-array-dom *sp* *cf-len (getparam-dom 'h-fifth-cost))) ; IntVar array representing the cost to have fifths
    (setq *octave-cost (gil::add-int-var-array-dom *sp* *cf-len (getparam-dom 'h-octave-cost))) ; IntVar array representing the cost to have octaves
    (add-cost-cst-if (first *h-intervals) gil::IRT_EQ 7 (first *is-cst-arr) *fifth-cost *h-fifth-cost*) ; *fifth-cost = 1 if *h-interval == 7
    (add-cost-cst-if (first *h-intervals) gil::IRT_EQ 0 (first *is-cst-arr) *octave-cost *h-octave-cost*) ; *octave-cost = 1 if *h-interval == 0
    (add-cost-to-factors *fifth-cost)
    (add-cost-to-factors *octave-cost)

    ; 3, 4) add off-key cost, m-degrees cost and tritons cost
    (set-general-costs-cst *total-cp-len *is-constrained-arr (collect-bot-array (butlast *is-constrained-arr) (rest *is-constrained-arr)))
    
    ; 5) contrary motion is preferred
    (add-cost-to-factors (fourth *motions))

    ; 6) cambiata notes are preferred (cons - dis - cons > cons - cons - cons)
    (print "Cambiata notes are preferred...")
    ; IntVar array representing the cost to have cambiata notes
    (setq *not-cambiata-cost (gil::add-int-var-array-dom *sp* *cf-last-index (getparam-dom 'non-cambiata-cost)))
    (add-cost-bool-cst-if *is-not-cambiata-arr *is-mostly-3rd-arr *not-cambiata-cost *non-cambiata-cost*)
    (add-cost-to-factors *not-cambiata-cost)

    ; 7) intervals between notes n and n+2 are prefered greater than zero
    (print "Intervals between notes n and n+2 are prefered different than zero...")
    ; IntVar array representing the cost to have intervals between notes n and n+2 equal to zero
    (setq *m2-eq-zero-cost (gil::add-int-var-array-dom *sp* *m2-len (getparam-dom 'two-beats-apart-cost)))
    (add-cost-cst-if
        *m2-intervals gil::IRT_EQ 0
        (collect-bot-array (butlast (butlast *is-constrained-arr)) (rest (rest *is-constrained-arr)))
        *m2-eq-zero-cost *two-beats-apart-cost*
    )
    (add-cost-to-factors *m2-eq-zero-cost)

    ; 8) add no syncopation cost
    (setq *no-syncope-cost (gil::add-int-var-array-dom *sp* *cf-penult-index (getparam-dom 'no-syncopation-cost)))
    (add-cost-cst-if
        (butlast (third *m-intervals)) gil::IRT_NQ 0
        (third *is-4th-species-arr)
        *no-syncope-cost
        *no-syncopation-cost*
    )
    (add-cost-to-factors *no-syncope-cost)


    ;======================================== COST FUNCTION ===================================
    (print "Cost function...")
    
    (loop for i from 0 to 3 do
        (setf (nth i *cons-cost) (gil::add-int-var-array *sp* *cf-last-index 0 1)) ; IntVar representing the cost to have a consonance
        (add-cost-bool-cst (nth i *is-cons-arr) (nth i *cons-cost)) ; *cons-cost = 1 if *is-cons-arr == 1
    )


    (print *extended-cp-domain)

    ; RETURN
    (if (eq species 5)
        ; then create the search engine
        ; (append (fux-search-engine *total-cp) (list species))
        (append (fux-search-engine *total-cp 5) '(5))
        ; else
        nil
    )
)