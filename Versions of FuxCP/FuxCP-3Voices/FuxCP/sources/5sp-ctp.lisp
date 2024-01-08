(in-package :fuxcp)

; Author: Thibault Wafflard, adapted by Anton Lamotte
; Date: June 3, 2023, adapted January 2024
; This file contains the function that adds all the necessary constraints to the fifth species.

;;==========================#
;; FIFTH SPECIES            #
;;==========================#
;; Note: fux-cp-5th execute the first species algorithm without some constraints.
;; In this function, 4 notes by measure are assumed.
(defun fux-cp-5th (counterpoint &optional (species 5))
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
    (setf (solution-len counterpoint) (+ *cf-len (* *cf-last-index 3)))
    ; array representing the species type [0: no constraint, 1: 1st species, 2: 2nd species, 3: 3rd species, 4: 4th species]
    (setf (species-arr counterpoint) (gil::add-int-var-array *sp* (solution-len counterpoint) 0 4))
    (create-species-arr (species-arr counterpoint) (solution-len counterpoint))
    ; arrays representing if a note is constraint by a species
    (setf (nth 0 (is-nth-species-arr counterpoint)) (gil::add-bool-var-array *sp* (solution-len counterpoint) 0 1))
    (create-simple-boolean-arr (species-arr counterpoint) gil::IRT_EQ 0 (nth 0 (is-nth-species-arr counterpoint)))
    (setf (nth 1 (is-nth-species-arr counterpoint)) (gil::add-bool-var-array *sp* (solution-len counterpoint) 0 1))
    (create-simple-boolean-arr (species-arr counterpoint) gil::IRT_EQ 1 (nth 1 (is-nth-species-arr counterpoint)))
    (setf (nth 2 (is-nth-species-arr counterpoint)) (gil::add-bool-var-array *sp* (solution-len counterpoint) 0 1))
    (create-simple-boolean-arr (species-arr counterpoint) gil::IRT_EQ 2 (nth 2 (is-nth-species-arr counterpoint)))
    (setf (nth 3 (is-nth-species-arr counterpoint)) (gil::add-bool-var-array *sp* (solution-len counterpoint) 0 1))
    (create-simple-boolean-arr (species-arr counterpoint) gil::IRT_EQ 3 (nth 3 (is-nth-species-arr counterpoint)))
    (setf (nth 4 (is-nth-species-arr counterpoint)) (gil::add-bool-var-array *sp* (solution-len counterpoint) 0 1))
    (create-simple-boolean-arr (species-arr counterpoint) gil::IRT_EQ 4 (nth 4 (is-nth-species-arr counterpoint)))
    
    ; creating boolean is constrained array
    (print "Creating is constrained array...")
    ; array of BoolVar representing if the interval is constrained
    (setf (is-constrained-arr counterpoint) (collect-not-array (nth 0 (is-nth-species-arr counterpoint))))


    ;======================================== CREATION OF GIL ARRAYS ==========================
    (print "Initializing variables...")
    
    (loop for i from 0 to 3 do
        (if (eq i 0)
            (progn
                ; creating harmonic intervals array
                (print "Creating harmonic intervals array...")
                ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the counterpoint
                (setf (nth i (h-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-len 0 11))
                (create-h-intervals (nth i (notes counterpoint)) (first (notes *lowest)) (nth i (h-intervals counterpoint)))
    
                (setf (nth i (h-intervals-to-cf counterpoint)) (gil::add-int-var-array *sp* *cf-len 0 11))
                (create-h-intervals (nth i (notes counterpoint)) *cf (nth i (h-intervals-to-cf counterpoint)))
            )
            (progn
                ; same as above but 1 note shorter
                (setf (nth i (h-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 11))
                (create-h-intervals (nth i (notes counterpoint)) (butlast (first (notes *lowest))) (nth i (h-intervals counterpoint)))

                (setf (nth i (h-intervals-to-cf counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 11))
                (create-h-intervals (nth i (notes counterpoint)) (butlast *cf) (nth i (h-intervals-to-cf counterpoint)))

            )
        )
    )

    (loop for i from 0 to 2 do
        (setq i+1 (+ i 1))
        (setf (nth i (m-succ-intervals-brut counterpoint)) (gil::add-int-var-array *sp* *cf-last-index -12 12))
        (if (eq i 1)
            ; then melodic interval could be 0 if there was a dissonant syncope before (see that later)
            (setf (nth i (m-succ-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 12))
            ; else no melodic interval of 0
            (setf (nth i (m-succ-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 12))
        )
        (create-intervals (nth i (notes counterpoint)) (nth i+1 (notes counterpoint)) (nth i (m-succ-intervals counterpoint)) (nth i (m-succ-intervals-brut counterpoint)))
    )

    
    ; merging all cp arrays into one
    (print "Merging cps...")
    (setf (solution-array counterpoint) (gil::add-int-var-array *sp* (solution-len counterpoint) 0 127)) ; array of IntVar representing thesis and arsis notes combined
    (merge-cp (notes counterpoint) (solution-array counterpoint)) ; merge the four counterpoint arrays into one

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the melodic intervals between arsis and next thesis note of the counterpoint
    (setf (third (m-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 16))
    ;(setf (third (m-intervals-brut counterpoint)) (gil::add-int-var-array *sp* *cf-last-index -16 16)) ; same without absolute reduction
    (create-m-intervals-next-meas (third (notes counterpoint)) (first (notes counterpoint)) (third (m-intervals counterpoint)) (third (m-intervals-brut counterpoint)))
    ; array of IntVar representing the absolute intervals
    ; between the last note of measure m and the first note of measure m+1 of the counterpoint
    (setf (fourth (m-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 12)) ; can be 0 if this is replace by 2 eight note

    #| next line defined in init-counterpoint |#
    ; (setf (fourth (m-intervals-brut counterpoint)) (gil::add-int-var-array *sp* *cf-last-index -12 12)) ; same without absolute reduction
    (create-m-intervals-next-meas (fourth (notes counterpoint)) (first (notes counterpoint)) (fourth (m-intervals counterpoint)) (fourth (m-intervals-brut counterpoint)))
    
    ; array of IntVar representing the melodic intervals between the thesis note and the arsis note of the same measure
    (setf (m-ta-intervals counterpoint) (gil::add-int-var-array *sp* *cf-last-index 0 16))
    (setf (m-ta-intervals-brut counterpoint) (gil::add-int-var-array *sp* *cf-last-index -16 16)) ; same without absolute reduction
    (create-intervals (first (notes counterpoint)) (third (notes counterpoint)) (m-ta-intervals counterpoint) (m-ta-intervals-brut counterpoint))
    
    ; creating melodic intervals array between the note n and n+2 for the whole counterpoint
    (setf (m2-len counterpoint) (- (* *cf-last-index 4) 1)) ; number of melodic intervals between n and n+2 for the total counterpoint
    (setf (m2-intervals counterpoint) (gil::add-int-var-array *sp* (m2-len counterpoint) 0 16))
    (setf (m2-intervals-brut counterpoint) (gil::add-int-var-array *sp* (m2-len counterpoint) -16 16))
    (create-m2-intervals (solution-array counterpoint) (m2-intervals counterpoint) (m2-intervals-brut counterpoint))
    
    ; creating melodic intervals array between the note n and n+1 for the whole counterpoint
    (setf (total-m-len counterpoint) (* *cf-last-index 4)) ; number of melodic intervals between n and n+1 for the total counterpoint
    (setf (m-all-intervals counterpoint) (gil::add-int-var-array *sp* (total-m-len counterpoint) 0 12))
    (setf (m-all-intervals-brut counterpoint) (gil::add-int-var-array *sp* (total-m-len counterpoint) -12 12))
    (create-m-intervals-self (solution-array counterpoint) (m-all-intervals counterpoint) (m-all-intervals-brut counterpoint) (is-constrained-arr counterpoint))

    ; creating motion array
    ; 0 = contrary, 1 = oblique, 2 = direct/parallel
    (print "Creating motion array...")
    (setf (fourth (motions counterpoint)) (gil::add-int-var-array *sp* *cf-last-index -1 2))
    (setf (fourth (motions-cost counterpoint)) (gil::add-int-var-array-dom *sp* *cf-last-index *motions-domain*))
    (create-motions (fourth (m-intervals-brut counterpoint)) (first (m-intervals-brut *lowest)) (fourth (motions counterpoint)) (fourth (motions-cost counterpoint)) (is-not-lowest counterpoint))

    ; creating boolean is cantus firmus bass array
    (print "Creating is cantus firmus bass array...")
    ; array of BoolVar representing if the cantus firmus is lower than the arsis counterpoint
    (setf (first (is-cf-lower-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-cf-lower-arr (first (notes counterpoint)) (rest *cf) (first (is-cf-lower-arr counterpoint))) ; 5th
    (setf (third (is-cf-lower-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-cf-lower-arr (third (notes counterpoint)) (butlast *cf) (third (is-cf-lower-arr counterpoint))) ; 5th
    (setf (fourth (is-cf-lower-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-cf-lower-arr (fourth (notes counterpoint)) (butlast *cf) (fourth (is-cf-lower-arr counterpoint)))

    ; creating boolean are five consecutive notes by joint degree array
    (print "Creating are five consecutive notes by joint degree array...")
    ; array of BoolVar representing if the five consecutive notes are by joint degree
    (setf (is-5qn-linked-arr counterpoint) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-5qn-linked-arr (m-all-intervals counterpoint) (m-all-intervals-brut counterpoint) (is-5qn-linked-arr counterpoint))
    (setf (is-mostly-3rd-arr counterpoint) (gil::add-bool-var-array *sp* *cf-last-index 0 1)) ; 5th
    (create-is-mostly-3rd-arr (nth 3 (is-nth-species-arr counterpoint)) (is-mostly-3rd-arr counterpoint))

    ; creating boolean is consonant array + species array
    (print "Creating is consonant array and species array...")
    (loop for i from 0 to 3 do
        ; array of BoolVar representing if the interval is consonant
        (if (eq i 0)
            (progn
                (setf (nth i (is-cons-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-len 0 1))
                (setf (nth i (is-3rd-species-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-len 0 1))
                (setf (nth i (is-4th-species-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-len 0 1))
                (setf (nth i (is-cst-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-len 0 1))
            )
            (progn
                (setf (nth i (is-cons-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
                (setf (nth i (is-3rd-species-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
                (setf (nth i (is-4th-species-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
                (setf (nth i (is-cst-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
            )
        )
        (create-is-member-arr (nth i (h-intervals counterpoint)) (nth i (is-cons-arr counterpoint)))
        (create-by-4 (nth 3 (is-nth-species-arr counterpoint)) (nth i (is-3rd-species-arr counterpoint)) i)
        (create-by-4 (nth 4 (is-nth-species-arr counterpoint)) (nth i (is-4th-species-arr counterpoint)) i)
        (create-by-4 (is-constrained-arr counterpoint) (nth i (is-cst-arr counterpoint)) i)
    )

    ; creating boolean diminution array
    (print "Creating diminution array...")
    ; Note: a diminution is the intermediate note that exists between two notes separated by a jump of a third
    ; i.e. E -> D (dim) -> C
    (setf (is-ta-dim-arr counterpoint) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-ta-dim-arr (second (m-succ-intervals counterpoint)) (collect-by-4 (m2-intervals counterpoint) 1 T) (third (m-succ-intervals counterpoint)) (is-ta-dim-arr counterpoint))

    ; creating boolean is not cambiata array
    (print "Creating is not cambiata array...")
    (setf (is-not-cambiata-arr counterpoint) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-not-cambiata-arr (second (is-cons-arr counterpoint)) (third (is-cons-arr counterpoint)) (second (m-succ-intervals counterpoint)) (is-not-cambiata-arr counterpoint))

    ; creating boolean is counterpoint off key array
    (print "Creating is counterpoint off key array...")
    (setf (is-cp-off-key-arr counterpoint) (gil::add-bool-var-array *sp* (solution-len counterpoint) 0 1))
    (create-is-member-arr (solution-array counterpoint) (is-cp-off-key-arr counterpoint) (off-domain counterpoint))

    ; creating perfect consonances boolean array
    (print "Creating perfect consonances boolean array...")
    ; array of BoolVar representing if the interval between the cantus firmus and the counterpoint is a perfect consonance
    (setf (is-p-cons-arr counterpoint) (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first (h-intervals counterpoint)) (is-p-cons-arr counterpoint))

    ; creation boolean is no syncope array
    (print "Creating is no syncope array...")
    ; array of BoolVar representing if the thesis note is note related to the previous one
    (setf (is-no-syncope-arr counterpoint) (gil::add-bool-var-array *sp* *cf-penult-index 0 1))
    (create-is-no-syncope-arr (third (m-intervals counterpoint)) (is-no-syncope-arr counterpoint))


    ;======================================== HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    ; one possible value for non-constrained notes
    (print "One possible value for non-constrained notes...")
    (add-one-possible-value-cst (solution-array counterpoint) (nth 0 (is-nth-species-arr counterpoint)))

    (if (eq *N-PARTS 2) (progn
        ; perfect consonances should be used at the start and at the end of the piece
        (print "Perfect consonances at the start and at the end...")
        ; if first note is constrained then it must be a perfect consonance
        (add-p-cons-cst-if (first (first (h-intervals counterpoint))) (first (is-constrained-arr counterpoint)))
        ; if first note is not constrained then the third note must be a perfect consonance
        (add-p-cons-cst-if (first (third (h-intervals counterpoint))) (first (nth 0 (is-nth-species-arr counterpoint))))
        ; no matter what species it is, the last harmonic interval must be a perfect consonance
        (add-p-cons-end-cst (first (h-intervals counterpoint)))
    
        ; if penultimate measure, a major sixth or a minor third must be used
        ; depending if the cantus firmus is at the bass or on the top part
        (print "Penultimate measure...")
            (add-penult-cons-cst (lastone (fourth (is-cf-lower-arr counterpoint))) (lastone (fourth (h-intervals-to-cf counterpoint)))
            (penult (nth 3 (is-nth-species-arr counterpoint)))
        ) ; 3rd species
    ))
    ; the third note of the penultimate measure must be below the fourth one. (3rd species)
    (gil::g-rel-reify *sp* (lastone (third (m-succ-intervals-brut counterpoint))) gil::IRT_GR 1
        (penult (nth 3 (is-nth-species-arr counterpoint))) gil::RM_IMP
    ) ; 3rd species
    ; the second note and the third note of the penultimate measure must be
    ; distant by greater than 1 semi-tone from the fourth note (3rd species)
    (gil::g-rel-reify *sp* (penult (m2-intervals counterpoint)) gil::IRT_NQ 1
        (nth (total-index *cf-penult-index 1) (nth 3 (is-nth-species-arr counterpoint))) gil::RM_IMP
    ) ; 3rd species
    (if (eq *N-PARTS 2) (progn
        ; for the 4th species, the thesis note must be a seventh or a second and the arsis note must be a major sixth or a minor third
        ; major sixth or minor third
        (add-penult-cons-cst (lastone (third (is-cf-lower-arr counterpoint))) (lastone (third (h-intervals-to-cf counterpoint)))
            (penult (butlast (nth 4 (is-nth-species-arr counterpoint))))
        ) ; 4th species
            ; seventh or second
        ; (note: a => !b <=> !(a ^ b)), so here we use the negation of the conjunction
    ))

    (if (eq *N-PARTS 3) (progn
        (print "Penultimate measure...")
        (gil::g-member *sp* PENULT_CONS_3P_VAR (lastone (third (h-intervals counterpoint))))
    ))

    (setf is-penult-cons-to-cf (gil::add-bool-var *sp* 0 1))
    (add-is-member-cst (penult (first (h-intervals-to-cf counterpoint))) ALL_CONS_VAR is-penult-cons-to-cf)
    (gil::g-op *sp* (penult (first (is-4th-species-arr counterpoint))) gil::BOT_AND is-penult-cons-to-cf 0) ; 4th species

    ; every thesis note should be consonant if it does not belong to the fourth species (or not constrained at all)
    (print "Every thesis note should be consonant...")
    (add-h-cons-cst-if (first (is-cons-arr counterpoint)) (collect-by-4 (nth 1 (is-nth-species-arr counterpoint)))) ; 1st species
    (add-h-cons-cst-if (first (is-cons-arr counterpoint)) (collect-by-4 (nth 2 (is-nth-species-arr counterpoint)))) ; 2nd species
    (add-h-cons-cst-if (first (is-cons-arr counterpoint)) (first (is-3rd-species-arr counterpoint))) ; 3rd species
    (add-h-cons-cst-if (third (is-cons-arr counterpoint)) (third (is-4th-species-arr counterpoint))) ; 4th species
    (add-h-cons-cst-if (first (is-cons-arr counterpoint)) (collect-bot-array (rest (first (is-4th-species-arr counterpoint))) (is-no-syncope-arr counterpoint))) ; 4th species

    ; five consecutive notes by joint degree implies that the first and the third note are consonants
    (print "Five consecutive notes by joint degree...") ; 3rd species
    (add-linked-5qn-cst (third (is-cons-arr counterpoint)) (collect-bot-array (is-5qn-linked-arr counterpoint) (is-mostly-3rd-arr counterpoint)))

    ; any dissonant note implies that it is surrounded by consonant notes
    (print "Any dissonant note...") ; 3rd species
    (add-h-dis-or-cons-3rd-cst
        (second (is-cons-arr counterpoint))
        (collect-t-or-f-array (third (is-cons-arr counterpoint)) (third (is-3rd-species-arr counterpoint)))
        (fourth (is-cons-arr counterpoint))
        (is-ta-dim-arr counterpoint)
    )

    ; no seventh dissonance if the cantus firmus is at the top
    (print "No seventh dissonance if the cantus firmus is at the top...")
    (add-no-seventh-cst (first (h-intervals counterpoint)) (is-not-lowest counterpoint) (first (is-4th-species-arr counterpoint))) ; 4th species


    ;======================================== MELODIC CONSTRAINTS =============================
    (print "Melodic constraints...")

    ; no melodic interval between 9 and 11
    (add-no-m-jump-extend-cst (m-all-intervals counterpoint) (collect-bot-array (butlast (is-constrained-arr counterpoint)) (rest (is-constrained-arr counterpoint))))

    ; no unison between two consecutive notes
    ; exept for in the second part or the fourth part of the measure
    (print "No unison between two consecutive notes...")
    ; if 1st note and 2nd note exists (it means it belongs to a species)
    (add-no-unison-at-all-cst
        (first (notes counterpoint)) (second (notes counterpoint))
        (collect-bot-array (first (is-cst-arr counterpoint)) (second (is-cst-arr counterpoint)))
    ) ; 5th
    (add-no-unison-at-all-cst
        (third (notes counterpoint)) (fourth (notes counterpoint))
        (collect-bot-array (third (is-cst-arr counterpoint)) (fourth (is-cst-arr counterpoint)))
    ) ; 5th

    ; melodic intervals between thesis and arsis note from the same measure
    ; can't be greater than a minor sixth expect the octave (just for the fourth species)
    (print "No more than minor sixth melodic interval between arsis and thesis notes...")
    ; only applied if the the second note is not constrained
    (add-no-m-jump-extend-cst (m-ta-intervals counterpoint) (collect-by-4 (nth 0 (is-nth-species-arr counterpoint)) 1)) ; 4th species

    ; no same syncopation if 4th species
    (add-no-same-syncopation-cst (first (notes counterpoint)) (third (notes counterpoint)) (collect-bot-array (first (is-4th-species-arr counterpoint)) (third (is-cst-arr counterpoint))))


    ;======================================== MOTION CONSTRAINTS ============================
    (print "Motion constraints...")

    ; no direct motion to reach a perfect consonance
    (print "No direct motion to reach a perfect consonance...")
    (if (eq species 5) (add-no-direct-move-to-p-cons-cst (fourth (motions counterpoint)) (collect-bot-array (is-p-cons-arr counterpoint) (fourth (is-3rd-species-arr counterpoint))) (is-not-lowest counterpoint) nil)) ; 3rd species

    ; no battuta kind of motion
    ; i.e. contrary motion to an *octave, lower voice up, higher voice down, counterpoint melodic interval < -4
    (print "No battuta kind of motion...")
    (add-no-battuta-cst
        (fourth (motions counterpoint)) (first (h-intervals counterpoint)) (fourth (m-intervals-brut counterpoint)) (fourth (is-cf-lower-arr counterpoint)) (fourth (is-3rd-species-arr counterpoint))
    ) ; 3rd species

    ; dissonant notes must be followed by the consonant note below
    (print "Dissonant notes must be followed by the consonant note below...")
    (add-h-dis-imp-cons-below-cst (m-ta-intervals-brut counterpoint) (first (is-cons-arr counterpoint)) (first (is-4th-species-arr counterpoint))) ; TODO 4th species

    ; no second dissonance if the cantus firmus is at the bass and a octave/unison precedes it
    (print "No second dissonance if the cantus firmus is at the bass...")
    (add-no-second-cst
        (third (h-intervals counterpoint)) (rest (first (h-intervals counterpoint))) (rest (is-not-lowest counterpoint))
        (rest (first (is-4th-species-arr counterpoint)))
    ) ; TODO 4th species

    ; Marcel's rule
    (add-contrary-step-after-skip-cst (m-all-intervals counterpoint) (m-all-intervals-brut counterpoint))


    ;======================================== COST FACTORS ====================================
    (print "Imperfect consonances are preferred to perfect consonances...")
    (setf (fifth-cost counterpoint)  (gil::add-int-var-array-dom *sp* *cf-len (getparam-dom 'h-fifth-cost))) ; IntVar array representing the cost to have fifths
    (setf (octave-cost counterpoint) (gil::add-int-var-array-dom *sp* *cf-len (getparam-dom 'h-octave-cost))) ; IntVar array representing the cost to have octaves
    (add-cost-cst-if (first (h-intervals counterpoint)) gil::IRT_EQ 7 (first (is-cst-arr counterpoint)) (fifth-cost counterpoint) *h-fifth-cost*) ; (fifth-cost counterpoint) = 1 if *h-interval == 7
    (let ((is-cst-and-not-bass-arr (gil::add-bool-var-array *sp* *cf-len 0 1)))
        (dotimes (i *cf-len)
            (gil::g-op *sp* (nth i (first (is-cst-arr counterpoint))) gil::BOT_AND (nth i (is-not-lowest counterpoint)) (nth i is-cst-and-not-bass-arr))
        )
        (add-cost-cst-if (first (h-intervals counterpoint)) gil::IRT_EQ 0 is-cst-and-not-bass-arr (octave-cost counterpoint) *h-octave-cost*) ; (octave-cost counterpoint) = 1 if *h-interval == 0
    )
    (add-cost-to-factors (fifth-cost counterpoint) 'fifth-cost)
    (add-cost-to-factors (octave-cost counterpoint) 'octave-cost)

    ; 3, 4) add off-key cost, m-degrees cost and tritons cost
    (set-general-costs-cst counterpoint (solution-len counterpoint) (is-constrained-arr counterpoint) (collect-bot-array (butlast (is-constrained-arr counterpoint)) (rest (is-constrained-arr counterpoint))))
    
    ; 5) contrary motion is preferred
    (add-cost-to-factors (fourth (motions-cost counterpoint)) 'motions-cost)

    ; 6) cambiata notes are preferred (cons - dis - cons > cons - cons - cons)
    (print "Cambiata notes are preferred...")
    ; IntVar array representing the cost to have cambiata notes
    (setf (not-cambiata-cost counterpoint) (gil::add-int-var-array-dom *sp* *cf-last-index (getparam-dom 'non-cambiata-cost)))
    (add-cost-bool-cst-if (is-not-cambiata-arr counterpoint) (is-mostly-3rd-arr counterpoint) (not-cambiata-cost counterpoint) *non-cambiata-cost*)
    (add-cost-to-factors (not-cambiata-cost counterpoint) 'non-cambiata-cost)

    ; 7) intervals between notes n and n+2 are prefered greater than zero
    (print "Intervals between notes n and n+2 are prefered different than zero...")
    ; IntVar array representing the cost to have intervals between notes n and n+2 equal to zero
    (setf (m2-eq-zero-cost counterpoint) (gil::add-int-var-array-dom *sp* (m2-len counterpoint) (getparam-dom 'm2-eq-zero-cost)))
    (add-cost-cst-if
        (m2-intervals counterpoint) gil::IRT_EQ 0
        (collect-bot-array (butlast (butlast (is-constrained-arr counterpoint))) (rest (rest (is-constrained-arr counterpoint))))
        (m2-eq-zero-cost counterpoint) *m2-eq-zero-cost*
    )
    (add-cost-to-factors (m2-eq-zero-cost counterpoint) 'm2-eq-zero-cost)

    ; 8) add no syncopation cost
    (setf (no-syncope-cost counterpoint) (gil::add-int-var-array-dom *sp* *cf-penult-index (getparam-dom 'no-syncopation-cost)))
    (add-cost-cst-if
        (butlast (third (m-intervals counterpoint))) gil::IRT_NQ 0
        (third (is-4th-species-arr counterpoint))
        (no-syncope-cost counterpoint)
        *no-syncopation-cost*
    )
    (add-cost-to-factors (no-syncope-cost counterpoint) 'no-syncope-cost)


    ;======================================== COST FUNCTION ===================================
    (print "Cost function...")
    
    (loop for i from 0 to 3 do
        (setf (nth i (cons-cost counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 1)) ; IntVar representing the cost to have a consonance
        (add-cost-bool-cst (nth i (is-cons-arr counterpoint)) (nth i (cons-cost counterpoint))) ; (cons-cost counterpoint) = 1 if (is-cons-arr counterpoint) == 1
    )

    ; RETURN
    (if (eq species 5)
        ; then create the search engine
        (append (fux-search-engine (solution-array counterpoint) '(5)) (list (list 5)) (voice-type counterpoint))
        ; else if 3v
        nil
    )
)