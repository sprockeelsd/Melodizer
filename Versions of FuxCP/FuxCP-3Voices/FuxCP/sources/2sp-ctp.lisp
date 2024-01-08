(in-package :fuxcp)

; Author: Thibault Wafflard, adapted by Anton Lamotte
; Date: June 3, 2023, adapted January 2024
; This file contains the function that adds all the necessary constraints to the second species.

;;==========================#
;; SECOND SPECIES           #
;;==========================#
;; Note: fux-cp-2nd execute the first species algorithm without some constraints.
;; In this function, all the variable names without the arsis-suffix refers to thesis notes AKA the first species notes.
;; All the variable names with the arsis-suffix refers to arsis notes AKA notes on the upbeat.
(defun fux-cp-2nd (counterpoint &optional (species 2))
    "Create the CSP for the 2nd species of Fux's counterpoint, with the cantus firmus as input"
    (print "########## SECOND SPECIES ##########")

    ;; ADD FIRST SPECIES CONSTRAINTS
    (fux-cp-1st counterpoint species)
    ;======================================== CREATION OF GIL ARRAYS ==========================
    (print "Initializing variables...")
    
    ; merging cp and cp-arsis into one array
    (setf (solution-len counterpoint) (+ *cf-len *cf-last-index))
    (setf (solution-array counterpoint) (gil::add-int-var-array *sp* (solution-len counterpoint) 0 127)) ; array of IntVar representing thesis and arsis notes combined
    (merge-cp (list (first (notes counterpoint)) (third (notes counterpoint))) (solution-array counterpoint)) ; merge the two counterpoint arrays into one
    
    ; creating harmonic intervals array
    (print "Creating harmonic intervals array...")
    ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the counterpoint (arsis notes)
    (setf (third (h-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 11))
    (create-h-intervals (third (notes counterpoint)) (butlast (first (notes *lowest))) (third (h-intervals counterpoint)))
    ; array of IntVar representing the absolute intervals (not % 12) and brut (just p - q)
    ; between the cantus firmus and the counterpoint (thesis notes)
    (setf (h-intervals-abs counterpoint) (gil::add-int-var-array *sp* *cf-len 0 127))
    (setf (h-intervals-brut counterpoint) (gil::add-int-var-array *sp* *cf-len -127 127))
    (create-intervals (first (notes *lowest)) (first (notes counterpoint)) (h-intervals-abs counterpoint) (h-intervals-brut counterpoint))
    

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the melodic intervals between arsis note and next thesis note of the counterpoint
    (setf (third (m-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 12))
    #| next line defined in init-counterpoint |#
    ;(setf (third (m-intervals-brut counterpoint)) (gil::add-int-var-array *sp* *cf-last-index -12 12)) ; same without absolute reduction
    (create-m-intervals-next-meas (third (notes counterpoint)) (first (notes counterpoint)) (third (m-intervals counterpoint)) (third (m-intervals-brut counterpoint)))
    ; array of IntVar representing the melodic intervals between a thesis and an arsis note of the same measure the counterpoint
    (setf (first (m-succ-intervals counterpoint)) (gil::add-int-var-array *sp* *cf-last-index 0 12))
    (setf (first (m-succ-intervals-brut counterpoint)) (gil::add-int-var-array *sp* *cf-last-index -12 12))
    (create-m-intervals-in-meas (first (notes counterpoint)) (third (notes counterpoint)) (first (m-succ-intervals counterpoint)) (first (m-succ-intervals-brut counterpoint)))

    
    ; creating melodic intervals array between the note n and n+2 for the whole counterpoint
    (setf (m2-len counterpoint) (- (* *cf-last-index 2) 1)) ; number of melodic intervals between n and n+2 for thesis and arsis notes combined
    (setf (m2-intervals counterpoint) (gil::add-int-var-array *sp* (m2-len counterpoint) 0 12))
    (setf (m2-intervals-brut counterpoint) (gil::add-int-var-array *sp* (m2-len counterpoint) -12 12))
    (create-m2-intervals (solution-array counterpoint) (m2-intervals counterpoint) (m2-intervals-brut counterpoint))
    
    ; creating melodic intervals array between the note n and n+1 for the whole counterpoint
    (setf (total-m-len counterpoint) (* *cf-last-index 2)) ; number of melodic intervals between n and n+1 for thesis and arsis notes combined
    (setf (m-all-intervals counterpoint) (gil::add-int-var-array *sp* (total-m-len counterpoint) 0 12))
    (setf (m-all-intervals-brut counterpoint) (gil::add-int-var-array *sp* (total-m-len counterpoint) -12 12))
    (create-m-intervals-self (solution-array counterpoint) (m-all-intervals counterpoint) (m-all-intervals-brut counterpoint))

    ; creating motion array
    ; 0 = contrary, 1 = oblique, 2 = direct/parallel
    (print "Creating motion array...")
    (setf (third (motions counterpoint)) (gil::add-int-var-array *sp* *cf-last-index -1 2))
    (setf (third (motions-cost counterpoint)) (gil::add-int-var-array-dom *sp* *cf-last-index *motions-domain*))
    (setf (real-motions counterpoint) (gil::add-int-var-array *sp* *cf-last-index -1 2))
    (setf (real-motions-cost counterpoint) (gil::add-int-var-array-dom *sp* *cf-last-index *motions-domain*))
    (create-motions (third (m-intervals-brut counterpoint)) (first (m-intervals-brut *lowest)) (third (motions counterpoint)) (third (motions-cost counterpoint)) (is-not-lowest counterpoint))
    (create-real-motions (first (m-succ-intervals counterpoint)) (first (motions counterpoint)) (third (motions counterpoint)) (real-motions counterpoint) (first (motions-cost counterpoint)) (third (motions-cost counterpoint)) (real-motions-cost counterpoint))

    ; creating boolean diminution array
    (print "Creating diminution array...")
    ; Note: a diminution is the intermediate note that exists between two notes separated by a jump of a third
    ; i.e. E -> D (dim) -> C
    (setf (is-ta-dim-arr counterpoint) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-ta-dim-arr (first (m-succ-intervals counterpoint)) (first (m-intervals counterpoint)) (third (m-intervals counterpoint)) (is-ta-dim-arr counterpoint))

    ; creating boolean is cantus firmus bass array
    (print "Creating is cantus firmus bass array...")
    ; array of BoolVar representing if the cantus firmus is lower than the arsis counterpoint
    (setf (third (is-cf-lower-arr counterpoint)) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-cf-lower-arr (third (notes counterpoint)) (butlast *cf) (third (is-cf-lower-arr counterpoint)))

    ; creating boolean is cantus firmus neighboring the counterpoint array
    (print "Creating is cantus firmus neighboring array...")
    (setf (is-nbour-arr counterpoint) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-nbour-arr (h-intervals-abs counterpoint) (is-not-lowest counterpoint) (first (m-intervals-brut *lowest)) (is-nbour-arr counterpoint))

    ; creating boolean is counterpoint off key array
    (print "Creating is counterpoint off key array...")
    (setf (is-cp-off-key-arr counterpoint) (gil::add-bool-var-array *sp* (solution-len counterpoint) 0 1))
    (create-is-member-arr (solution-array counterpoint) (is-cp-off-key-arr counterpoint) (off-domain counterpoint))


    ;======================================== HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    (print "Harmonic consonances...")
    
    ; for all harmonic intervals between the cantus firmus and the arsis notes, the interval must be a consonance
    ; unless the arsis note is a diminution
    (print "No dissonance unless diminution for arsis notes...")
    (add-h-cons-arsis-cst *cf-len *cf-penult-index (third (h-intervals counterpoint)) (is-ta-dim-arr counterpoint))

    ; Fux does not follow this rule so deactivate ?
    ; no unison between the cantus firmus and the arsis counterpoint
    ; (print "No unison at all...")
    ; (add-no-unison-at-all-cst (third (notes counterpoint)) (butlast (cf counterpoint)))

    (if (eq *N-PARTS 2) (progn
        ; if penultimate measure, a major sixth or a minor third must be used
        ; depending if the cantus firmus is at the bass or on the top part
        (print "Penultimate measure...")
        (add-penult-cons-cst (lastone (third (is-cf-lower-arr counterpoint))) (lastone (third (h-intervals counterpoint))))
    ))

    (if (eq *N-PARTS 3) (progn
        (print "Penultimate measure...")
        (gil::g-member *sp* PENULT_CONS_3P_VAR (lastone (third (h-intervals counterpoint))))
    ))

    
    ;======================================== MELODIC CONSTRAINTS =============================
    (print "Melodic constraints...")

    ; no more than minor sixth melodic interval between thesis and arsis notes UNLESS:
    ;   - the interval between the cantus firmus and the thesis note <= major third
    ;   - the cantus firmus is getting closer to the thesis note
    (print "No more than minor sixth melodic interval between thesis and arsis notes unless...")
    (add-m-inter-arsis-cst (first (m-succ-intervals counterpoint)) (is-nbour-arr counterpoint))

    ; Fux does not follow this rule, deactivate ?
    ; (print "No more than minor sixth melodic interval between arsis and thesis notes...")
    ; (add-no-m-jump-cst (third (m-intervals counterpoint)))

    ; no *chromatic motion between three consecutive notes
    (print "No chromatic motion...")
    (add-no-chromatic-m-cst (m-all-intervals-brut counterpoint) (m2-intervals-brut counterpoint))

    ; no unison between two consecutive notes
    (print "No unison between two consecutive notes...")
    (case species
        (2 (add-no-unison-at-all-cst (solution-array counterpoint) (rest (solution-array counterpoint))))
        ; @completely new or reworked
        ; ========= 2 counterpoints specific
        (3v-2st (progn
            ; when there is more than one counterpoint, unison can occur between the fourth-to-last and third-to-last note
            (if (member 3 *species-list) (progn
                ; when used in combination with a third species counterpoint, unison can also occurr between the third-to-last and the second-to-last
                (add-no-unison-at-all-cst (butlast (solution-array counterpoint) 3) (rest (butlast (solution-array counterpoint) 3))) ; no unison until fourth-to-last
                (add-no-unison-at-all-cst (last (solution-array counterpoint) 2) (rest (last (solution-array counterpoint) 2))) ; no unison in the two last ones
                (gil::g-rel *sp* (first (last (solution-array counterpoint) 4)) gil::IRT_NQ (first (last (solution-array counterpoint) 2))) ; but the three of them cannot be a unison
            ) (progn 
                ; when used in combination with another counterpoint (that is not of third species)
                (add-no-unison-at-all-cst (butlast (solution-array counterpoint) 3) (rest (butlast (solution-array counterpoint) 3))) ; no unison until fourth-to-last
                (add-no-unison-at-all-cst (last (solution-array counterpoint) 3) (rest (last (solution-array counterpoint) 3))) ; no unison in the three last ones
            ))
        ))
        ; ========= 
    )


    ;======================================== MOTION CONSTRAINTS ============================
    (print "Motion constraints...")
    ; no direct motion to reach a perfect consonance
    (print "No direct motion to reach a perfect consonance...")
    (if (eq species 2) (add-no-direct-move-to-p-cons-cst (real-motions counterpoint) (is-p-cons-arr counterpoint) (is-not-lowest counterpoint)))
    ; no battuta kind of motion
    ; i.e. contrary motion to an *octave, lower voice up, higher voice down, counterpoint melodic interval < -4
    (print "No battuta kind of motion...")
    (add-no-battuta-cst (third (motions counterpoint)) (first (h-intervals counterpoint)) (third (m-intervals-brut counterpoint)) (third (is-cf-lower-arr counterpoint)))



    ;======================================== COST FACTORS ====================================
    ; 1, 2) imperfect consonances are preferred to perfect consonances
    (print "Imperfect consonances are preferred to perfect consonances...")
    (add-p-cons-cost-cst (h-intervals counterpoint) (is-not-lowest counterpoint))
    
    ; 3, 4) add off-key cost, m-degrees cost
    (set-general-costs-cst counterpoint (solution-len counterpoint))
    
    ; 5) contrary motion is preferred
    (add-cost-to-factors (real-motions-cost counterpoint) 'motions-cost)
    

    ; 6) the penultimate thesis note is not a fifth
    (print "Penultimate thesis note is not a fifth...")
    ; *penult-thesis-cost = *cf-len (big cost) if penultimate *h-interval /= 7
    (setf (penult-thesis-cost counterpoint) (gil::add-int-var-dom *sp* (getparam-dom 'penult-sixth-cost)))
    (add-single-cost-cst (penult (first (h-intervals counterpoint))) gil::IRT_NQ 7 (penult-thesis-cost counterpoint) *penult-sixth-cost*)
    (add-cost-to-factors (penult-thesis-cost counterpoint) 'penult-thesis-cost nil)
     
    ;======================================== COST FUNCTION ===================================
    (print "Cost function...")

    (case species
        (2 (append (fux-search-engine (solution-array counterpoint) '(2)) (list (list 2))))
        (3v-2sp nil) ; if 3v don't return a search engine, just apply the constraints
    )
)