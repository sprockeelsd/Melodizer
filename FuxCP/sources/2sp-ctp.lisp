(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains the function that adds all the necessary constraints to the second species.

;;==========================#
;; SECOND SPECIES           #
;;==========================#
;; Note: fux-cp-2nd execute the first species algorithm without some constraints.
;; In this function, all the variable names without the arsis-suffix refers to thesis notes AKA the first species notes.
;; All the variable names with the arsis-suffix refers to arsis notes AKA notes on the upbeat.
(defun fux-cp-2nd (&optional (species 2))
    "Create the CSP for the 2nd species of Fux's counterpoint, with the cantus firmus as input"

    ;; ADD FIRST SPECIES CONSTRAINTS
    (fux-cp-1st 2)

    (print "########## SECOND SPECIES ##########")

    ;======================================== CREATION OF GIL ARRAYS ==========================
    (print "Initializing variables...")
    ; add the arsis counterpoint array (of [*cf-len - 1] length) to the space with the domain *cp-domain
    (setf (third *cp) (gil::add-int-var-array-dom *sp* *cf-last-index *extended-cp-domain))
    ; add to the penultimate note more possibilities
    (if (is-borrow-allowed)
        (setf (nth *cf-penult-index (third *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain))
    )
    
    ; merging cp and cp-arsis into one array
    (setq *total-cp-len (+ *cf-len *cf-last-index))
    (setq *total-cp (gil::add-int-var-array *sp* *total-cp-len 0 127)) ; array of IntVar representing thesis and arsis notes combined
    (merge-cp (list (first *cp) (third *cp)) *total-cp) ; merge the two counterpoint arrays into one
    
    ; creating harmonic intervals array
    (print "Creating harmonic intervals array...")
    ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the counterpoint (arsis notes)
    (setf (third *h-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 11))
    (create-h-intervals (third *cp) (butlast *cf) (third *h-intervals))
    ; array of IntVar representing the absolute intervals (not % 12) and brut (just p - q)
    ; between the cantus firmus and the counterpoint (thesis notes)
    (setq *h-intervals-abs (gil::add-int-var-array *sp* *cf-len 0 127))
    (setq *h-intervals-brut (gil::add-int-var-array *sp* *cf-len -127 127))
    (create-intervals *cf (first *cp) *h-intervals-abs *h-intervals-brut)
    

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the melodic intervals between arsis note and next thesis note of the counterpoint
    (setf (third *m-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 12))
    (setf (third *m-intervals-brut) (gil::add-int-var-array *sp* *cf-last-index -12 12)) ; same without absolute reduction
    (create-m-intervals-next-meas (third *cp) (first *cp) (third *m-intervals) (third *m-intervals-brut))
    ; array of IntVar representing the melodic intervals between a thesis and an arsis note of the same measure the counterpoint
    (setf (first *m-succ-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 12))
    (setf (first *m-succ-intervals-brut) (gil::add-int-var-array *sp* *cf-last-index -12 12))
    (create-m-intervals-in-meas (first *cp) (third *cp) (first *m-succ-intervals) (first *m-succ-intervals-brut))

    
    ; creating melodic intervals array between the note n and n+2 for the whole counterpoint
    (setq *m2-len (- (* *cf-last-index 2) 1)) ; number of melodic intervals between n and n+2 for thesis and arsis notes combined
    (setq *m2-intervals (gil::add-int-var-array *sp* *m2-len 0 12))
    (setq *m2-intervals-brut (gil::add-int-var-array *sp* *m2-len -12 12))
    (create-m2-intervals *total-cp *m2-intervals *m2-intervals-brut)
    
    ; creating melodic intervals array between the note n and n+1 for the whole counterpoint
    (setq *total-m-len (* *cf-last-index 2)) ; number of melodic intervals between n and n+1 for thesis and arsis notes combined
    (setq *m-all-intervals (gil::add-int-var-array *sp* *total-m-len 0 12))
    (setq *m-all-intervals-brut (gil::add-int-var-array *sp* *total-m-len -12 12))
    (create-m-intervals-self *total-cp *m-all-intervals *m-all-intervals-brut)

    ; creating motion array
    ; 0 = contrary, 1 = oblique, 2 = direct/parallel
    (print "Creating motion array...")
    (setf (third *motions) (gil::add-int-var-array *sp* *cf-last-index 0 2))
    (setf (third *motions-cost) (gil::add-int-var-array-dom *sp* *cf-last-index *motions-domain*))
    (setq *real-motions (gil::add-int-var-array *sp* *cf-last-index 0 2))
    (setf *real-motions-cost (gil::add-int-var-array-dom *sp* *cf-last-index *motions-domain*))
    (create-motions (third *m-intervals-brut) *cf-brut-m-intervals (third *motions) (third *motions-cost))
    (create-real-motions (first *m-succ-intervals) (first *motions) (third *motions) *real-motions (first *motions-cost) (third *motions-cost) *real-motions-cost)

    ; creating boolean diminution array
    (print "Creating diminution array...")
    ; Note: a diminution is the intermediate note that exists between two notes separated by a jump of a third
    ; i.e. E -> D (dim) -> C
    (setq *is-ta-dim-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (print "DEBUG")
    (print (first *m-succ-intervals))
    (print (first *m-intervals))
    (print (third *m-intervals))
    (create-is-ta-dim-arr (first *m-succ-intervals) (first *m-intervals) (third *m-intervals) *is-ta-dim-arr)


    ; creating boolean is cantus firmus bass array
    (print "Creating is cantus firmus bass array...")
    ; array of BoolVar representing if the cantus firmus is lower than the arsis counterpoint
    (setf (third *is-cf-bass-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-cf-bass-arr (third *cp) (butlast *cf) (third *is-cf-bass-arr))

    ; creating boolean is cantus firmus neighboring the counterpoint array
    (print "Creating is cantus firmus neighboring array...")
    (setq *is-nbour-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-nbour-arr *h-intervals-abs (first *is-cf-bass-arr) *cf-brut-m-intervals *is-nbour-arr)

    ; creating boolean is counterpoint off key array
    (print "Creating is counterpoint off key array...")
    (setq *is-cp-off-key-arr (gil::add-bool-var-array *sp* *total-cp-len 0 1))
    (create-is-member-arr *total-cp *is-cp-off-key-arr *off-domain)


    ;======================================== HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    (print "Harmonic consonances...")
    
    ; for all harmonic intervals between the cantus firmus and the arsis notes, the interval must be a consonance
    ; unless the arsis note is a diminution
    (print "No dissonance unless diminution for arsis notes...")
    (add-h-cons-arsis-cst *cf-len *cf-penult-index (third *h-intervals) *is-ta-dim-arr)

    ; Fux does not follow this rule so deactivate ?
    ; no unisson between the cantus firmus and the arsis counterpoint
    ; (print "No unisson at all...")
    ; (add-no-unisson-at-all-cst (third *cp) (butlast *cf))

    ; if penultimate measure, a major sixth or a minor third must be used
    ; depending if the cantus firmus is at the bass or on the top part
    (print "Penultimate measure...")
    ; (gil::g-rel *sp* (fourth (first *h-intervals)) gil::IRT_NQ 7) ; TODO: fix this
    (add-penult-cons-cst (lastone (third *is-cf-bass-arr)) (lastone (third *h-intervals)))


    ;======================================== MELODIC CONSTRAINTS =============================
    (print "Melodic constraints...")

    ; no more than minor sixth melodic interval between thesis and arsis notes UNLESS:
    ;   - the interval between the cantus firmus and the thesis note <= major third
    ;   - the cantus firmus is getting closer to the thesis note
    (print "No more than minor sixth melodic interval between thesis and arsis notes unless...")
    (add-m-inter-arsis-cst (first *m-succ-intervals) *is-nbour-arr)

    ; Fux does not follow this rule, deactivate ?
    ; (print "No more than minor sixth melodic interval between arsis and thesis notes...")
    ; (add-no-m-jump-cst (third *m-intervals))

    ; no *chromatic motion between three consecutive notes
    (print "No chromatic motion...")
    (add-no-chromatic-m-cst *m-all-intervals-brut *m2-intervals-brut)

    ; no unisson between two consecutive notes
    (print "No unisson between two consecutive notes...")
    (add-no-unisson-at-all-cst *total-cp (rest *total-cp))


    ;======================================== MOTION CONSTRAINTS ============================
    (print "Motion constraints...")

    ; no direct motion to reach a perfect consonance
    (print "No direct motion to reach a perfect consonance...")
    (add-no-direct-move-to-p-cons-cst *real-motions *is-p-cons-arr)

    ; no battuta kind of motion
    ; i.e. contrary motion to an *octave, lower voice up, higher voice down, counterpoint melodic interval < -4
    (print "No battuta kind of motion...")
    (add-no-battuta-cst (third *motions) (first *h-intervals) (third *m-intervals-brut) (third *is-cf-bass-arr))



    ;======================================== COST FACTORS ====================================
    (set-cost-factors)
    ; 1, 2) imperfect consonances are preferred to perfect consonances
    (print "Imperfect consonances are preferred to perfect consonances...")
    (add-p-cons-cost-cst)
    
    ; 3, 4) add off-key cost, m-degrees cost
    (set-general-costs-cst)
    
    ; 5) contrary motion is preferred
    (add-cost-to-factors *real-motions-cost)
    
    ; 6) the penultimate thesis note is not a fifth
    (print "Penultimate thesis note is not a fifth...")
    ; *penult-thesis-cost = *cf-len (big cost) if penultimate *h-interval /= 7
    (setq *penult-thesis-cost (gil::add-int-var-dom *sp* (getparam-dom 'penult-sixth-cost)))
    (add-single-cost-cst (penult (first *h-intervals)) gil::IRT_NQ 7 *penult-thesis-cost *penult-sixth-cost*)
    (setf (nth *n-cost-added *cost-factors) *penult-thesis-cost)
    (incf *n-cost-added)


    ;======================================== COST FUNCTION ===================================
    (print "Cost function...")

    ; RETURN
    (if (eq species 2)
        ; then create the search engine
        (append (fux-search-engine *total-cp 2) (list species))
        ; else
        nil
    )
)