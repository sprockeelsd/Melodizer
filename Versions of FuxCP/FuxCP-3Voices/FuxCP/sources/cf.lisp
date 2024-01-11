(in-package :fuxcp)

; Author: Anton Lamotte
; Date: January 2024
; This file contains the function that adds all the necessary constraints to link the cantus firmus to the lowest stratum.

;;==========================#
;; CANTUS FIRMUS            #
;;==========================#
(defun fux-cp-cf (cantus-firmus &optional (species 0))
    (print "########## CANTUS FIRMUS RULES ##########")
    "Create the CSP for the cantus-firmus."

    ;============================================ CREATING GIL ARRAYS =============================
    ;; initialize the variables
    (print "Initializing variables...")
        
    ; creating harmonic intervals array
    (print "Creating harmonic intervals array...")

    ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the cantus-firmus
    (setf (first (h-intervals cantus-firmus)) (gil::add-int-var-array *sp* *cf-len 0 11))
    (create-h-intervals (first (notes cantus-firmus)) (first (notes *lowest)) (first (h-intervals cantus-firmus)))

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the absolute intervals between two notes in a row of the cantus-firmus
    (setf (first (m-intervals cantus-firmus)) (gil::add-int-var-array *sp* *cf-last-index 0 12))
    ;(setf (first (m-intervals-brut cantus-firmus)) (gil::add-int-var-array *sp* *cf-last-index -12 12))
    (create-m-intervals-self (first (notes cantus-firmus)) (first (m-intervals cantus-firmus)) (first (m-intervals-brut cantus-firmus)))
    
    ; creating perfect consonances boolean array
    (print "Creating perfect consonances boolean array...")
    ; array of BoolVar representing if the interval between the cantus firmus and the cantus-firmus is a perfect consonance
    (setf (is-p-cons-arr cantus-firmus) (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first (h-intervals cantus-firmus)) (is-p-cons-arr cantus-firmus))


    ; creating motion array
    (print "Creating motion array...")
    (setf (first (motions cantus-firmus)) (gil::add-int-var-array *sp* *cf-last-index -1 2)) ; 0 = contrary, 1 = oblique, 2 = direct/parallel
    (setf (first (motions-cost cantus-firmus)) (gil::add-int-var-array-dom *sp* *cf-last-index *motions-domain*))
    (create-motions (first (m-intervals-brut cantus-firmus)) (first (m-intervals-brut *lowest)) (first (motions cantus-firmus)) (first (motions-cost cantus-firmus)) (is-not-lowest cantus-firmus))
    ;============================================ HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    ; for all intervals between the cantus firmus and the cantus-firmus, the interval must be a consonance
    (print "Harmonic consonances...")
    (add-h-cons-cst *cf-len *cf-penult-index (first (h-intervals cantus-firmus)))

    (if (= *N-PARTS 2) (progn 
        ; must start with a perfect consonance
        (print "Perfect consonance at the beginning...")
        (add-p-cons-start-cst (first (h-intervals cantus-firmus)))

        ; must end with a perfect consonance
        (print "Perfect consonance at the end...")
        (add-p-cons-end-cst (first (h-intervals cantus-firmus)))
        
        (print "Penultimate measure...")    
        (add-penult-cons-1sp-and-cf-cst (penult (is-not-lowest cantus-firmus)) (penult (first (h-intervals cantus-firmus))) 0)
        )
        ; else (if 3 parts)
        (progn
            (print "Penultimate measure...")    
            (gil::g-member *sp* PENULT_CONS_3P_VAR (penult (first (h-intervals cantus-firmus))))
        )
    )

    ;==================================== MELODIC CONSTRAINTS ===========================
    ; There are no melodic constraints for the cantus firmus, as its notes are already fixed

    ;==================================== MOTION CONSTRAINTS ============================
    (print "Motion constraints...")
    (if (= *N-PARTS 2)
        (add-no-direct-move-to-p-cons-cst (first (motions cantus-firmus)) (is-p-cons-arr cantus-firmus) (is-not-lowest cantus-firmus))
    )

    ;============================================ COST FACTORS ====================================
    (print "Cost function...")

    ; 1, 2) imperfect consonances are preferred to perfect consonances
    (print "Imperfect consonances are preferred to perfect consonances...")
    (add-p-cons-cost-cst (h-intervals cantus-firmus) (is-not-lowest cantus-firmus))
    ; 3) motion costs
    (print "add motion costs")
    (add-cost-to-factors (first (motions-cost cantus-firmus)) 'motions-cost)    
)