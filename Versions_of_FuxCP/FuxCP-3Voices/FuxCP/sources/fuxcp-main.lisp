(in-package :fuxcp)

; Author: Thibault Wafflard and Anton Lamotte
; Date: June 3, 2023 and January 2024
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


; define all the constants that are going to be used
(defun define-global-constants ()
    ; Number of costs added
    (defparameter *n-cost-added 0)

    ;; CONSTANTS
    ; Motion types
    (defparameter DIRECT 2)
    (defparameter OBLIQUE 1)
    (defparameter CONTRARY 0)

    ; Integer constants (to represent costs or intervals)
    ; 0 in IntVar
    (defparameter ZERO (gil::add-int-var-dom *sp* (list 0)))
    ; 1 in IntVar
    (defparameter ONE (gil::add-int-var-dom *sp* (list 1)))
    ; 3 in IntVar (minor third)
    (defparameter THREE (gil::add-int-var-dom *sp* (list 3)))
    ; 9 in IntVar (major sixth)
    (defparameter NINE (gil::add-int-var-dom *sp* (list 9)))

    (defparameter CANTUS_FIRMUS (gil::add-int-var-array *sp* *cf-len 0 120))
    (dotimes (i *cf-len)
        (gil::g-rel *sp* (nth i CANTUS_FIRMUS) gil::IRT_EQ (nth i *cf))
    )

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
    ; harmonic triad intervals
    (defparameter H_TRIAD (list 0 3 4 7))
    ; major harmonic triad intervals
    (defparameter MAJ_H_TRIAD (list 0 4 7))
    ; dissonances intervals
    (defparameter DIS (list 1 2 5 6 10 11))
    ; penultimate intervals, i.e. minor third and major sixth
    (defparameter PENULT_CONS (list 0 3 9))
    ; penultimate thesis intervals, i.e. perfect fifth and sixth
    (defparameter PENULT_THESIS (list 0 7 8 9))
    ; penultimate 1st quarter note intervals, i.e. minor third, major sixth and octave/unisson
    (defparameter PENULT_1Q (list 0 3 8))
    ; penultimate syncope intervals, i.e. seconds and sevenths
    (defparameter PENULT_SYNCOPE (list 0 1 2 10 11))
    ; penultimate intervals for three parts, i.e. minor third and major sixth + the perfect consonances
    (defparameter PENULT_CONS_3P (list 0 3 7 9))

    ; P_CONS in IntVar
    (defparameter P_CONS_VAR (gil::add-int-var-const-array *sp* P_CONS))
    ; IMP_CONS in IntVar
    (defparameter IMP_CONS_VAR (gil::add-int-var-const-array *sp* IMP_CONS))
    ; ALL_CONS in IntVar
    (defparameter ALL_CONS_VAR (gil::add-int-var-const-array *sp* ALL_CONS))
    ; H_TRIAD in IntVar
    (defparameter H_TRIAD_VAR (gil::add-int-var-const-array *sp* H_TRIAD))
    ; MAJ_H_TRIAD in IntVar
    (defparameter MAJ_H_TRIAD_VAR (gil::add-int-var-const-array *sp* MAJ_H_TRIAD))
    ; DIS in IntVar
    (defparameter DIS_VAR (gil::add-int-var-const-array *sp* DIS))
    ; PENULT_CONS in IntVar
    (defparameter PENULT_CONS_VAR (gil::add-int-var-const-array *sp* PENULT_CONS))
    ; PENULT_THESIS in IntVar
    (defparameter PENULT_THESIS_VAR (gil::add-int-var-const-array *sp* PENULT_THESIS))
    ; PENULT_1Q in IntVar
    (defparameter PENULT_1Q_VAR (gil::add-int-var-const-array *sp* PENULT_1Q))
    ; PENULT_SYNCOPE in IntVar
    (defparameter PENULT_SYNCOPE_VAR (gil::add-int-var-const-array *sp* PENULT_SYNCOPE))
    ; PENULT_CONS_3P in IntVar
    (defparameter PENULT_CONS_3P_VAR (gil::add-int-var-const-array *sp* PENULT_CONS_3P))
    
    ; *cf-brut-intervals is the list of brut melodic intervals in the cantus firmus
    (setq *cf-brut-m-intervals (gil::add-int-var-array *sp* *cf-last-index -127 127))
    ; array representing the brut melodic intervals of the cantus firmus
    (create-cf-brut-m-intervals *cf *cf-brut-m-intervals)

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
    ; 3v general costs
    (defparameter *succ-p-cons-cost* (gil::add-int-var-dom *sp* (getparam-val 'succ-p-cons-cost)))
    (defparameter *variety-cost* (gil::add-int-var-dom *sp* (getparam-val 'variety-cost)))
    (defparameter *h-triad-cost* (gil::add-int-var-dom *sp* (getparam-val 'h-triad-cost)))
    (defparameter *direct-move-to-p-cons-cost* (gil::add-int-var-dom *sp* (getparam-val 'direct-move-to-p-cons-cost)))
    ;; Species specific costs
    (defparameter *penult-sixth-cost* (gil::add-int-var-dom *sp* (getparam-val 'penult-sixth-cost)))
    (defparameter *non-cambiata-cost* (gil::add-int-var-dom *sp* (getparam-val 'non-cambiata-cost)))
    (defparameter *m2-eq-zero-cost* (gil::add-int-var-dom *sp* (getparam-val 'm2-eq-zero-cost)))
    (defparameter *no-syncopation-cost* (gil::add-int-var-dom *sp* (getparam-val 'no-syncopation-cost)))
    ; 3v species specific costs
    (defparameter *h-triad-3rd-species-cost* (gil::add-int-var-dom *sp* (getparam-val 'h-triad-3rd-species-cost)))

    ;; Params domains
    (defparameter *motions-domain* ; equal to all possible values of the motions cost, plus zero
        (remove-duplicates (append
            (mapcar (lambda (x) (getparam x))
                (list 'con-motion-cost 'obl-motion-cost 'dir-motion-cost)
            )
            (list 0)
        ))
    )

    ; To make the code more readable
    (defparameter 3v-1sp 6)
    (defparameter 3v-2sp 7)
    (defparameter 3v-3sp 8)
    (defparameter 3v-4sp 9)
    (defparameter 3v-5sp 10)
)

(defclass stratum-class () (
    ; represents a stratum, it is a simplified part-class
    (solution-array :accessor solution-array :initarg :solution-array :initform nil)
    (solution-len :accessor solution-len :initarg :solution-len :initform nil)

    (notes :accessor notes :initarg :notes :initform 
        (list 
            (gil::add-int-var-array *sp* *cf-len 0 120)
            (gil::add-int-var-array *sp* *cf-len 0 120)
            (gil::add-int-var-array *sp* *cf-len 0 120)
            (gil::add-int-var-array *sp* *cf-len 0 120)
        )
    ) ; represents the notes of the counterpoint
    (h-intervals :accessor h-intervals :initarg :h-intervals :initform 
        (list
            (gil::add-int-var-array *sp* *cf-len 0 11)
            (gil::add-int-var-array *sp* *cf-len 0 11)
            (gil::add-int-var-array *sp* *cf-len 0 11)
            (gil::add-int-var-array *sp* *cf-len 0 11)
        )
    )
    (is-p-cons-arr :accessor is-p-cons-arr :initarg :is-p-cons-arr :initform nil)
    (m-intervals :accessor m-intervals :initarg :m-intervals :initform (list (gil::add-int-var-array *sp* *cf-last-index 0 12) nil nil nil))
    (m-intervals-brut :accessor m-intervals-brut :initarg :m-intervals-brut :initform (list (gil::add-int-var-array *sp* *cf-last-index -12 12) nil nil nil))
    ;(m-intervals-brut :accessor m-intervals-brut :initarg :m-intervals-brut :initform (list nil nil nil nil))
    ;(m-intervals :accessor m-intervals :initarg :m-intervals :initform (list nil nil nil nil))
    (motions :accessor motions :initarg :motions :initform (list nil nil nil nil))
    (motions-cost :accessor motions-cost :initarg :motions-cost :initform (list nil nil nil nil))
    (m2-intervals-brut :accessor m2-intervals-brut :initarg :m2-intervals-brut :initform nil)
    (m2-intervals :accessor m2-intervals :initarg :m2-intervals :initform nil)
    (cf-brut-m-intervals :accessor cf-brut-m-intervals :initarg :cf-brut-m-intervals :initform nil)
    (is-cp-off-key-arr :accessor is-cp-off-key-arr :initarg :is-cp-off-key-arr :initform nil)
    (p-cons-cost :accessor p-cons-cost :initarg :p-cons-cost :initform nil)
    (fifth-cost :accessor fifth-cost :initarg :fifth-cost :initform nil)
    (octave-cost :accessor octave-cost :initarg :octave-cost :initform nil)
    (m-degrees-cost :accessor m-degrees-cost :initarg :m-degrees-cost :initform nil)
    (m-degrees-type :accessor m-degrees-type :initarg :m-degrees-type :initform nil)
    (off-key-cost :accessor off-key-cost :initarg :off-key-cost :initform nil)
    (m-all-intervals :accessor m-all-intervals :initarg :m-all-intervals :initform nil)

    (h-intervals-abs :accessor h-intervals-abs :initarg :h-intervals-abs :initform (list nil nil nil nil))
    (h-intervals-brut :accessor h-intervals-brut :initarg :h-intervals-brut :initform (list nil nil nil nil))
))

(defclass part-class () (
    ; represents a part, i.e. a counterpoint or a cantus firmus
    ; species
    (species :accessor species :initarg :species :initform nil) ; 0 for cf, 1 for 1st, 2 for 2nd, 3 for 3rd, 4 for 4th, 5 for 5th

    ; solution-array
    (solution-array :accessor solution-array :initarg :solution-array :initform nil) ; contains the whole array of notes for this part, all merged together in the good order
    (solution-len :accessor solution-len :initarg :solution-len :initform nil) ; number of note in this part

    ; voice variables
    (cp-range :accessor cp-range :initarg :cp-range :initform nil)
    (cp-domain :accessor cp-domain :initarg :cp-domain :initform nil)
    (chromatic-cp-domain :accessor chromatic-cp-domain :initarg :chromatic-cp-domain :initform nil)
    (extended-cp-domain :accessor extended-cp-domain :initarg :extended-cp-domain :initform nil)
    (off-domain :accessor off-domain :initarg :off-domain :initform nil)
    (voice-type :accessor voice-type :initarg :voice-type :initform nil)

    ; 1st species variables
    (notes :accessor notes :initarg :notes :initform (list nil nil nil nil)) ; represents the notes of the counterpoint; (first notes) are all the notes of the first beat, (second notes) of the second etc
    (h-intervals :accessor h-intervals :initarg :h-intervals :initform (list nil nil nil nil)) ; h-intervals to the lowest stratum
    (m-intervals-brut :accessor m-intervals-brut :initarg :m-intervals-brut :initform (list 
        (gil::add-int-var-array *sp* *cf-last-index -12 12) 
        (gil::add-int-var-array *sp* *cf-last-index -12 12)
        (gil::add-int-var-array *sp* *cf-last-index -12 12) 
        (gil::add-int-var-array *sp* *cf-last-index -12 12))) ; this variable is set before the others because we need it earlier
    (m-intervals :accessor m-intervals :initarg :m-intervals :initform (list nil nil nil nil)) ; melodic intervals 
    (motions :accessor motions :initarg :motions :initform (list nil nil nil nil))
    (motions-cost :accessor motions-cost :initarg :motions-cost :initform (list nil nil nil nil))
    (is-cf-lower-arr :accessor is-cf-lower-arr :initarg :is-cf-lower-arr :initform (list nil nil nil nil)) ; true if the cf is lower
    (m2-intervals-brut :accessor m2-intervals-brut :initarg :m2-intervals-brut :initform nil)
    (m2-intervals :accessor m2-intervals :initarg :m2-intervals :initform nil)
    (cf-brut-m-intervals :accessor cf-brut-m-intervals :initarg :cf-brut-m-intervals :initform nil)
    (is-p-cons-arr :accessor is-p-cons-arr :initarg :is-p-cons-arr :initform nil)
    (is-cp-off-key-arr :accessor is-cp-off-key-arr :initarg :is-cp-off-key-arr :initform nil)
    (p-cons-cost :accessor p-cons-cost :initarg :p-cons-cost :initform nil)
    (fifth-cost :accessor fifth-cost :initarg :fifth-cost :initform nil)
    (octave-cost :accessor octave-cost :initarg :octave-cost :initform nil)
    (m-degrees-cost :accessor m-degrees-cost :initarg :m-degrees-cost :initform nil)
    (m-degrees-type :accessor m-degrees-type :initarg :m-degrees-type :initform nil)
    (off-key-cost :accessor off-key-cost :initarg :off-key-cost :initform nil)
    (m-all-intervals :accessor m-all-intervals :initarg :m-all-intervals :initform nil)

    ; 2nd species variables
    (h-intervals-abs :accessor h-intervals-abs :initarg :h-intervals-abs :initform (list nil nil nil nil))
    (h-intervals-brut :accessor h-intervals-brut :initarg :h-intervals-brut :initform (list nil nil nil nil))
    (m-succ-intervals :accessor m-succ-intervals :initarg :m-succ-intervals :initform (list nil nil nil nil)) 
    (m-succ-intervals-brut :accessor m-succ-intervals-brut :initarg :m-succ-intervals-brut :initform (list nil nil nil nil)) 
    (m2-len :accessor m2-len :initarg :m2-len :initform nil)
    (total-m-len :accessor total-m-len :initarg :total-m-len :initform nil)
    (m-all-intervals-brut :accessor m-all-intervals-brut :initarg :m-all-intervals-brut :initform nil)
    (real-motions :accessor real-motions :initarg :real-motions :initform nil)
    (real-motions-cost :accessor real-motions-cost :initarg :real-motions-cost :initform nil)
    (is-ta-dim-arr :accessor is-ta-dim-arr :initarg :is-ta-dim-arr :initform nil)
    (is-nbour-arr :accessor is-nbour-arr :initarg :is-nbour-arr :initform nil)
    (penult-thesis-cost :accessor penult-thesis-cost :initarg :penult-thesis-cost :initform nil) 

    ; 3rd species variables
    (is-5qn-linked-arr :accessor is-5qn-linked-arr :initarg :is-5qn-linked-arr :initform nil)
    (is-not-cambiata-arr :accessor is-not-cambiata-arr :initarg :is-not-cambiata-arr :initform nil)
    (not-cambiata-cost :accessor not-cambiata-cost :initarg :not-cambiata-cost :initform nil)
    (m2-eq-zero-cost :accessor m2-eq-zero-cost :initarg :m2-eq-zero-cost :initform nil)
    (is-cons-arr :accessor is-cons-arr :initarg :is-cons-arr :initform (list nil nil nil nil))
    (cons-cost :accessor cons-cost :initarg :cons-cost :initform (list nil nil nil nil))

    ; 4th species variables
    (is-no-syncope-arr :accessor is-no-syncope-arr :initarg :is-no-syncope-arr :initform nil)
    (no-syncope-cost :accessor no-syncope-cost :initarg :no-syncope-cost :initform nil)

    ; 5th species variables
    (species-arr :accessor species-arr :initarg :species-arr :initform nil) ; 0: no constraint, 1: first species, 2: second species, 3: third species, 4: fourth species
    (sp-arr :accessor sp-arr :initarg :sp-arr :initform nil) ; represents *species-arr by position in the measure
    (is-nth-species-arr :accessor is-nth-species-arr :initarg :is-nth-species-arr :initform (list nil nil nil nil nil)) ; if *species-arr is n, then *is-nth-species-arr is true
    (is-3rd-species-arr :accessor is-3rd-species-arr :initarg :is-3rd-species-arr :initform (list nil nil nil nil)) ; if *species-arr is 3, then *is-3rd-species-arr is true
    (is-4th-species-arr :accessor is-4th-species-arr :initarg :is-4th-species-arr :initform (list nil nil nil nil)) ; if *species-arr is 4, then *is-4th-species-arr is true
    (is-2nd-or-3rd-species-arr :accessor is-2nd-or-3rd-species-arr :initarg :is-2nd-or-3rd-species-arr :initform nil) ; if *species-arr is 2 or 3, then *is-2nd-or-3rd-species-arr is true
    (m-ta-intervals :accessor m-ta-intervals :initarg :m-ta-intervals :initform nil) ; represents the m-intervals between the thesis note and the arsis note of the same measure
    (m-ta-intervals-brut :accessor m-ta-intervals-brut :initarg :m-ta-intervals-brut :initform nil) ; same but without the absolute reduction
    (is-mostly-3rd-arr :accessor is-mostly-3rd-arr :initarg :is-mostly-3rd-arr :initform nil) ; true if second, third and fourth notes are from the 3rd species
    (is-constrained-arr :accessor is-constrained-arr :initarg :is-constrained-arr :initform nil) ; represents !(*is-0th-species-arr) i.e. there are species constraints
    (is-cst-arr :accessor is-cst-arr :initarg :is-cst-arr :initform (list nil nil nil nil)) ; represents *is-constrained-arr for all beats of the measure

    ; 3v variables
    (variety-cost :accessor variety-cost :initarg :variety-cost :initform nil)
    (is-not-lowest :accessor is-not-lowest :initarg :is-not-lowest :initform nil) ; represents if the current part is not the lowest stratum
    (h-intervals-to-cf :accessor h-intervals-to-cf :initarg :h-intervals-to-cf :initform (list nil nil nil nil))
))

(defun init-cantus-firmus ()
    ; init a cantus-firmus class "object"
    (let (
            (cantus-firmus-notes (gil::add-int-var-array *sp* *cf-len 0 120))
        )
        (dotimes (i *cf-len) (gil::g-rel *sp* (nth i cantus-firmus-notes) gil::IRT_EQ (nth i *cf)))
        (make-instance 'part-class
                    :species 0 ; value 0 = cantus firmus
                    :notes (list cantus-firmus-notes nil nil nil) ; no notes in the second, third and fourth beat
        )
    )
)

(defun init-counterpoint (voice-type species)
    ; initialise a counterpoint 'object'
    (let (
        ; Lower bound and upper bound related to the cantus firmus pitch
        (range-upper-bound (+ 12 (* 6 voice-type)))
        (range-lower-bound (+ -6 (* 6 voice-type)))
        )
        (let (
            ; set the pitch range of the counterpoint
            (cp-range (range (+ *tone-pitch-cf range-upper-bound) :min (+ *tone-pitch-cf range-lower-bound))) ; arbitrary range
            )
            (let (
                ; set counterpoint pitch domain
                (cp-domain (intersection cp-range *scale))
                ; penultimate (first *cp) note domain
                (chromatic-cp-domain (intersection cp-range *chromatic-scale))
                ; set counterpoint extended pitch domain
                (extended-cp-domain (intersection cp-range (union *scale *borrowed-scale)))
                ; set the domain of the only barrowed notes
                (off-domain (intersection cp-range *off-scale))
                )
                ; create the instance, by passing it the arguments
                (setf counterpoint (make-instance 'part-class 
                                                :cp-range cp-range
                                                :cp-domain cp-domain
                                                :chromatic-cp-domain chromatic-cp-domain
                                                :extended-cp-domain extended-cp-domain
                                                :off-domain off-domain
                                                :voice-type voice-type
                                                :species species
                ))
                (case species
                    ((1 2 3) (progn
                        ; set the first beats of all measures of the counterpoints to be in the extended-cp-domain
                        (setf (first (notes counterpoint)) (gil::add-int-var-array-dom *sp* *cf-len (extended-cp-domain counterpoint)))
                        ; then treat the case where species = 1, 2, or 3
                        (case species
                            (1 (if (is-borrow-allowed)
                                ; then add to the penultimate note more possibilities
                                (setf (nth *cf-penult-index (first (notes counterpoint))) (gil::add-int-var-dom *sp* (chromatic-cp-domain counterpoint))) 
                            ))
                            (2 (progn
                                ; add the arsis counterpoint array (of [*cf-len - 1] length) to the space with the domain cp-domain
                                (setf (third (notes counterpoint)) (gil::add-int-var-array-dom *sp* *cf-last-index (extended-cp-domain counterpoint)))
                                ; add to the penultimate note more possibilities
                                (if (is-borrow-allowed)
                                    (setf (nth *cf-penult-index (third (notes counterpoint))) (gil::add-int-var-dom *sp* (chromatic-cp-domain counterpoint)))
                                )
                            ))
                            (3 (progn
                                (loop for i from 1 to 3 do
                                    ; add all quarter notes to the space with the domain (cp-domain counterpoint)
                                    (setf (nth i (notes counterpoint)) (gil::add-int-var-array-dom *sp* *cf-last-index (extended-cp-domain counterpoint)))
                                    
                                    (if (and (eq i 3) (is-borrow-allowed))
                                        ; then add to the penultimate note more possibilities
                                        (setf (nth *cf-penult-index (nth i (notes counterpoint))) (gil::add-int-var-dom *sp* (chromatic-cp-domain counterpoint)))
                                    )
                                )
                            ))
                        )
                    ))
                    (4 (progn
                        ; add the arsis counterpoint array (of [*cf-len - 1] length) to the space with the domain (cp-domain counterpoint)
                        (setf (third (notes counterpoint)) (gil::add-int-var-array-dom *sp* *cf-last-index (extended-cp-domain counterpoint)))
                        (setf (first (notes counterpoint)) (gil::add-int-var-array-dom *sp* *cf-last-index (extended-cp-domain counterpoint)))
                        ; add to the penultimate note more possibilities
                        (if (and (is-borrow-allowed) (/= *N-PARTS 1))
                            (progn
                            (setf (nth *cf-penult-index (third (notes counterpoint))) (gil::add-int-var-dom *sp* (chromatic-cp-domain counterpoint)))
                            (setf (nth *cf-penult-index (first (notes counterpoint))) (gil::add-int-var-dom *sp* (chromatic-cp-domain counterpoint)))
                            )
                        )
                    ))
                    (5 (progn
                        (loop for i from 0 to 3 do
                            (if (eq i 0)
                                (progn
                                    ; add all quarter notes to the space with the domain (notes counterpoint)-domain
                                    (setf (nth i (notes counterpoint)) (gil::add-int-var-array-dom *sp* *cf-len (extended-cp-domain counterpoint)))
                                    ; then add to the penultimate note more possibilities
                                    (if (is-borrow-allowed)
                                        (setf (nth *cf-penult-index (nth i (notes counterpoint))) (gil::add-int-var-dom *sp* (chromatic-cp-domain counterpoint)))
                                    )
                                )
                                (progn
                                    ; same as above but 1 note shorter
                                    (setf (nth i (notes counterpoint)) (gil::add-int-var-array-dom *sp* *cf-last-index (extended-cp-domain counterpoint)))
                                    (if (is-borrow-allowed)
                                        (setf (nth *cf-penult-index (nth i (notes counterpoint))) (gil::add-int-var-dom *sp* (chromatic-cp-domain counterpoint)))
                                    )
                                )
                            )
                        )
                        (setf (third (m-intervals-brut counterpoint)) (gil::add-int-var-array *sp* *cf-last-index -16 16)) 
                    ))
                )
                ; 3 voices specific
                (if (eq *N-PARTS 3) (let ( ; if re-mi-la-si is the last cf note then you can use a major third even if it's not in the harmony
                    (tonal (mod (car (last *cf)) 12))
                    )
                    (case tonal ((2 4 9 10) 
                        ; using the chromatic domain as it is going to be constrained to the harmonic triad by a later constraint
                        (setf (car (last (first (notes counterpoint)))) (gil::add-int-var-dom *sp* (chromatic-cp-domain counterpoint))) 
                    )))
                )
                counterpoint
            )
        )
    )
)

(defun fux-cp (species-list)
    "Dispatches the counterpoint generation to the appropriate function according to the species."
    (print (list "Chosen species for the counterpoints: " species-list))
    ; THE CSP SPACE 
    (defparameter *sp* (gil::new-space))

    ; re/set global variables
    (define-global-constants)
    (setq *species-list species-list) ; corresponds to the species of the counterpoints (1 5) means one ctp of 1st sp. and one ctp of 5th sp.
    (setq *cost-indexes (make-hash-table)) ; a hashmap recording which cost is at which position in the cost-factors list                   
    (setq *cost-factors (set-cost-factors)) ; the cost-factors list, contains all the individual costs

    ;; CREATE THE PARTS
    (setq counterpoints (make-list *N-COUNTERPOINTS :initial-element nil)) ; list containing the counterpoints
    (dotimes (i *N-COUNTERPOINTS) (setf (nth i counterpoints) (init-counterpoint (nth i *voices-types) (nth i species-list)))) ; init the counterpoints-
    (setq *cantus-firmus (init-cantus-firmus)) ; init the part 'object' for the cantus-firmus
    (setq *parts (cons *cantus-firmus counterpoints)) ; list containing all the parts in this order: (cf, cp1, cp2)
        

    ;; CREATE THE STRATA
    (setq *upper (make-list *N-COUNTERPOINTS :initial-element nil)) ; list containing the upper strata (the middle and the uppermost strata)
    (dotimes (i *N-COUNTERPOINTS) (setf (nth i *upper) (make-instance 'stratum-class))) ; declare the upper strata
    (setq *lowest (make-instance 'stratum-class)) ; declare the lowest stratum
    (setf (first (notes *lowest)) (gil::add-int-var-array *sp* *cf-len 0 120))
    (create-strata-arrays *parts) ; create the strata arrays 

    (case *N-COUNTERPOINTS
        (1 (progn 
            (fux-cp-cf (first *parts)) ; apply the constraints wrt. the cantus firmus
            (case (first species-list) ; in this case len(species-list) = 1
                (1 (fux-cp-1st (second *parts)))
                (2 (fux-cp-2nd (second *parts)))
                (3 (fux-cp-3rd (second *parts)))
                (4 (fux-cp-4th (second *parts)))
                (5 (fux-cp-5th (second *parts)))
                (otherwise (error "Species ~A not implemented" species))
            )
        ))
        (2 (fux-cp-3v species-list *parts)) ; 3v dispatcher
        (otherwise (error "Only two additional voices are implemented up to now. You asked for ~A." (length species)))
    )
)

(defun fux-search-engine (the-cp &optional (species '(1)) (voice-type 0))
    (let (se tstop sopts)
        (print (list "Starting fux-search-engine with species = " species))
        ;; COST
        (reorder-costs)
        (gil::g-cost *sp* *cost-factors) ; set the cost function

        ;; SPECIFY SOLUTION VARIABLES
        ; (print "Specifying solution variables...")
        (gil::g-specify-sol-variables *sp* the-cp)
        (gil::g-specify-percent-diff *sp* 0)
        
        ;; BRANCHING
        ; (print "Branching...")
        (setq var-branch-type gil::INT_VAR_DEGREE_MAX)
        (setq val-branch-type gil::INT_VAL_SPLIT_MIN)

        (gil::g-branch *sp* (first (notes *lowest)) gil::INT_VAR_DEGREE_MAX gil::INT_VAL_SPLIT_MIN)
        (dotimes (i *N-COUNTERPOINTS) (progn
            ; 5th species specific
            (if (eq (nth i species) 5) ; otherwise there is no species array
                (gil::g-branch *sp* (species-arr (nth i counterpoints)) var-branch-type gil::INT_VAL_RND)
            )

            ; 5th species specific
            (if (eq (nth i species) 5) (progn ; otherwise there is no species array
                (gil::g-branch *sp* (no-syncope-cost (nth i counterpoints)) var-branch-type val-branch-type)
                (gil::g-branch *sp* (not-cambiata-cost (nth i counterpoints)) var-branch-type val-branch-type)
            ))

            (if (eq (nth i species) 4) 
                (gil::g-branch *sp* (no-syncope-cost (nth i counterpoints)) var-branch-type gil::INT_VAL_MIN)
            )
        ))

        ;; Solution variables branching
        (gil::g-branch *sp* the-cp gil::INT_VAR_DEGREE_MAX gil::INT_VAL_RND) ; the-cp is all the solution arrays merged together 

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
        (setq se (gil::search-engine *sp* (gil::opts sopts) gil::BAB));
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
        (species-list (fifth l))
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
                ; else we have found a solution so break fthe loop
                (setf check nil)
            )
        ))

        ; print the solution from GiL
        (print "Solution: ")
        (handler-case
            (progn 
                (print (list "*cost-factors" (gil::g-values sol *cost-factors)))
                (print (list "sum of all costs = " (reduce #'+ (gil::g-values sol *cost-factors) :initial-value 0)))
            ) 
            (error (c)
                (dotimes (i *N-COST-FACTORS)
                    (handler-case (gil::g-values sol (nth i *cost-factors)) (error (c) (print (list "Cost" i "had a problem."))))
                )
                (error "All costs are not set correctly. Correct this problem before trying to find a solution.")
            )
        )
        
        (print (list "species = " species-list))
        
        (print "The solution can now be retrieved by evaluating the third output of cp-params.")
        (setq sol-pitches (gil::g-values sol the-cp)) ; store the values of the solution
        (let (
            (basic-rythmics (get-basic-rythmics species-list *cf-len sol-pitches counterpoints sol))
            (sol-voices (make-list *N-COUNTERPOINTS :initial-element nil))
            )

            (loop for i from 0 below *N-COUNTERPOINTS do (progn
                (setq rythmic+pitches (nth i basic-rythmics)) ; get the rythmic correpsonding to the species
                (setq rythmic-om (first rythmic+pitches))
                (setq pitches-om (second rythmic+pitches))
            )

                (setf (nth i sol-voices) (make-instance 'voice :chords (to-midicent pitches-om) :tree (om::mktree rythmic-om '(4 4)) :tempo *cf-tempo))
            )
            (make-instance 'poly :voices sol-voices)
        )
    )
)

; try to find a solution, catch errors from GiL and Gecode and restart the search
(defun try-find-solution (se)
    (handler-case
        (gil::search-next se) ; search the next solution, sol is the space of the solution
        (error (c)
            (print "A search was already running. Please start over by saving the configuration and starting the search.")
            ;(try-find-solution se)
        )
    )
)

; determines if the search has been stopped by the solver because there are no more solutions or if the user has stopped the search
(defun stopped-or-ended (stopped-se stop-user)
    (print (list "stopped-se" stopped-se "stop-user" stop-user))
    (if (= stopped-se 0); if the search has not been stopped by the TimeStop object, there is no more solutions
        (error "The search was stopped because no more solution was found. Either the best solution was found or none exist.")
    )
    ;otherwise, check if the user wants to keep searching or not
    (if stop-user
        (error "The search was stopped. Press next to continue the search.")
    )
)

(defun reorder-costs ()
    ; this function serves to put the costs in the order asked by the user and combines them using either a linear combination or a maximum minimisation
    ; in the interface the user selects an "importance" for each cost: it is the order in which the costs are being sorted.
    ; advice if someone wants to continue with the implementation: in a OOP language just define the order directly in a dictionary or somewhat so there is no need to reorder at some point
    
    ; at the end of this function, 'cost-names-by-order' 
    (setf costs-names-by-order (make-list 14 :initial-element nil)) ; there are fourteen types of cost
    ; take the costs in *cost-preferences* and sort them according to the user preferences in list 'costs-names-by-order'
    (maphash #'(lambda (key value)
            (setf value (- (parse-integer value) 1))
            (setf (nth value costs-names-by-order) (append (nth value costs-names-by-order) (list key)))
            )
        *cost-preferences*)
    ; now 'costs-names-by-order looks like this (python notation) [[cost1, cost2], [cost3], [cost4], [cost5, cost6]]
    ; which means first level for the lexicographic order are cost1 and cost2
    ; cost3 comes on the second level, cost4 on the third, and cost5 and cost6 are together on the fourth level

    ; reverse the cost order because when passing them between GiL and C++ they are reversed again 
    (setq costs-names-by-order (reverse costs-names-by-order))
    (print "Order of the costs, in reversed order:")
    (let (
        (i 0)
        (n-different-costs 0) ; the amount of cost types that have been encountered; not all the costs will be encountered, as some are species-specific (like the cost for no syncopation, that occurs only with a 4th species ctp)
        (reordered-costs (make-list *N-COST-FACTORS :initial-element nil))
        )
        (assert costs-names-by-order () "costs-names-by-order is nil, which means no preferences were passed. This is an implementation bug.")
        ; for each level of the ordered cost names (i.e. for each sublist in [[cost1, cost2], [cost3], [cost4], [cost5, cost6]])
        (dolist (preference-level costs-names-by-order)
            (let
                (
                    (current-cost-array '()) ; the array of the actual values of the costs in the preference-level
                    (current-cost-sum (gil::add-int-var *sp* 0 1000)) ; a variable representing the combination of the costs of this level
                )
                ; for each cost name in the level
                (dolist (cost preference-level)
                    (let ((index (gethash cost *cost-indexes)))
                        (if index (progn
                            (loop for index in (gethash cost *cost-indexes) do (progn
                                ; get the value of the cost and put in into the current array of cost values
                                (push (nth index *cost-factors) current-cost-array)
                            ))
                            )
                            ; if index is nil (i.e. if this cost doesn't exist in this species)
                            ; it is not a problem, it is the normal way of working since some costs don't exist in some species
                            #| debug |# ; (print (list "Cost " cost " was not found in this configuration."))
                        )
                    )
                )
                (if current-cost-array (progn ; if there was at least one cost on this level
                    (if *linear-combination 
                        ; if the user asked for linear combination then perform a linear combination
                        (gil::g-sum *sp* current-cost-sum current-cost-array)
                        ; else perform a maximum minimisation (take the maximum and the solver will minimise it)
                        (gil::g-lmax *sp* current-cost-sum current-cost-array)
                    )
                    ; put our linear combination or maximum minimisation into our global cost array
                    (setf (nth n-different-costs reordered-costs) current-cost-sum)
                    (print (list n-different-costs "th cost = " preference-level)) ; print the index at which the cost was added, please remember that it is reverted wrt. user preferences, as it will be reverted again when passing to GiL->C++
                    (incf n-different-costs) 
                ))
            )
        )
        ; if some costs are on the same level, then not all slots were used, get rid of them
        (setf reordered-costs (subseq reordered-costs 0 n-different-costs))
        ; verify that no index of the newly created cost array is nil
        (dolist (cost reordered-costs) (assert cost () "A cost is nil. Ordered costs = ~A. This is an implementation bug." reordered-costs))

        ; set the global variable to now be the reordered-costs
        (setf *cost-factors reordered-costs)
    )
)