(in-package :fuxcp)

(print "Loading fux-cp...")

; Lower bound and upper bound related to the cantus firmus pitch
(defparameter VOICE_TYPE 1)
(defparameter RANGE_UB (+ 20 (* 6 VOICE_TYPE)))
(defparameter RANGE_LB (+ -4 (* 6 VOICE_TYPE)))
; (defparameter RANGE_UB 17)
; (defparameter RANGE_LB 3)
(defvar *prev-sol-pitches nil)
(defvar rythmic+pitches nil)
(defvar rythmic-om nil)
(defvar pitches-om nil)

(defun set-space-variables ()
    ; THE CSP SPACE 
    (defparameter *sp* (gil::new-space))
    ; (defvar *b-nms nil)

    ;; CONSTANTS
    ; Motion types
    ; TODO change to 0 1 2
    (defparameter DIRECT 2)
    (defparameter OBLIQUE 1)
    (defparameter CONTRARY 0)

    ; Integer constants (to represent costs or intervals)
    ; -1 in IntVar
    (defparameter MINUS_ONE (gil::add-int-var-dom *sp* (list -1)))
    ; 0 in IntVar
    (defparameter ZERO (gil::add-int-var-dom *sp* (list 0)))
    ; 1 in IntVar
    (defparameter ONE (gil::add-int-var-dom *sp* (list 1)))
    ; 2 in IntVar
    (defparameter TWO (gil::add-int-var-dom *sp* (list 2)))
    ; 3 in IntVar (minor third)
    (defparameter THREE (gil::add-int-var-dom *sp* (list 3)))
    ; 7 in IntVar (perfect *fifth)
    (defparameter SEVEN (gil::add-int-var-dom *sp* (list 7)))
    ; 9 in IntVar (major sixth)
    (defparameter NINE (gil::add-int-var-dom *sp* (list 9)))

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
    ; dissonances intervals
    (defparameter DIS (list 1 2 5 6 10 11))
    ; penultimate intervals, i.e. minor third and major sixth
    (defparameter PENULT_CONS (list 3 9))
    ; penultimate thesis intervals, i.e. perfect fifth and sixth
    (defparameter PENULT_THESIS (list 7 8 9))
    ; penultimate 1st quarter note intervals, i.e. minor third, major sixth and octave/unisson
    (defparameter PENULT_1Q (list 0 3 8))
    ; penultimate syncope intervals, i.e. seconds and sevenths
    (defparameter PENULT_SYNCOPE (list 1 2 10 11))

    ; P_CONS in IntVar
    (defparameter P_CONS_VAR (gil::add-int-var-const-array *sp* P_CONS))
    ; IMP_CONS in IntVar
    (defparameter IMP_CONS_VAR (gil::add-int-var-const-array *sp* IMP_CONS))
    ; ALL_CONS in IntVar
    (defparameter ALL_CONS_VAR (gil::add-int-var-const-array *sp* ALL_CONS))
    ; PENULT_CONS in IntVar
    (defparameter PENULT_CONS_VAR (gil::add-int-var-const-array *sp* PENULT_CONS))
    ; PENULT_THESIS in IntVar
    (defparameter PENULT_THESIS_VAR (gil::add-int-var-const-array *sp* PENULT_THESIS))
    ; PENULT_1Q in IntVar
    (defparameter PENULT_1Q_VAR (gil::add-int-var-const-array *sp* PENULT_1Q))
    ; PENULT_SYNCOPE in IntVar
    (defparameter PENULT_SYNCOPE_VAR (gil::add-int-var-const-array *sp* PENULT_SYNCOPE))

    ;; CANTUS FIRMUS GLOBAL VARIABLES
    ; get the tonalite of the cantus firmus
    (defvar *tonalite-offset)
    ; get the *scale of the cantus firmus
    (defvar *scale)
    ; *chromatic *scale
    (defvar *chromatic-scale)
    ; get the first note of each chord of the cantus firmus
    (defvar *cf)
    ; get the first note of the cantus firmus ;; just used for the moment
    (defvar *tone-pitch-cf)
    ; get the borrowed scale of the cantus firmus, i.e. some notes borrowed from the natural scale of the tone (useful for modes)
    (defvar *borrowed-scale)
    ; get notes that are not in the natural scale of the tone
    (defvar *off-scale)
    ; set the pitch range of the counterpoint
    (defvar *cp-range) ; arbitrary range
    ; set counterpoint pitch domain
    (defvar *cp-domain)
    ; penultimate (first *cp) note domain
    (defvar *chromatic-cp-domain)
    ; set counterpoint extended pitch domain
    (defvar *extended-cp-domain)
    ; set the domain of the only barrowed notes
    (defvar *off-domain)
    ; length of the cantus firmus
    (defvar *cf-len)
    ; *cf-last-index AKA (cf-len - 1) often represents the last note of the counterpoint
    (defvar *cf-last-index)
    ; *cf-penult-index AKA (cf-len - 2) often represents the penultimate note of the counterpoint
    (defvar *cf-penult-index)
    ; *cf-brut-m-intervals
    (defvar *cf-brut-m-intervals)
    ; *BIG_COST is proportional to the length of the cantus firmus, used for soft constraints
    (defvar *BIG_COST)
    ; *BIG_COST in IntVar
    (defvar *BIG_COST_VAR)

    ;; FIRST SPECIES COUNTERPOINT GLOBAL VARIABLES
    ; (first *cp) (first *h-intervals) (first *m-intervals-brut) (first *m-intervals)
    ; *m2-intervals-brut *m2-intervals *cf-brut-m-intervals
    ; *is-p-cons-arr (first *motions)
    ; *is-cf-bass *cf-rel-pitch (first *is-cf-bass-arr)
    ; *cost-factors *total-cost *p-cons-cost *fifth-cost *octave-cost *tritone-cost
    (defvar *cp (list nil nil nil nil))
    (defvar *h-intervals (list nil nil nil nil))
    (defvar *m-intervals-brut (list nil nil nil nil))
    (defvar *m-intervals (list nil nil nil nil))
    (defvar *m2-intervals-brut)
    (defvar *m2-intervals)
    (defvar *cf-brut-m-intervals)
    (defvar *is-p-cons-arr)
    (defvar *motions (list nil nil nil nil))
    (defvar *is-cf-bass)
    ;; (defvar *cf-rel-pitch)
    (defvar *is-cf-bass-arr (list nil nil nil nil))
    (defvar *is-cp-off-key-arr)
    (defvar *N-COST-FACTORS)
    (defvar *cost-factors)
    (defvar *total-cost)
    (defvar *p-cons-cost)
    (defvar *fifth-cost)
    (defvar *octave-cost)
    (defvar *tritone-cost)
    (defvar *gr-one-degree-cost)
    (defvar *gr-two-degree-cost)
    (defvar *m-degrees-cost)
    (defvar *off-key-cost)

    ;; SECOND SPECIES COUNTERPOINT GLOBAL VARIABLES
    ; (third *cp)
    ; (third *h-intervals) *h-intervals-abs *h-intervals-brut
    ; (third *m-intervals-brut) (third *m-intervals)
    ; (first *m-succ-intervals) (first *m-succ-intervals-brut)
    ; *m-all-intervals *m-all-intervals-brut
    ; (third *motions) *real-motions
    ; *is-ta-dim-arr (third *is-cf-bass-arr) *is-nbour-arr
    ; *penult-thesis-cost
    ; *total-cp
    (defvar *h-intervals-abs)
    (defvar *h-intervals-brut)
    (defvar *m-succ-intervals (list nil nil nil))
    (defvar *m-succ-intervals-brut (list nil nil nil))
    (defvar *m2-len)
    (defvar *total-m-len)
    (defvar *m-all-intervals)
    (defvar *m-all-intervals-brut)
    (defvar *real-motions)
    (defvar *is-ta-dim-arr)
    (defvar *is-nbour-arr)
    (defvar *penult-thesis-cost)
    (defvar *total-cp)

    ;; THIRD SPECIES COUNTERPOINT GLOBAL VARIABLES
    (defvar *is-5qn-linked-arr)
    (defvar *total-cp-len)
    (defvar *is-cons-arr (list nil nil nil nil))
    (defvar *cons-cost (list nil nil nil nil))
    (defvar *is-not-cambiata-arr)
    (defvar *not-cambiata-cost)
    (defvar *m2-eq-zero-cost)
    (defvar *cancel-octave-leap-cost)

    ;; FOURTH SPECIES COUNTERPOINT GLOBAL VARIABLES
    (defvar *no-syncope-cost)

    ;; FIFTH SPECIES COUNTERPOINT GLOBAL VARIABLES
    (defvar *species-arr) ; 0: no constraint, 1: first species, 2: second species, 3: third species, 4: fourth species
    (defvar *sp-arr) ; represents *species-arr by position in the measure
    (defvar *is-nth-species-arr (list nil nil nil nil nil)) ; if *species-arr is n, then *is-nth-species-arr is true
    (defvar *is-3rd-species-arr (list nil nil nil nil)) ; if *species-arr is 3, then *is-3rd-species-arr is true
    (defvar *is-4th-species-arr (list nil nil nil nil)) ; if *species-arr is 4, then *is-4th-species-arr is true
    (defvar *is-2nd-or-3rd-species-arr) ; if *species-arr is 2 or 3, then *is-2nd-or-3rd-species-arr is true
    (defvar *m-ta-intervals) ; represents the m-intervals between the thesis note and the arsis note of the same measure
    (defvar *m-ta-intervals-brut) ; same but without the absolute reduction
    (defvar *is-mostly-3rd-arr) ; true if second, third and fourth notes are from the 3rd species
    ; (defvar *is-5qn-linked-arr-and-3rd) ; represents the conjunction of *is-5qn-linked-arr and *is-mostly-3rd-arr
    (defvar *is-constrained-arr) ; represents !(*is-0th-species-arr) i.e. there are species constraints
    (defvar *is-cst-arr (list nil nil nil nil)) ; represents *is-constrained-arr for all beats of the measure
)



;; DISPATCHER FUNCTION
(defun fux-cp (cantus-firmus &optional (species 1) (selected nil) (midi-selected nil))
    "Dispatches the counterpoint generation to the appropriate function according to the species."
    #| (if (= species 5)
        ; then directly call the fifth species function
        (fux-cp-5th cantus-firmus species)
    ) |#
    ; get the tonalite of the cantus firmus
    (setq *tonalite-offset (get-tone-offset cantus-firmus))
    ; get the *scale of the cantus firmus
    (setq *scale (build-scaleset (get-scale) *tonalite-offset))
    ; *chromatic *scale
    (setq *chromatic-scale (build-scaleset (get-scale "chromatic") *tonalite-offset))
    ; get the first note of each chord of the cantus firmus
    (setq *cf (mapcar #'first (to-pitch-list (om::chords cantus-firmus))))
    ; get the tempo of the cantus firmus
    (setq *cf-tempo (om::tempo cantus-firmus))
    ; get the first note of the cantus firmus ;; just used for the moment
    (setq *tone-pitch-cf (first *cf))
    ; get the borrowed scale of the cantus firmus, i.e. some notes borrowed from the natural scale of the tone (useful for modes)
    (setq *borrowed-scale (build-scaleset (get-scale "borrowed") (mod *tone-pitch-cf 12)))
    ; get notes that are not in the natural scale of the tone
    (setq *off-scale (set-difference *chromatic-scale *scale))
    ; set the pitch range of the counterpoint
    (setq *cp-range (range (+ *tone-pitch-cf RANGE_UB) :min (+ *tone-pitch-cf RANGE_LB))) ; arbitrary range
    ; set counterpoint pitch domain
    (setq *cp-domain (intersection *cp-range *scale))
    ; penultimate (first *cp) note domain
    (setq *chromatic-cp-domain (intersection *cp-range *chromatic-scale))
    ; set counterpoint extended pitch domain
    (setq *extended-cp-domain (intersection *cp-range (union *scale *borrowed-scale)))
    ; set the domain of the only barrowed notes
    (setq *off-domain (intersection *cp-range *off-scale))
    ; length of the cantus firmus
    (setq *cf-len (length *cf))
    ; *cf-last-index is the number of melodic intervals in the cantus firmus
    (setq *cf-last-index (- *cf-len 1))
    ; *cf-penult-index is the number of larger (n -> n+2) melodic intervals in the cantus firmus
    (setq *cf-penult-index (- *cf-len 2))
    ; ; *cf-brut-intervals is the list of brut melodic intervals in the cantus firmus
    ; (setq *cf-brut-m-intervals (gil::add-int-var-array *sp* *cf-last-index -127 127))
    ; (create-cf-brut-m-intervals *cf *cf-brut-m-intervals)
    ; ; *BIG_COST_VAR which is proportional to the length of the cantus firmus, used for soft constraints
    ; (setq *BIG_COST_VAR (gil::add-int-var-dom *sp* (list (* *cf-len 10))))

    ; re/set global variables
    (set-space-variables)

    ; *cf-brut-intervals is the list of brut melodic intervals in the cantus firmus
    (setq *cf-brut-m-intervals (gil::add-int-var-array *sp* *cf-last-index -127 127))

    (create-cf-brut-m-intervals *cf *cf-brut-m-intervals)
    ; *BIG_COST_VAR which is proportional to the length of the cantus firmus, used for soft constraints
    (setq *BIG_COST (* *cf-len 2))
    (setq *BIG_COST_VAR (gil::add-int-var-dom *sp* (list *BIG_COST)))
    
    (print (list "Choosing species: " species))
    (case species ; [1, 2, 3, 4, 5]
        (1 (progn
            (setq *N-COST-FACTORS 6)
            (fux-cp-1st cantus-firmus)
        ))
        (2 (progn
            (setq *N-COST-FACTORS 7)
            (fux-cp-2nd cantus-firmus)
        ))
        (3 (progn
            (setq *N-COST-FACTORS 9)
            (fux-cp-3rd cantus-firmus)
        ))
        (4 (progn
            (setq *N-COST-FACTORS 7)
            (fux-cp-4th cantus-firmus)
        ))
        (5 (progn
            (setq *N-COST-FACTORS 9)
            (fux-cp-5th cantus-firmus)
        ))
        (otherwise (error "Species ~A not implemented" species))
    )
)



;;==========================#
;; FIRST SPECIES            #
;;==========================#
(defun fux-cp-1st (cantus-firmus &optional (species 1))
    "Create the CSP for the first species of Fux's counterpoint."

    ;; RESET THE SPACE
    ; (print (list "*b-nms" *b-nms))
    ; (if *b-nms
    ;     (setq *sp* (gil::new-space))
    ;     ; (print "Resetting space")
    ; )

    ;; Print section
    (print "Cantus Firmus: ")
    (print (to-pitch-list (om::chords cantus-firmus)))
    (print "Tonalite: ")
    (print (om::tonalite cantus-firmus))
    (print "Tree: ")
    (print (om::tree cantus-firmus))
    (print "Scale: ")
    (print *scale)
    (print "Counterpoint domain: ")
    ; (print *cp-domain)
    (print *extended-cp-domain)


    ;============================================ CREATING GIL ARRAYS =============================
    ;; initialize the variables
    (print "Initializing variables...")
    
    ; add the counterpoint array to the space with the domain *cp-domain
    ; (setf (first *cp) (gil::add-int-var-array-dom *sp* *cf-len *cp-domain))
    (setf (first *cp) (gil::add-int-var-array-dom *sp* *cf-len *extended-cp-domain))
    
    (if (= species 1)
        ; then
        ; add to the penultimate note more possibilities
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

    (if (= species 1) ; only for the first species
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

    ;; ; creating brut melodic intervals array for cantus firmus
    ;; (setq *cf-brut-m-intervals (gil::add-int-var-array *sp* *cf-last-index -127 127))
    ;; (create-cf-brut-m-intervals *cf *cf-brut-m-intervals)

    ; creating perfect consonances boolean array
    (print "Creating perfect consonances boolean array...")
    ; array of BoolVar representing if the interval between the cantus firmus and the counterpoint is a perfect consonance
    (setq *is-p-cons-arr (gil::add-bool-var-array *sp* *cf-len 0 1))
    (create-is-p-cons-arr (first *h-intervals) *is-p-cons-arr)

    ; creating order/role of pitch array (if cantus firmus is higher or lower than counterpoint)
    ; 0 for being the bass, 1 for being above
    (print "Creating order of pitch array...")
    ;; (setq *cf-rel-pitch (gil::add-int-var-array *sp* *cf-len 0 1)) ; could be more than 2 voices but for now it's enough
    (setf (first *is-cf-bass-arr) (gil::add-bool-var-array *sp* *cf-len 0 1))
    ;; (create-cf-rel-pitch (first *cp) *cf *cf-rel-pitch (first *is-cf-bass-arr))
    (create-is-cf-bass-arr (first *cp) *cf (first *is-cf-bass-arr))

    ; creating motion array
    (print "Creating motion array...")
    (setf (first *motions) (gil::add-int-var-array *sp* *cf-last-index 0 2)) ; 0 = contrary, 1 = oblique, 2 = direct/parallel
    (create-motions (first *m-intervals-brut) *cf-brut-m-intervals (first *motions))


    ;============================================ HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    ; for all intervals between the cantus firmus and the counterpoint, the interval must be a consonance
    (print "Harmonic consonances...")
    (case species
        (1 (add-h-cons-cst *cf-len *cf-penult-index (first *h-intervals)))
        (2 (add-h-cons-cst *cf-len *cf-penult-index (first *h-intervals) PENULT_THESIS_VAR))
        (3 (add-h-cons-cst *cf-len *cf-penult-index (first *h-intervals) PENULT_1Q_VAR))
        ; (4 (add-h-cons-cst *cf-len *cf-penult-index (first *h-intervals) PENULT_THESIS_VAR))
        ; (5 (add-h-cons-cst *cf-len *cf-penult-index (first *h-intervals) PENULT_THESIS_VAR))
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
    (if (= species 1)
        ; then
        (add-penult-cons-cst (penult (first *is-cf-bass-arr)) (penult (first *h-intervals)))
    )


    ;============================================ MELODIC CONSTRAINTS =============================
    
    ; NOTE: with the degree iii in penultimate *cf measure -> no solution bc there is a *tritone between I#(minor third) and V.
    ; no *tritone melodic interval
    ; (print "No *tritone...")
    ; (gil::g-rel *sp* (first *m-intervals) gil::IRT_NQ 6)
    
    (print "Melodic constraints...")
    (if (= species 1)
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
    ; IntVar array representing all the cost factors
    (setq *cost-factors (gil::add-int-var-array *sp* *N-COST-FACTORS (* *cf-len -10) (* *cf-len 20)))
    ; IntVar representing the *total *cost
    (setq *total-cost (gil::add-int-var *sp* (* *cf-len -10) (* *cf-len 20)))
    
    ; 1, 2) imperfect consonances are preferred to perfect consonances
    (print "Imperfect consonances are preferred to perfect consonances...")
    ;; TODO replace *fifth-cost and *octave-cost by a single array
    (add-p-cons-cost-cst)
    #| (setq *fifth-cost  (gil::add-int-var-array *sp* *cf-len 0 1)) ; IntVar array representing the cost to have fifths
    (setq *octave-cost (gil::add-int-var-array *sp* *cf-len 0 1)) ; IntVar array representing the cost to have octaves
    (add-cost-cst (first *h-intervals) gil::IRT_EQ 7 *fifth-cost) ; *fifth-cost = 1 if *h-interval == 7
    (add-cost-cst (first *h-intervals) gil::IRT_EQ 0 *octave-cost) ; *octave-cost = 1 if *h-interval == 0
    (gil::g-sum *sp* (nth 0 *cost-factors) *fifth-cost) ; sum of the cost of the fifth consonances
    (gil::g-sum *sp* (nth 1 *cost-factors) *octave-cost) ; sum of the cost of the octave consonances |#

    (if (= species 1)
        ; then
        (progn
            ; 3, 4, 5) add off-key cost, m-degrees cost and tritons cost
            (setq *m-all-intervals (first *m-intervals))
            (set-general-costs-cst *cf-len)

            ; 6) motion costs
            (gil::g-sum *sp* (nth 5 *cost-factors) (first *motions)) ; sum of the cost of the (first *motions)
            
            ; TODO) melodic intervals should be as small as possible
            (print "Melodic intervals should be as small as possible...")
            ; IntVar array representing the cost to have melodic intervals > 2
            (print 'debug1)
            (setq *gr-one-degree-cost (gil::add-int-var-array *sp* *cf-last-index 0 1))
            ; IntVar array representing the cost to have melodic intervals > 3
            (setq *gr-two-degree-cost (gil::add-int-var-array *sp* *cf-last-index 0 1))
            (add-cost-cst (first *m-intervals) gil::IRT_GR 2 *gr-one-degree-cost)
            (add-cost-cst (first *m-intervals) gil::IRT_GR 4 *gr-two-degree-cost)
            ; (gil::g-sum *sp* (nth 4 *cost-factors) *gr-one-degree-cost)
            ; (gil::g-sum *sp* (nth 5 *cost-factors) *gr-two-degree-cost)
            
            ; total cost
            (gil::g-sum *sp* *total-cost *cost-factors) ; sum of all the cost factors
            (gil::g-cost *sp* *total-cost) ; set the cost function
        )
    )


    ; RETURN
    (if (= species 1)
        ; then create the search engine
        (append (fux-search-engine (first *cp)) (list species))
        ; else
        nil
    )
)



;;==========================#
;; SECOND SPECIES           #
;;==========================#
;; Note: fux-cp-2nd execute the first species algorithm without some constraints.
;; In this function, all the variable names without the arsis-suffix refers to thesis notes AKA the first species notes.
;; All the variable names with the arsis-suffix refers to arsis notes AKA notes on the upbeat.
(defun fux-cp-2nd (cantus-firmus &optional (species 2))
    "Create the CSP for the 2nd species of Fux's counterpoint, with the cantus firmus as input"

    ;; ADD FIRST SPECIES CONSTRAINTS
    (fux-cp-1st cantus-firmus 2)

    (print "########## SECOND SPECIES ##########")

    ;======================================== CREATION OF GIL ARRAYS ==========================
    (print "Initializing variables...")
    ; add the arsis counterpoint array (of [*cf-len - 1] length) to the space with the domain *cp-domain
    ; (setf (third *cp) (gil::add-int-var-array-dom *sp* *cf-last-index *cp-domain))
    (setf (third *cp) (gil::add-int-var-array-dom *sp* *cf-last-index *extended-cp-domain))
    ; add to the penultimate note more possibilities
    ; (setf (nth *cf-penult-index (first *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain)) ; TODO TEMP
    (setf (nth *cf-penult-index (third *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain))
    
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
    (setq *real-motions (gil::add-int-var-array *sp* *cf-last-index 0 2))
    (create-motions (third *m-intervals-brut) *cf-brut-m-intervals (third *motions))
    (create-real-motions (first *m-succ-intervals) (first *motions) (third *motions) *real-motions)

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

    ; for all harmonic intervals between the cantus firmus and the thesis notes, the interval must be a consonance
    (print "Harmonic consonances...")
    ; here the penultimate thesis note must be a fifth (sixth if not possible)
    ; (add-h-cons-cst *cf-len *cf-penult-index (first *h-intervals) PENULT_THESIS_VAR)
    
    ; for all harmonic intervals between the cantus firmus and the arsis notes, the interval must be a consonance
    ; unless the arsis note is a diminution
    (print "No dissonance unless diminution for arsis notes...")
    (add-h-cons-arsis-cst *cf-len *cf-penult-index (third *h-intervals) *is-ta-dim-arr)

    ; TODO: Fux does not follow this rule so deactivate ?
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

    ; TODO: Fux does not follow this rule, deactivate ?
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
    ; (add-no-battuta-cst *real-motions (first *h-intervals) (third *m-intervals-brut) (third *is-cf-bass-arr))
    ; adapted during thesis writing to
    (add-no-battuta-cst (third *motions) (first *h-intervals) (third *m-intervals-brut) (third *is-cf-bass-arr))



    ;======================================== COST FACTORS ====================================
    ; (setq *p-cons-cost (gil::add-int-var-array *sp* *cf-len 0 1)) ; IntVar array representing the cost to have perfect consonances

    ; 1, 2) imperfect consonances are preferred to perfect consonances
    
    ; 3, 4, 5) add off-key cost, m-degrees cost and tritons cost
    (set-general-costs-cst)
    
    ; 6) contrary motion is preferred
    (gil::g-sum *sp* (nth 5 *cost-factors) *real-motions) ; sum of the cost of the (first *motions)
    
    ; 7) the penultimate thesis note is not a fifth
    (print "Penultimate thesis note is not a fifth...")
    ; *penult-thesis-cost = *cf-len (big cost) if penultimate *h-interval /= 7
    (setq *penult-thesis-cost (gil::add-int-var-dom *sp* (list 0 *BIG_COST)))
    (add-single-cost-cst (penult (first *h-intervals)) gil::IRT_NQ 7 *penult-thesis-cost *BIG_COST_VAR) ; TODO TEMP
    (setf (nth 6 *cost-factors) *penult-thesis-cost)

    ; TODO) melodic intervals should be as small as possible
    (print "Melodic intervals should be as small as possible...")
    ; (gil::g-sum *sp* (nth 4 *cost-factors) *m-all-intervals) ; sum of the cost of the melodic intervals
    ; IntVar array representing the cost to have melodic intervals > 2
    (setq *gr-one-degree-cost (gil::add-int-var-array *sp* *total-m-len 0 1))
    ; IntVar array representing the cost to have melodic intervals > 4
    (setq *gr-two-degree-cost (gil::add-int-var-array *sp* *total-m-len 0 1))
    (add-cost-cst *m-all-intervals gil::IRT_GR 2 *gr-one-degree-cost)
    (add-cost-cst *m-all-intervals gil::IRT_GR 4 *gr-two-degree-cost)
    ; (gil::g-sum *sp* (nth 4 *cost-factors) *gr-one-degree-cost)
    ; (gil::g-sum *sp* (nth 5 *cost-factors) *gr-two-degree-cost)



    ;======================================== COST FUNCTION ===================================
    (print "Cost function...")
    ; *total *cost
    (gil::g-sum *sp* *total-cost *cost-factors) ; sum of all the cost factors
    (gil::g-cost *sp* *total-cost) ; set the cost function

    ; (setf (nth (- (length *total-cp) 1) *total-cp) (gil::add-int-var-dom *sp* *chromatic-cp-domain)) ; TEMP
    
    ; specify a certain solution for testing
    ; (gil::g-rel *sp* (fifth *m-all-intervals) gil::IRT_GR 7) ; TODO TEMP

    ; RETURN
    (if (= species 2)
        ; then create the search engine
        (append (fux-search-engine *total-cp) (list species))
        ; else
        nil
    )
)


;;==========================#
;; THIRD SPECIES            #
;;==========================#
;; Note: fux-cp-3rd execute the first species algorithm without some constraints.
;; In this function, 4 quarter notes by measure are assumed.
(defun fux-cp-3rd (cantus-firmus &optional (species 3))
    "Create the CSP for the 3rd species of Fux's counterpoint, with the cantus firmus as input"
    (print "Creating the CSP for the 3rd species of Fux's counterpoint...")

    ;; ADD FIRST SPECIES CONSTRAINTS
    (fux-cp-1st cantus-firmus 3)

    (print "########## THIRD SPECIES ##########")

    ;======================================== CREATION OF GIL ARRAYS ==========================
    (print "Initializing variables...")
    
    (loop for i from 1 to 3 do
        ; add all quarter notes to the space with the domain *cp-domain
        (setf (nth i *cp) (gil::add-int-var-array-dom *sp* *cf-last-index *extended-cp-domain))
        
        (if (= i 3)
            ; then add to the penultimate note more possibilities
            (progn
                (setf (nth *cf-penult-index (nth i *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain))
            )
        )
    )

    (loop for i from 1 to 3 do
        (setq i-1 (- i 1))
        ; creating harmonic intervals array
        ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the counterpoint
        ; (print "Creating harmonic intervals array...")
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

    ; FORCE A CERTAIN SOLUTION
    ; (setq test (list 65 64 62 60 59 62 67 65 64 62 60 58 57 60 62 64 65 62 64 65 67 64 65 67 69 67 65 69 67 65 64 62 60 64 60 64 65 64 62 60 58 60 62 64 65))
    ; (loop for i from 0 below (length test) do
    ;     (gil::g-rel *sp* (nth i *total-cp) gil::IRT_EQ (nth i test))
    ; )
    
    ; array of IntVar representing the absolute intervals (not % 12) and brut (just p - q)
    ; between the cantus firmus and the counterpoint (thesis notes)

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
    ; (setq *real-motions (gil::add-int-var-array *sp* *cf-last-index 0 2))
    (create-motions (fourth *m-intervals-brut) *cf-brut-m-intervals (fourth *motions))
    ; (create-real-motions (first *m-succ-intervals) (first *motions) (third *motions) *real-motions)

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

    ; TODO: new version, check if it works
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
        (if (= i 0)
            (setf (nth i *is-cons-arr) (gil::add-bool-var-array *sp* *cf-len 0 1))
            (setf (nth i *is-cons-arr) (gil::add-bool-var-array *sp* *cf-last-index 0 1))
        )
        (create-is-member-arr (nth i *h-intervals) (nth i *is-cons-arr))
    )

    ; creating boolean is not cambiata array
    (print "Creating is not cambiata array...")
    (setq *is-not-cambiata-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    ; (create-is-not-cambiata-arr (second *is-cons-arr) (third *is-cons-arr) (first *m-succ-intervals) *is-not-cambiata-arr)
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
    ; (add-linked-5qn-cst (first *is-cons-arr) (third *is-cons-arr) *is-5qn-linked-arr)
    (add-linked-5qn-cst (third *is-cons-arr) *is-5qn-linked-arr)

    ; any dissonant note implies that it is surrounded by consonant notes
    (print "Any dissonant note...")
    ; (add-h-dis-imp-cons-cst (first *is-cons-arr) (second *is-cons-arr) (third *is-cons-arr))
    ; (add-h-dis-imp-cons-cst (second *is-cons-arr) (third *is-cons-arr) (fourth *is-cons-arr))
    ; (add-h-dis-imp-cons-cst (third *is-cons-arr) (fourth *is-cons-arr) (rest (first *is-cons-arr)))
    (add-h-dis-or-cons-3rd-cst (second *is-cons-arr) (third *is-cons-arr) (fourth *is-cons-arr) *is-ta-dim-arr)
    ; first note always consonant, necessary ?
    ; (add-h-dis-imp-cons-cst (butlast (fourth *is-cons-arr)) (restbutlast (first *is-cons-arr)) (rest (second *is-cons-arr)))


    ;======================================== MELODIC CONSTRAINTS =============================
    (print "Melodic constraints...")

    ; no melodic interval between 9 and 11
    (loop for m in *m-succ-intervals do
        (add-no-m-jump-extend-cst m)
    )
    ; (add-no-m-jump-extend-cst (first *m-intervals))
    (add-no-m-jump-extend-cst (fourth *m-intervals))

    ; no *chromatic motion between three consecutive notes
    (print "No chromatic motion...")
    (add-no-chromatic-m-cst *m-all-intervals-brut *m2-intervals-brut)

    ; no unisson between two consecutive notes
    ; (print "No unisson between two consecutive notes...")
    ; (add-no-unisson-at-all-cst *total-cp (rest *total-cp))
    ; (gil::g-rel *sp* (first *m-succ-intervals) gil::IRT_NQ 0)
    ; (gil::g-rel *sp* (third *m-succ-intervals) gil::IRT_NQ 0)
    ; (gil::g-rel *sp* (second *m-succ-intervals) gil::IRT_NQ 0)
    ; (gil::g-rel *sp* (fourth *m-intervals) gil::IRT_NQ 0)
    ; (gil::g-rel *sp* *m-all-intervals gil::IRT_NQ 0)


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

    #| ; 1, 2) imperfect consonances are preferred to perfect consonances
    ;   -> already done in the first species ? Not for now...
    (print "Imperfect consonances are preferred to perfect consonances...")
    ;; TODO replace *fifth-cost and *octave-cost by a single array0 array representing the cost to have octaves
    (add-cost-cst (first *h-intervals) gil::IRT_EQ 7 *fifth-cost) ; *fifth-cost = 1 if *h-interval == 7
    (add-cost-cst (first *h-intervals) gil::IRT_EQ 0 *octave-cost) ; *octave-cost = 1 if *h-interval == 0
    (gil::g-sum *sp* (nth 4 *cost-factors) *fifth-cost) ; sum of the cost of the fifth consonances
    (gil::g-sum *sp* (nth 5 *cost-factors) *octave-cost) ; sum of the cost of the octave consonances |#

    ; 3, 4, 5) add off-key cost, m-degrees cost and tritons cost
    (set-general-costs-cst)
    
    ; 6) contrary motion is preferred
    (gil::g-sum *sp* (nth 5 *cost-factors) (fourth *motions)) ; sum of the cost of the (fourth *motions)
    
    ; TODO) melodic intervals should be as small as possible
    ; IntVar array representing the cost to have melodic intervals > 2
    (setq *gr-one-degree-cost (gil::add-int-var-array *sp* *total-m-len 0 1))
    ; IntVar array representing the cost to have melodic intervals > 3
    (setq *gr-two-degree-cost (gil::add-int-var-array *sp* *total-m-len 0 1))
    (add-cost-cst *m-all-intervals gil::IRT_GR 2 *gr-one-degree-cost)
    (add-cost-cst *m-all-intervals gil::IRT_GR 4 *gr-two-degree-cost)
    ; (gil::g-sum *sp* (nth 0 *cost-factors) *gr-one-degree-cost)
    ; (gil::g-sum *sp* (nth 1 *cost-factors) *gr-two-degree-cost)


    ; 7) cambiata notes are preferred (cons - dis - cons > cons - cons - cons)
    (print "Cambiata notes are preferred...")
    ; IntVar array representing the cost to have cambiata notes
    (setq *not-cambiata-cost (gil::add-int-var-array *sp* *cf-last-index 0 *BIG_COST))
    (add-cost-bool-cst *is-not-cambiata-arr *not-cambiata-cost *BIG_COST_VAR)
    (gil::g-sum *sp* (nth 6 *cost-factors) *not-cambiata-cost) ; sum of the cost of the cambiata notes

    ; 8) intervals between notes n and n+2 are prefered greater than zero
    (print "Intervals between notes n and n+2 are prefered different than zero...")
    ; IntVar array representing the cost to have intervals between notes n and n+2 equal to zero
    (setq *m2-eq-zero-cost (gil::add-int-var-array *sp* *m2-len 0 1))
    (add-cost-cst *m2-intervals gil::IRT_EQ 0 *m2-eq-zero-cost)
    (gil::g-sum *sp* (nth 7 *cost-factors) *m2-eq-zero-cost) ; sum of the cost of the intervals between notes n and n+2 equal to zero

    ; 9) cancel octave leap cost generated by the previous constraint 4)
    (print "Cancel octave leap cost...")
    ; IntVar array representing the cost to cancel octave leap cost
    (setq *cancel-octave-leap-cost (gil::add-int-var-array *sp* *total-m-len -1 0))
    (add-cost-cst *m-all-intervals gil::IRT_EQ 12 *cancel-octave-leap-cost MINUS_ONE)
    ; (gil::g-sum *sp* (nth 8 *cost-factors) *cancel-octave-leap-cost) ; sum of the cost of the octave leap cost
    (setf (nth 8 *cost-factors) ONE) ; TEMP


    ;======================================== COST FUNCTION ===================================
    (print "Cost function...")
    ; *total *cost
    (gil::g-sum *sp* *total-cost *cost-factors) ; sum of all the cost factors
    ; (gil::g-sum *sp* *total-cost *gr-one-degree-cost) ; sum of all the cost factors
    (gil::g-cost *sp* *total-cost) ; set the cost function
    
    ; specify a certain solution for testing
    ; (gil::g-rel *sp* (lastone (fourth *m-intervals)) gil::IRT_LE 3) ; TODO TEMP
    ; (gil::g-rel *sp* (penult (fourth *m-intervals)) gil::IRT_LE 3) ; TODO TEMP
    ; (gil::g-rel *sp* (penult (first *h-intervals)) gil::IRT_EQ 3) ; TODO TEMP
    ; (gil::g-rel *sp* *m-all-intervals gil::IRT_LE 5) ; TODO TEMP
    ; (gil::g-rel *sp* (first *total-cp) gil::IRT_EQ 71) ; TODO TEMP
    
    ; (loop for i from 0 to 3 do
    ;     (setf (nth i *cons-cost) (gil::add-int-var-array *sp* *cf-last-index 0 1)) ; IntVar representing the cost to have a consonance
    ;     (add-cost-bool-cst (nth i *is-cons-arr) (nth i *cons-cost)) ; *cons-cost = 1 if *is-cons-arr == 1
    ; )


    ; RETURN
    (if (= species 3)
        ; then create the search engine
        ; (append (fux-search-engine *total-cp) (list species))
        (append (fux-search-engine *total-cp species) (list species))
        ; else
        nil
    )
)


;;==========================#
;; FOURTH SPECIES           #
;;==========================#
;; Note: fux-cp-4nd execute the first species algorithm without some constraints.
;; In this function, the first notes are in Arsis because of the syncopation.
(defun fux-cp-4th (cantus-firmus &optional (species 4))
    "Create the CSP for the 2nd species of Fux's counterpoint, with the cantus firmus as input"

    ;; ADD FIRST SPECIES CONSTRAINTS
    ; (fux-cp-1st cantus-firmus 4)

    (print "########## FOURTH SPECIES ##########")

    ;======================================== CREATION OF GIL ARRAYS ==========================
    (print "Initializing variables...")
    ; add the arsis counterpoint array (of [*cf-len - 1] length) to the space with the domain *cp-domain
    (setf (third *cp) (gil::add-int-var-array-dom *sp* *cf-last-index *extended-cp-domain))
    (setf (first *cp) (gil::add-int-var-array-dom *sp* *cf-last-index *extended-cp-domain))
    ; add to the penultimate note more possibilities
    (setf (nth *cf-penult-index (third *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain))
    (setf (nth *cf-penult-index (first *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain)) ; TODO TEMP
    
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

    ; creating motion array
    ; 0 = contrary, 1 = oblique, 2 = direct/parallel
    #| (print "Creating motion array...")
    (setf (third *motions) (gil::add-int-var-array *sp* *cf-last-index -1 1))
    (create-motions (first *m-succ-intervals-brut) *cf-brut-m-intervals (third *motions)) |#

    #| ; creating boolean diminution array
    (print "Creating diminution array...")
    ; Note: a diminution is the intermediate note that exists between two notes separated by a jump of a third
    ; i.e. E -> D (dim) -> C
    (setq *is-ta-dim-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-ta-dim-arr (first *m-succ-intervals) (first *m-intervals) (third *m-intervals) *is-ta-dim-arr) |#

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


    ;======================================== HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    ; for all harmonic intervals between the cantus firmus and the thesis notes, the interval must be a consonance
    (print "Harmonic consonances...")
    ; here the penultimate thesis note must be a seventh or a second and the arsis note must be a major sixth or a minor third
    (gil::g-member *sp* PENULT_SYNCOPE_VAR (penult (first *h-intervals)))
    (add-h-cons-cst *cf-len *cf-penult-index (third *h-intervals))

    ; must start with a perfect consonance
    (print "Perfect consonance at the beginning...")
    ; (add-p-cons-start-cst (third *cp) *cf)
    (add-p-cons-start-cst (third *h-intervals))

    ; must end with a perfect consonance
    (print "Perfect consonance at the end...")
    ; (add-p-cons-end-cst (first *cp) *cf)
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
    ; (gil::g-rel *sp* (third *m-intervals) gil::IRT_LQ 4)

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

    #| ; no direct motion to reach a perfect consonance
    (print "No direct motion to reach a perfect consonance...")
    (add-no-direct-move-to-p-cons-cst *real-motions *is-p-cons-arr)

    ; no battuta kind of motion
    ; i.e. contrary motion to an *octave, lower voice up, higher voice down, counterpoint melodic interval < -4
    (print "No battuta kind of motion...")
    (add-no-battuta-cst *real-motions (first *h-intervals) (third *m-intervals-brut) (third *is-cf-bass-arr)) |#



    ;======================================== COST FACTORS ====================================
    (print "Cost factors...")
    ; IntVar array representing all the cost factors
    (setq *cost-factors (gil::add-int-var-array *sp* *N-COST-FACTORS (* *cf-len -10) (* *cf-len 20)))
    ; IntVar representing the *total *cost
    (setq *total-cost (gil::add-int-var *sp* (* *cf-len -10) (* *cf-len 20)))

    ; 1, 2) imperfect consonances are preferred to perfect consonances
    (add-p-cons-cost-cst *cf-last-index)
    
    ; 3, 4, 5) add off-key cost, m-degrees cost and tritons cost
    (set-general-costs-cst)

    ; 6) add no syncopation cost
    (print "No syncopation cost...")
    (setq *no-syncope-cost (gil::add-int-var-array *sp* *cf-penult-index 0 *big_cost))
    (add-cost-cst (butlast (third *m-intervals)) gil::IRT_NQ 0 *no-syncope-cost *big_cost_var)
    (gil::g-sum *sp* (nth 5 *cost-factors) *no-syncope-cost)

    ; 7) add m2-intervals equal to 0 cost
    (print "Monotonia...")
    ; (setq *m2-eq-zero-cost (gil::add-int-var-array *sp* *cf-penult-index 0 1))
    (setq *m2-eq-zero-cost (gil::add-int-var-array *sp* (- *cf-len 3) 0 1))
    ; (add-cost-cst (first *m-succ-intervals) gil::IRT_EQ 0 *m2-eq-zero-cost)
    (add-cost-multi-cst (second *cp) gil::IRT_EQ (cddr (second *cp)) *m2-eq-zero-cost)
    (gil::g-sum *sp* (nth 6 *cost-factors) *m2-eq-zero-cost)

    ; TODO) melodic intervals should be as small as possible
    (print "Melodic intervals should be as small as possible...")
    ; (gil::g-sum *sp* (nth 4 *cost-factors) *m-all-intervals) ; sum of the cost of the melodic intervals
    (setq *gr-one-degree-cost (gil::add-int-var-array *sp* *total-m-len 0 1)) ; IntVar array representing the cost to have melodic intervals > 2
    (setq *gr-two-degree-cost (gil::add-int-var-array *sp* *total-m-len 0 1)) ; IntVar array representing the cost to have melodic intervals > 4
    (add-cost-cst *m-all-intervals gil::IRT_GR 2 *gr-one-degree-cost)
    (add-cost-cst *m-all-intervals gil::IRT_GR 4 *gr-two-degree-cost)
    ; (gil::g-sum *sp* (nth 4 *cost-factors) *gr-one-degree-cost)
    ; (gil::g-sum *sp* (nth 5 *cost-factors) *gr-two-degree-cost)



    ;======================================== COST FUNCTION ===================================
    (print "Cost function...")
    ; *total *cost
    (gil::g-sum *sp* *total-cost *cost-factors) ; sum of all the cost factors
    (gil::g-cost *sp* *total-cost) ; set the cost function

    ; (setf (nth (- (length *total-cp) 1) *total-cp) (gil::add-int-var-dom *sp* *chromatic-cp-domain)) ; TEMP
    
    ; specify a certain solution for testing
    ; (gil::g-rel *sp* (lastone *m-all-intervals) gil::IRT_LE 3) ; TODO TEMP
    ; (gil::g-rel *sp* (lastone (first *h-intervals)) gil::IRT_EQ 0) ; TODO TEMP
    ; (gil::g-rel *sp* (first (third *h-intervals)) gil::IRT_EQ 0) ; TODO TEMP
    ; (gil::g-rel *sp* (first (first *h-intervals)) gil::IRT_EQ 0) ; TODO TEMP
    ; (gil::g-rel *sp* (first *no-syncope-cost) gil::IRT_EQ 0) ; TODO TEMP

    ; RETURN
    (if (= species 4)
        ; then create the search engine
        (append (fux-search-engine *total-cp) (list species))
        ; else
        nil
    )
)

;;==========================#
;; FIFTH SPECIES            #
;;==========================#
;; Note: fux-cp-3rd execute the first species algorithm without some constraints.
;; In this function, 4 quarter notes by measure are assumed.
(defun fux-cp-5th (cantus-firmus &optional (species 5))
    "Create the CSP for the 3rd species of Fux's counterpoint, with the cantus firmus as input"
    (print "Creating the CSP for the 3rd species of Fux's counterpoint...")

    ;; CLEANING PREVIOUS SOLUTIONS
    (setq *prev-sol-pitches nil)
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
    ; (gil::g-rel *sp* *species-arr gil::IRT_GQ 3) ; TODO TEMP
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


    ;======================================== CREATION OF GIL ARRAYS ==========================
    (print "Initializing variables...")
    
    (loop for i from 0 to 3 do
        (if (= i 0)
            (progn
                ; add all quarter notes to the space with the domain *cp-domain
                (setf (nth i *cp) (gil::add-int-var-array-dom *sp* *cf-len *extended-cp-domain))
                ; then add to the penultimate note more possibilities
                (setf (nth *cf-penult-index (nth i *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain))
                ; creating harmonic intervals array
                (print "Creating harmonic intervals array...")
                ; array of IntVar representing the absolute intervals % 12 between the cantus firmus and the counterpoint
                (setf (nth i *h-intervals) (gil::add-int-var-array *sp* *cf-len 0 11))
                (create-h-intervals (nth i *cp) *cf (nth i *h-intervals))
            )
            (progn
                ; same as above but 1 note shorter
                (setf (nth i *cp) (gil::add-int-var-array-dom *sp* *cf-last-index *extended-cp-domain))
                (setf (nth *cf-penult-index (nth i *cp)) (gil::add-int-var-dom *sp* *chromatic-cp-domain))
                (setf (nth i *h-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 11))
                (create-h-intervals (nth i *cp) (butlast *cf) (nth i *h-intervals))
            )
        )
    )

    (loop for i from 0 to 2 do
        (setq i+1 (+ i 1))
        (setf (nth i *m-succ-intervals-brut) (gil::add-int-var-array *sp* *cf-last-index -12 12))
        (if (= i 1)
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
    
    ; array of IntVar representing the absolute intervals (not % 12) and brut (just p - q)
    ; between the cantus firmus and the counterpoint (thesis notes)

    ; creating melodic intervals array
    (print "Creating melodic intervals array...")
    ; array of IntVar representing the melodic intervals between arsis and next thesis note of the counterpoint
    (setf (third *m-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 12))
    (setf (third *m-intervals-brut) (gil::add-int-var-array *sp* *cf-last-index -12 12)) ; same without absolute reduction
    (create-m-intervals-next-meas (third *cp) (first *cp) (third *m-intervals) (third *m-intervals-brut))
    ; array of IntVar representing the absolute intervals
    ; between the last note of measure m and the first note of measure m+1 of the counterpoint
    (setf (fourth *m-intervals) (gil::add-int-var-array *sp* *cf-last-index 0 12)) ; can be 0 if this is replace by 2 eight note
    (setf (fourth *m-intervals-brut) (gil::add-int-var-array *sp* *cf-last-index -12 12)) ; same without absolute reduction
    (create-m-intervals-next-meas (fourth *cp) (first *cp) (fourth *m-intervals) (fourth *m-intervals-brut))
    
    ; array of IntVar representing the melodic intervals between the thesis note and the arsis note of the same measure
    (setq *m-ta-intervals (gil::add-int-var-array *sp* *cf-last-index 0 12))
    (setq *m-ta-intervals-brut (gil::add-int-var-array *sp* *cf-last-index -12 12)) ; same without absolute reduction
    (create-intervals (first *cp) (third *cp) *m-ta-intervals *m-ta-intervals-brut)
    
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
    ; (setq *real-motions (gil::add-int-var-array *sp* *cf-last-index 0 2))
    (create-motions (fourth *m-intervals-brut) *cf-brut-m-intervals (fourth *motions))
    ; (create-real-motions (first *m-succ-intervals) (first *motions) (third *motions) *real-motions)

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
    ; (setq *is-5qn-linked-arr-and-3rd (gil::add-bool-var-array *sp* *cf-last-index 0 1)) ; 5th
    ; (bot-merge-array *is-5qn-linked-arr *is-mostly-3rd-arr *is-5qn-linked-arr-and-3rd)
    
    ; creating boolean is constrained array
    (print "Creating is constrained array...")
    ; array of BoolVar representing if the interval is constrained
    (setq *is-constrained-arr (collect-not-array (nth 0 *is-nth-species-arr)))

    ; creating boolean is consonant array + species array
    (print "Creating is consonant array and species array...")
    (loop for i from 0 to 3 do
        ; array of BoolVar representing if the interval is consonant
        (if (= i 0)
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

    ; ; creating boolean is 2nd or 3rd species array
    ; (print "Creating is 2nd or 3rd species array...")
    ; (setq *is-2nd-or-3rd-species-arr (cbma (nth 2 *is-nth-species-arr) (nth 3 *is-nth-species-arr) gil::BOT_OR))

    ; TODO: new version, check if it works
    ; creating boolean diminution array
    (print "Creating diminution array...")
    ; Note: a diminution is the intermediate note that exists between two notes separated by a jump of a third
    ; i.e. E -> D (dim) -> C
    (setq *is-ta-dim-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    (create-is-ta-dim-arr (second *m-succ-intervals) (collect-by-4 *m2-intervals 1 T) (third *m-succ-intervals) *is-ta-dim-arr)

    ; creating boolean is not cambiata array
    (print "Creating is not cambiata array...")
    (setq *is-not-cambiata-arr (gil::add-bool-var-array *sp* *cf-last-index 0 1))
    ; (create-is-not-cambiata-arr (second *is-cons-arr) (third *is-cons-arr) (first *m-succ-intervals) *is-not-cambiata-arr)
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


    ;======================================== HARMONIC CONSTRAINTS ============================
    (print "Posting constraints...")

    ; perfect consonances should be used at the start and at the end of the piece
    (print "Perfect consonances at the start and at the end...")
    ; if first note is constrained then it must be a perfect consonance
    (add-p-cons-cst-if (first (first *h-intervals)) (first *is-constrained-arr))
    ; if first note is not constrained then the third note must be a perfect consonance
    (add-p-cons-cst-if (third (first *h-intervals)) (first (nth 0 *is-nth-species-arr)))
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
    ; (add-h-cons-cst-if (first *is-cons-arr) (first *is-2nd-or-3rd-species-arr)) ; 2nd + 3rd species
    (add-h-cons-cst-if (first *is-cons-arr) (collect-by-4 (nth 1 *is-nth-species-arr))) ; 1st species
    (add-h-cons-cst-if (first *is-cons-arr) (collect-by-4 (nth 2 *is-nth-species-arr))) ; 2nd species
    (add-h-cons-cst-if (first *is-cons-arr) (first *is-3rd-species-arr)) ; 3rd species
    
    (add-h-cons-cst-if (third *is-cons-arr) (third *is-4th-species-arr)) ; 4th species

    ; five consecutive notes by joint degree implies that the first and the third note are consonants
    (print "Five consecutive notes by joint degree...") ; 3rd species
    ; (add-linked-5qn-cst (first *is-cons-arr) (third *is-cons-arr) *is-5qn-linked-arr)
    ; (add-linked-5qn-cst (third *is-cons-arr) *is-5qn-linked-arr-and-3rd) ; 5th
    (add-linked-5qn-cst (third *is-cons-arr) (cbma *is-5qn-linked-arr *is-mostly-3rd-arr)) ; 3rd

    ; any dissonant note implies that it is surrounded by consonant notes
    (print "Any dissonant note...") ; 3rd species
    ; (add-h-dis-imp-cons-cst
    ;     (first *is-cons-arr)
    ;     (collect-t-or-f-array (second *is-cons-arr) (second *is-3rd-species-arr))
    ;     (third *is-cons-arr)
    ; )
    ; (add-h-dis-imp-cons-cst
    ;     (second *is-cons-arr)
    ;     (collect-t-or-f-array (third *is-cons-arr) (third *is-3rd-species-arr))
    ;     (fourth *is-cons-arr)
    ; )
    ; (add-h-dis-imp-cons-cst
    ;     (third *is-cons-arr)
    ;     (collect-t-or-f-array (fourth *is-cons-arr) (fourth *is-3rd-species-arr))
    ;     (rest (first *is-cons-arr)))
    (add-h-dis-or-cons-3rd-cst
        (second *is-cons-arr)
        (collect-t-or-f-array (third *is-cons-arr) (third *is-3rd-species-arr))
        (fourth *is-cons-arr)
        *is-ta-dim-arr
    )
    ; always consonant if the first note belongs to the 3rd species, necessary ?
    ; (add-h-dis-imp-cons-cst
    ;     (butlast (fourth *is-cons-arr))
    ;     (collect-t-or-f-array (restbutlast (first *is-cons-arr)) (restbutlast (first *is-3rd-species-arr)))
    ;     (rest (second *is-cons-arr)))

    ; no seventh dissonance if the cantus firmus is at the top
    (print "No seventh dissonance if the cantus firmus is at the top...")
    (add-no-seventh-cst (first *h-intervals) (first *is-cf-bass-arr) (first *is-4th-species-arr)) ; 4th species


    ;======================================== MELODIC CONSTRAINTS =============================
    (print "Melodic constraints...")

    ; no melodic interval between 9 and 11
    ; TODO: add rel-reify to all possibilities of melodic intervals by species
    #| (loop for m in *m-succ-intervals do
        (add-no-m-jump-extend-cst m)
    )
    ; (add-no-m-jump-extend-cst (first *m-intervals))
    (add-no-m-jump-extend-cst (fourth *m-intervals)) |#
    (add-no-m-jump-extend-cst *m-all-intervals (cbma (butlast *is-constrained-arr) (rest *is-constrained-arr)))

    ; no *chromatic motion between three consecutive notes
    ; TODO: maybe m2-intervals should be calculated differently
    (print "No chromatic motion...")
    ; (add-no-chromatic-m-cst *m-all-intervals-brut *m2-intervals-brut)

    ; no unisson between two consecutive notes
    ; exept for in the second part or the fourth part of the measure
    (print "No unisson between two consecutive notes...")
    ; if 1st note and 2nd note exists (it means it belongs to a species)
    (add-no-unisson-at-all-cst
        (first *cp) (second *cp)
        (cbma (first *is-cst-arr) (second *is-cst-arr))
    ) ; 5th
    (add-no-unisson-at-all-cst
        (third *cp) (fourth *cp)
        (cbma (third *is-cst-arr) (fourth *is-cst-arr))
    ) ; 5th

    ; melodic intervals between thesis and arsis note from the same measure
    ; can't be greater than a minor sixth expect the octave (just for the fourth species)
    (print "No more than minor sixth melodic interval between arsis and thesis notes...")
    ; only applied if the the second note is not constrained
    (add-no-m-jump-extend-cst *m-ta-intervals (collect-by-4 (nth 0 *is-nth-species-arr) 1)) ; 4th species


    ;======================================== MOTION CONSTRAINTS ============================
    (print "Motion constraints...")

    ; no direct motion to reach a perfect consonance
    ; TODO: just for the fourth species ?
    (print "No direct motion to reach a perfect consonance...")
    (add-no-direct-move-to-p-cons-cst (fourth *motions) (cbma *is-p-cons-arr (fourth *is-3rd-species-arr)) nil) ; 3rd species

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


    ;======================================== COST FACTORS ====================================

    ; IntVar array representing all the cost factors
    (setq *cost-factors (gil::add-int-var-array *sp* *N-COST-FACTORS (* *cf-len -10) (* *cf-len 20)))
    ; IntVar representing the *total *cost
    (setq *total-cost (gil::add-int-var *sp* (* *cf-len -10) (* *cf-len 20)))

    ; 1, 2) imperfect consonances are preferred to perfect consonances
    ;   -> already done in the first species ? Not for now...
    (print "Imperfect consonances are preferred to perfect consonances...")
    ;; TODO replace *fifth-cost and *octave-cost by a single array
    (setq *fifth-cost  (gil::add-int-var-array *sp* *cf-len 0 1)) ; IntVar array representing the cost to have fifths
    (setq *octave-cost (gil::add-int-var-array *sp* *cf-len 0 1)) ; IntVar array representing the cost to have octaves
    (add-cost-cst-if (first *h-intervals) gil::IRT_EQ 7 (first *is-cst-arr) *fifth-cost) ; *fifth-cost = 1 if *h-interval == 7
    (add-cost-cst-if (first *h-intervals) gil::IRT_EQ 0 (first *is-cst-arr) *octave-cost) ; *octave-cost = 1 if *h-interval == 0
    (gil::g-sum *sp* (nth 0 *cost-factors) *fifth-cost) ; sum of the cost of the fifth consonances
    (gil::g-sum *sp* (nth 1 *cost-factors) *octave-cost) ; sum of the cost of the octave consonances

    ; 3, 4, 5) add off-key cost, m-degrees cost and tritons cost
    (set-general-costs-cst *total-cp-len *is-constrained-arr (cbma (butlast *is-constrained-arr) (rest *is-constrained-arr)))
    
    ; 6) contrary motion is preferred
    (gil::g-sum *sp* (nth 5 *cost-factors) (fourth *motions)) ; sum of the cost of the (fourth *motions)
    
    ; TODO) melodic intervals should be as small as possible
    ; IntVar array representing the cost to have melodic intervals > 2
    (setq *gr-one-degree-cost (gil::add-int-var-array *sp* *total-m-len 0 1))
    ; IntVar array representing the cost to have melodic intervals > 3
    (setq *gr-two-degree-cost (gil::add-int-var-array *sp* *total-m-len 0 1))
    (add-cost-cst *m-all-intervals gil::IRT_GR 2 *gr-one-degree-cost)
    (add-cost-cst *m-all-intervals gil::IRT_GR 4 *gr-two-degree-cost)
    ; (gil::g-sum *sp* (nth 0 *cost-factors) *gr-one-degree-cost)
    ; (gil::g-sum *sp* (nth 1 *cost-factors) *gr-two-degree-cost)


    ; 7) cambiata notes are preferred (cons - dis - cons > cons - cons - cons)
    (print "Cambiata notes are preferred...")
    ; IntVar array representing the cost to have cambiata notes
    (setq *not-cambiata-cost (gil::add-int-var-array *sp* *cf-last-index 0 *BIG_COST))
    (add-cost-bool-cst-if *is-not-cambiata-arr *is-mostly-3rd-arr *not-cambiata-cost *BIG_COST_VAR)
    (gil::g-sum *sp* (nth 6 *cost-factors) *not-cambiata-cost) ; sum of the cost of the cambiata notes

    ; 8) intervals between notes n and n+2 are prefered greater than zero
    (print "Intervals between notes n and n+2 are prefered different than zero...")
    ; IntVar array representing the cost to have intervals between notes n and n+2 equal to zero
    (setq *m2-eq-zero-cost (gil::add-int-var-array *sp* *m2-len 0 1))
    (add-cost-cst-if
        *m2-intervals gil::IRT_EQ 0
        (cbma (butlast (butlast *is-constrained-arr)) (rest (rest *is-constrained-arr)))
        *m2-eq-zero-cost
    )
    (gil::g-sum *sp* (nth 7 *cost-factors) *m2-eq-zero-cost) ; sum of the cost of the intervals between notes n and n+2 equal to zero

    ; 9) cancel octave leap cost generated by the previous constraint 4)
    (print "Cancel octave leap cost...")
    ; IntVar array representing the cost to cancel octave leap cost
    (setq *cancel-octave-leap-cost (gil::add-int-var-array *sp* *total-m-len -1 0))
    (add-cost-cst *m-all-intervals gil::IRT_EQ 12 *cancel-octave-leap-cost MINUS_ONE)
    ; (gil::g-sum *sp* (nth 8 *cost-factors) *cancel-octave-leap-cost) ; sum of the cost of the octave leap cost ;TODO TEMP

    ; 10) add no syncopation cost
    (setq *no-syncope-cost (gil::add-int-var-array *sp* *cf-penult-index 0 *big_cost))
    (add-cost-cst-if
        (butlast (third *m-intervals)) gil::IRT_NQ 0
        (third *is-4th-species-arr)
        *no-syncope-cost
        *big_cost_var
    )
    (gil::g-sum *sp* (nth 8 *cost-factors) *no-syncope-cost)


    ;======================================== COST FUNCTION ===================================
    (print "Cost function...")
    ; *total *cost
    (gil::g-sum *sp* *total-cost *cost-factors) ; sum of all the cost factors
    ; (gil::g-sum *sp* *total-cost *gr-one-degree-cost) ; sum of all the cost factors
    (gil::g-cost *sp* *total-cost) ; set the cost function
    
    ; specify a certain solution for testing
    ; (gil::g-rel *sp* (first (first *h-intervals)) gil::IRT_NQ 0) ; TODO TEMP it should be possible with the fourth species
    ; (gil::g-rel *sp* (first (first *h-intervals)) gil::IRT_NQ 7) ; TODO TEMP
    ; (gil::g-rel *sp* (penult (fourth *m-intervals)) gil::IRT_LE 3) ; TODO TEMP
    ; (gil::g-rel *sp* (penult (first *h-intervals)) gil::IRT_EQ 3) ; TODO TEMP
    ; (gil::g-rel *sp* *m-all-intervals gil::IRT_LE 5) ; TODO TEMP
    ; (gil::g-rel *sp* (first *total-cp) gil::IRT_EQ 71) ; TODO TEMP
    
    (loop for i from 0 to 3 do
        (setf (nth i *cons-cost) (gil::add-int-var-array *sp* *cf-last-index 0 1)) ; IntVar representing the cost to have a consonance
        (add-cost-bool-cst (nth i *is-cons-arr) (nth i *cons-cost)) ; *cons-cost = 1 if *is-cons-arr == 1
    )


    ; RETURN
    (if (= species 5)
        ; then create the search engine
        ; (append (fux-search-engine *total-cp) (list species))
        (append (fux-search-engine *total-cp 5) '(5))
        ; else
        nil
    )
)

(defun fux-search-engine (the-cp &optional (species 1))
    (let (se tstop sopts)
        ;; SPECIFY SOLUTION VARIABLES
        (print "Specifying solution variables...")
        (gil::g-specify-sol-variables *sp* the-cp)
        ; (gil::g-specify-percent-diff *sp* 100)
        
        ;; BRANCHING
        (print "Branching...")

        ; (gil::g-branch *sp* *gr-one-degree-cost gil::INT_VAR_RND gil::INT_VAL_MIN)
        ; (gil::g-branch *sp* *gr-two-degree-cost gil::INT_VAR_RND gil::INT_VAL_MIN)
        (if (or (= species 3) (= species 5)) ; TODO really necessary? It seems to be
            (gil::g-branch *sp* *m-degrees-cost gil::INT_VAR_SIZE_MIN gil::INT_VAL_MIN)
        )
        
        (gil::g-branch *sp* *off-key-cost gil::INT_VAR_SIZE_MIN gil::INT_VAL_MIN)
        (if (= species 5) ; otherwise there is no species array
            (gil::g-branch *sp* *species-arr gil::INT_VAR_SIZE_MIN gil::INT_VAL_RND)
        )
        (gil::g-branch *sp* *total-cost gil::INT_VAR_SIZE_MIN gil::INT_VAL_MIN)
        (gil::g-branch *sp* the-cp gil::INT_VAR_SIZE_MIN gil::INT_VAL_RND)

        ; time stop
        (setq tstop (gil::t-stop)); create the time stop object
        (gil::time-stop-init tstop 30000); initialize it (time is expressed in ms)

        ; search options
        (setq sopts (gil::search-opts)); create the search options object
        (gil::init-search-opts sopts); initialize it
        (gil::set-time-stop sopts tstop); set the timestop object to stop the search if it takes too long

        ;; SEARCH ENGINE
        (print "Search engine...")
        (setq se (gil::search-engine *sp* (gil::opts sopts) gil::DFS)); branch and bound search-engine, remove t for dfs
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
        (species (fifth l))
        sol sol-pitches sol-species
        ; rythmic+pitches
        ; rythmic-om
        ; pitches-om
        )

        ; reset the tstop timer before launching the search
        (gil::time-stop-reset tstop)

        ; try to find a solution
        (setq sol (try-find-solution se))

        (if (null sol)
            ; then no more solution
            (progn
                ; (setq *sp* (gil::new-space)) ; reset the CSP
                ; (setq *b-nms t)
                (error "No more solutions")
            )
            ; (setq *b-nms nil)
        )

        ; print the solution from GiL
        (print "Solution: ")
        (case species
            (1 (progn
                (print "PRINT 1st species")
                (print (list "(first *m-intervals-brut)" (gil::g-values sol (first *m-intervals-brut))))
                (print (list "*cf-brut-m-intervals     " (gil::g-values sol *cf-brut-m-intervals)))
                (print (list "(first *motions)       " (gil::g-values sol (first *motions))))
                (print (list "(first *h-intervals)     " (gil::g-values sol (first *h-intervals))))
            ))
            (2 (progn
                (print "PRINT 2nd species")
                (print (list "(first *cp)         " (gil::g-values sol (first *cp))))
                (print (list "(third *cp)         " (gil::g-values sol (third *cp))))
                (print (list "(third *h-intervals)" (gil::g-values sol (third *h-intervals))))
                (print (list "*m-all-intervals" (gil::g-values sol *m-all-intervals)))
                (print (list "*real-motions" (gil::g-values sol *real-motions)))
                (print (list "*penult-thesis-cost" (gil::g-values sol *penult-thesis-cost)))
            ))
            (3 (progn
                (print "PRINT 3rd species")
                (print (list "(first *cp) " (gil::g-values sol (first *cp))))
                (print (list "(second *cp)" (gil::g-values sol (second *cp))))
                (print (list "(third *cp) " (gil::g-values sol (third *cp))))
                (print (list "(fourth *cp)" (gil::g-values sol (fourth *cp))))
                (print (list "*extended-cp-domain" *extended-cp-domain))
                (print (list "(first *h-intervals) " (gil::g-values sol (first *h-intervals))))
                (print (list "(second *h-intervals)" (gil::g-values sol (second *h-intervals))))
                (print (list "(third *h-intervals) " (gil::g-values sol (third *h-intervals))))
                (print (list "(fourth *h-intervals)" (gil::g-values sol (fourth *h-intervals))))
                (print (list "*m-all-intervals" (gil::g-values sol *m-all-intervals)))
                ; (print (list "(fourth *m-intervals-brut)" (gil::g-values sol (fourth *m-intervals-brut))))
                ; (print (list "(first *motions) " (gil::g-values sol (first *motions))))
                (print (list "(fourth *motions)" (gil::g-values sol (fourth *motions))))
                (print (list "*not-cambiata-cost " (gil::g-values sol *not-cambiata-cost)))
                (print (list "*m2-eq-zero-cost   " (gil::g-values sol *m2-eq-zero-cost)))
                (print (list "*cancel-octave-cost" (gil::g-values sol *cancel-octave-leap-cost)))
                ; (print (list "(first *cons-cost)  " (gil::g-values sol (first *cons-cost))))
                ; (print (list "(second *cons-cost) " (gil::g-values sol (second *cons-cost))))
                ; (print (list "(third *cons-cost)  " (gil::g-values sol (third *cons-cost))))
                ; (print (list "(fourth *cons-cost) " (gil::g-values sol (fourth *cons-cost))))
            ))
            (4 (progn
                (print "PRINT 4th species")
                (print (list "(first *cp)         " (gil::g-values sol (first *cp))))
                (print (list "(third *cp)         " (gil::g-values sol (third *cp))))
                (print (list "(first *h-intervals)" (gil::g-values sol (first *h-intervals))))
                (print (list "(third *h-intervals)" (gil::g-values sol (third *h-intervals))))
                (print (list "*m-all-intervals         " (gil::g-values sol *m-all-intervals)))
                (print (list "(third *m-intervals)     " (gil::g-values sol (third *m-intervals))))
                (print (list "(first *m-succ-intervals) " (gil::g-values sol (first *m-succ-intervals))))
                (print (list "*no-syncope-cost" (gil::g-values sol *no-syncope-cost)))
                (print (list "*m2-eq-zero-cost" (gil::g-values sol *m2-eq-zero-cost)))
                
            ))
            (5 (progn
                (print "PRINT 5th species")
                (print (list "(first *cp) " (gil::g-values sol (first *cp))))
                (print (list "(second *cp)" (gil::g-values sol (second *cp))))
                (print (list "(third *cp) " (gil::g-values sol (third *cp))))
                (print (list "(fourth *cp)" (gil::g-values sol (fourth *cp))))
                (print (list "(first *h-intervals) " (gil::g-values sol (first *h-intervals))))
                (print (list "(second *h-intervals)" (gil::g-values sol (second *h-intervals))))
                (print (list "(third *h-intervals) " (gil::g-values sol (third *h-intervals))))
                (print (list "(fourth *h-intervals)" (gil::g-values sol (fourth *h-intervals))))
                (print (list "*m-all-intervals" (gil::g-values sol *m-all-intervals)))
                ; (print (list "(fourth *m-intervals-brut)" (gil::g-values sol (fourth *m-intervals-brut))))
                ; (print (list "(first *motions) " (gil::g-values sol (first *motions))))
                (print (list "(fourth *motions)" (gil::g-values sol (fourth *motions))))
                (print (list "*not-cambiata-cost " (gil::g-values sol *not-cambiata-cost)))
                (print (list "*m2-eq-zero-cost   " (gil::g-values sol *m2-eq-zero-cost)))
                (print (list "*cancel-octave-cost" (gil::g-values sol *cancel-octave-leap-cost)))
                ; (print (list "(first *cons-cost)  " (gil::g-values sol (first *cons-cost))))
                (print (list "(second *cons-cost) " (gil::g-values sol (second *cons-cost))))
                (print (list "(third *cons-cost)  " (gil::g-values sol (third *cons-cost))))
                (print (list "(fourth *cons-cost) " (gil::g-values sol (fourth *cons-cost))))
                (print (list "*species-arr" sol-species))
                (print (list "*sp-arr1" (gil::g-values sol (first *sp-arr))))
                (print (list "*sp-arr2" (gil::g-values sol (second *sp-arr))))
                (print (list "*sp-arr3" (gil::g-values sol (third *sp-arr))))
                (print (list "*sp-arr4" (gil::g-values sol (fourth *sp-arr))))
            ))
        )
        ; (print (list "*is-p-cons-arr" (gil::g-values sol *is-p-cons-arr)))
        (print (list "*gr-one-degree-cost" (gil::g-values sol *gr-one-degree-cost)))
        (print (list "*gr-two-degree-cost" (gil::g-values sol *gr-two-degree-cost)))
        (print (list "*m-degrees-cost    " (gil::g-values sol *m-degrees-cost)))
        (print (list "*off-key-cost     " (gil::g-values sol *off-key-cost)))
        (print (list "*fifth-cost  " (gil::g-values sol *fifth-cost)))
        (print (list "*octave-cost " (gil::g-values sol *octave-cost)))
        (print (list "*tritone-cost " (gil::g-values sol *tritone-cost)))
        (print (list "*cost-factors" (gil::g-values sol *cost-factors)))
        (print (list "### COST ### " (gil::g-values sol *total-cost)))
        #| (print (list "scale         " *scale))
        (print (list "borrowed-scale" *borrowed-scale))
        (print (list "off-scale     " (reverse *off-scale))) |#
        (setq sol-pitches (gil::g-values sol the-cp)) ; store the values of the solution
        (print sol-pitches)
        (case species
            (4 (progn
                (setq rythmic+pitches (get-basic-rythmic 4 *cf-len sol-pitches)) ; get the rythmic correpsonding to the species
                (setq rythmic-om (first rythmic+pitches))
                (setq pitches-om (second rythmic+pitches))
                (print (list "rythmic-om" rythmic-om))
                (print (list "pitches-om" pitches-om))
                ; (list (to-midicent pitches-om) (om::mktree rythmic-om '(4 4)))
            ))
            (5 (progn
                (setq sol-species (gil::g-values sol *species-arr)) ; store the values of the solution
                (setq rythmic+pitches (parse-species-to-om-rythmic sol-species sol-pitches))
                (setq rythmic-om (first rythmic+pitches))
                (setq pitches-om (second rythmic+pitches))
                (if (not (null *prev-sol-pitches))
                    ; then compare the pitches of the previous solution with the current one
                    ; if they are the same launch a new search
                    (if (equal *prev-sol-pitches rythmic+pitches)
                        (progn
                            (print "SAME PITCHES")
                            (print (list "rythmic-om" rythmic-om))
                            (print (list "pitches-om" pitches-om))
                            ; (setq *prev-sol-pitches rythmic+pitches)
                            (search-next-fux-cp l)
                        )
                        (progn
                            (print "KEEP PITCHES")
                            (print (list "rythmic-om" rythmic-om))
                            (print (list "pitches-om" pitches-om))
                            (setq *prev-sol-pitches rythmic+pitches)
                            ; (list (to-midicent pitches-om) (om::mktree rythmic-om '(4 4)))
                        )
                    )
                    ; else register the pitches of the current solution
                    (progn
                        (print "FIRST PITCHES")
                        (setq *prev-sol-pitches rythmic+pitches)
                    )
                )
            ))
            (otherwise (progn
                (setq rythmic-om (get-basic-rythmic species *cf-len)) ; get the rythmic correpsonding to the species
                (setq pitches-om sol-pitches)
                ; (list (to-midicent sol-pitches) (om::mktree rythmic-om '(4 4)))
            ))
        )
        ; (print "BEFORE RETURN")
        ; (print (list "pitches-om" pitches-om))
        ; (list (to-midicent sol-pitches) (om::mktree rythmic-om '(4 4)))
        (make-instance 'voice :chords (to-midicent sol-pitches) :tree (om::mktree rythmic-om '(4 4)) :tempo *cf-tempo)
    )
)

(defun try-find-solution (se)
    (handler-case
        (gil::search-next se) ; search the next solution, sol is the space of the solution
        (error (c)
            (print "gil::ERROR")
            (try-find-solution se)
        )
    )
)




















;================================================ CP CONSTRAINTS UTILS ============================

; tibo 
; add a single cost regarding if the relation rel-type(tested, cst-val) is true
(defun add-single-cost-cst (tested rel-type cst-val cost &optional (cost-value ONE))
    (let (
        (b (gil::add-bool-var *sp* 0 1)) ; to store the result of the test
    )
        (gil::g-rel-reify *sp* tested rel-type cst-val b) ; test the relation
        (gil::g-ite *sp* b cost-value ZERO cost) ; add the cost if the test is true
    )
)

; tibo
; add a cost regarding if the relation rel-type(tested-var, cst-val) is true
(defun add-cost-cst (tested-var-arr rel-type cst-val costs &optional (cost-value ONE))
    (loop
        for cost in costs
        for tested in tested-var-arr
        do
            (add-single-cost-cst tested rel-type cst-val cost cost-value)
    )
)

; tibo
; add a cost regarding if the relation rel-type(tested-var, cst-val) is true
; NOTE: the difference with add-cost-cst is that the cst-val is an array 
(defun add-cost-multi-cst (tested-var-arr rel-type cst-val-arr costs &optional (cost-value ONE))
    (loop
        for cost in costs
        for tested in tested-var-arr
        for cst-val in cst-val-arr
        do
            (add-single-cost-cst tested rel-type cst-val cost cost-value)
    )
)

; tibo
; add a cost regarding if the relation rel-type(tested-var, cst-val) is true AND is-cst is true
(defun add-cost-cst-if (tested-var-arr rel-type cst-val is-cst-arr costs &optional (cost-value ONE))
    (loop
        for cost in costs
        for tested in tested-var-arr
        for cst in is-cst-arr
        do
            (let (
                (b (gil::add-bool-var *sp* 0 1)) ; to store the result of the test
                (b-and (gil::add-bool-var *sp* 0 1)) ; b and cst
            )
                (gil::g-rel-reify *sp* tested rel-type cst-val b)
                (gil::g-op *sp* b gil::BOT_AND cst b-and) ; b-and = b and cst
                (gil::g-ite *sp* b-and cost-value ZERO cost) ; add the cost if the test is true
            )
    )
)

(defun add-cost-bool-cst (bool-arr costs &optional (cost-value ONE))
    (loop
        for b in bool-arr
        for cost in costs
        do
            (gil::g-ite *sp* b cost-value ZERO cost)
    )
)

(defun add-cost-bool-cst-if (bool-arr is-cst-arr costs &optional (cost-value ONE))
    (loop
        for b in bool-arr
        for cst in is-cst-arr
        for cost in costs
        do
            (let (
                (b-and (gil::add-bool-var *sp* 0 1)) ; b and cst
            )
                (gil::g-op *sp* b gil::BOT_AND cst b-and) ; b-and = b and cst
                (gil::g-ite *sp* b-and cost-value ZERO cost) ; add the cost if the test is true
            )
    )
)

; add constraints such that costs =
;   - 0 if m-degree in [0, 1, 2]
;   - 1 if m-degree in [3, 4, 12]
;   - 2 otherwise
; @m-all-intervals: all the melodic intervals of cp in a row
; @m-degrees-cost: the cost of each melodic interval
(defun add-m-degrees-cost-cst (m-all-intervals m-degrees-cost &optional (is-cst-arr nil))
    (loop
    for m in m-all-intervals
    for c in m-degrees-cost
    for i from 0 below (length m-all-intervals)
    do
        (let (
            ; (b1 (gil::add-bool-var *sp* 0 1)) ; true if m-degree in [3, 4]
            ; (b2 (gil::add-bool-var *sp* 0 1)) ; true if m-degree > 4
            ; (b1-not (gil::add-bool-var *sp* 0 1))
            ; (b-xor (gil::add-bool-var *sp* 0 1))
            (b-l3 (gil::add-bool-var *sp* 0 1)) ; true if m-degree < 3 -> then cost = 0
            (b-3 (gil::add-bool-var *sp* 0 1)) ; true if m-degree is 3
            (b-4 (gil::add-bool-var *sp* 0 1)) ; true if m-degree is 4
            (b-12 (gil::add-bool-var *sp* 0 1)) ; true if m-degree is 12
            (b-34 (gil::add-bool-var *sp* 0 1)) ; true if m-degree in [3, 4]
            (b-3412 (gil::add-bool-var *sp* 0 1)) ; true if m-degree in [3, 4, 12] -> then cost = 1
            (b-or (gil::add-bool-var *sp* 0 1)) ; true if b-3412 OR b-l3
            (b-or-not (gil::add-bool-var *sp* 0 1)) ; true if NOT b-or -> then cost = 2
            (is-cst (true-if-null is-cst-arr i)) ; true if both notes are constrained by a species
        )
            #| (gil::g-rel-reify *sp* m gil::IRT_GR 2 b1)
            (gil::g-rel-reify *sp* m gil::IRT_GR 4 b2)
            (gil::g-rel-reify *sp* m gil::IRT_LQ 2 b1-not)
            (gil::g-op *sp* b1 gil::BOT_XOR b2 b-xor) ; means that just b1 is true because b2 cannot be true unless b1 is also true
            (gil::g-rel-reify *sp* c gil::IRT_EQ 0 b1-not)
            (gil::g-rel-reify *sp* c gil::IRT_EQ 1 b-xor)
            (gil::g-rel-reify *sp* c gil::IRT_EQ 2 b2) |#
            (gil::g-rel-reify *sp* m gil::IRT_LQ 2 b-l3) ; m-degree < 3
            (gil::g-rel-reify *sp* m gil::IRT_EQ 3 b-3) ; m-degree = 3
            (gil::g-rel-reify *sp* m gil::IRT_EQ 4 b-4) ; m-degree = 4
            (gil::g-rel-reify *sp* m gil::IRT_EQ 12 b-12) ; m-degree = 12
            (gil::g-op *sp* b-3 gil::BOT_OR b-4 b-34) ; b-34 = true if m-degree in [3, 4]
            (gil::g-op *sp* b-34 gil::BOT_OR b-12 b-3412) ; b-3412 = true if m-degree in [3, 4, 12]
            (gil::g-op *sp* b-3412 gil::BOT_OR b-l3 b-or) ; b-or = true if m-degree in [3, 4, 12] OR m-degree < 3
            (gil::g-op *sp* b-or gil::BOT_EQV FALSE b-or-not) ; b-or-not = true if NOT b-or
            (gil::g-rel-reify *sp* c gil::IRT_EQ 0 b-l3) ; cost = 0 if m-degree < 3
            (gil::g-rel-reify *sp* c gil::IRT_EQ 1 b-3412) ; cost = 1 if m-degree in [3, 4, 12]
            (gil::g-rel-reify *sp* c gil::IRT_EQ 2 b-or-not) ; cost = 2 otherwise
        )
    )
)

(defun add-p-cons-cost-cst (&optional (len *cf-len))
    (setq *fifth-cost  (gil::add-int-var-array *sp* len 0 1)) ; IntVar array representing the cost to have fifths
    (setq *octave-cost (gil::add-int-var-array *sp* len 0 1)) ; IntVar array representing the cost to have octaves
    (if (= len *cf-len)
        ; then
        (progn
        (add-cost-cst (first *h-intervals) gil::IRT_EQ 7 *fifth-cost) ; *fifth-cost = 1 if *h-interval == 7
        (add-cost-cst (first *h-intervals) gil::IRT_EQ 0 *octave-cost) ; *octave-cost = 1 if *h-interval == 0
        )
        ; else
        (progn
        (add-cost-cst (third *h-intervals) gil::IRT_EQ 7 *fifth-cost) ; *fifth-cost = 1 if *h-interval == 7
        (add-cost-cst (third *h-intervals) gil::IRT_EQ 0 *octave-cost) ; *octave-cost = 1 if *h-interval == 0
        )
    )
    (gil::g-sum *sp* (nth 0 *cost-factors) *fifth-cost) ; sum of the cost of the fifth consonances
    (gil::g-sum *sp* (nth 1 *cost-factors) *octave-cost) ; sum of the cost of the octave consonances
)

; add general costs for most of the species
(defun set-general-costs-cst (&optional (cp-len *total-cp-len) (is-cst-arr1 nil) (is-cst-arr2 nil))
    (let (
        (m-len (- cp-len 1))
    )
        ; 2) sharps and flats should be used sparingly
        (print "Sharps and flats should be used sparingly...")
        (setq *off-key-cost (gil::add-int-var-array *sp* cp-len 0 2)) ; IntVar array representing the cost to have off-key notes
        (if (null is-cst-arr1)
            ; then
            (add-cost-bool-cst *is-cp-off-key-arr *off-key-cost TWO)
            ; else
            (add-cost-bool-cst-if *is-cp-off-key-arr is-cst-arr1 *off-key-cost TWO)
        )
        (gil::g-sum *sp* (nth 2 *cost-factors) *off-key-cost) ; sum of the cost of the off-key notes

        ; 3) melodic intervals should be as small as possible
        (print "Melodic intervals should be as small as possible...")
        ; IntVar array representing the cost to have melodic large intervals
        (setq *m-degrees-cost (gil::add-int-var-array *sp* m-len 0 2))
        (add-m-degrees-cost-cst *m-all-intervals *m-degrees-cost is-cst-arr2)
        (gil::g-sum *sp* (nth 3 *cost-factors) *m-degrees-cost)

        ; 4) melodic *tritone intervals are avoided if possible
        (print "Melodic tritone intervals are avoided...")
        (setq *tritone-cost (gil::add-int-var-array *sp* m-len 0 1)) ; IntVar array representing the cost to have tritons
        (if (null is-cst-arr2)
            ; then
            (add-cost-cst *m-all-intervals gil::IRT_EQ 6 *tritone-cost)
            ; else
            (add-cost-cst-if *m-all-intervals gil::IRT_EQ 6 is-cst-arr2 *tritone-cost)
        )
        (gil::g-sum *sp* (nth 4 *cost-factors) *tritone-cost) ; sum of the cost of the tritons
    )
)

; merge lists intermittently such that the first element of the first list is followed by the first element of the second list, etc.
; attention: cp-len is lenght of the first list in cp-list and it should be 1 more than the lenght of the other lists
(defun merge-cp (cp-list total-cp)
    (let (
        (cp-len-1 (- (length (first cp-list)) 1))
        (n-list (length cp-list))
        ; (cp-last-index (- (length total-cp) 1))
    )
        ; (print (list "cp-list" cp-list))
        (loop
        for i from 0 below cp-len-1
        do
            (loop for j from 0 below n-list do
                ; (gil::g-rel *sp* (nth (+ (* i n-list) j) total-cp) gil::IRT_EQ (nth i (nth j cp-list)))
                (setf (nth (+ (* i n-list) j) total-cp) (nth i (nth j cp-list)))
            )
        )
        (gil::g-rel *sp* (lastone total-cp) gil::IRT_EQ (lastone (first cp-list)))
    )
)

; merge lists intermittently such that the first element of the first list is followed by the first element of the second list, etc.
; attention: lengths should be the same
(defun merge-cp-same-len (cp-list total-cp)
    (let (
        (cp-len (length (first cp-list)))
        (n-list (length cp-list))
        ; (cp-last-index (- (length total-cp) 1))
    )
        ; (print (list "cp-list" cp-list))
        (loop
        for i from 0 below cp-len
        do
            (loop for j from 0 below n-list do
                ; (gil::g-rel *sp* (nth (+ (* i n-list) j) total-cp) gil::IRT_EQ (nth i (nth j cp-list)))
                (setf (nth (+ (* i n-list) j) total-cp) (nth i (nth j cp-list)))
            )
        )
        ; (gil::g-rel *sp* (lastone total-cp) gil::IRT_EQ (lastone (first cp-list)))
    )
)

(defun create-h-intervals (cp cf h-intervals)
    ; (print (list "Creating h-intervals..." cp cf))
    (loop
        for p in cp
        for q in cf
        for i in h-intervals do
            ; (print (list "cp=" p))
            ; (print (list "cf=" q))
            ; (print (list "i =" i))
            (inter-eq-cst *sp* p q i) ; add a constraint to *sp* such that i = |p - q| % 12
    )
)

(defun create-intervals (line1 line2 intervals brut-intervals)
    (loop
        for p in line1
        for q in line2
        for i in intervals
        for ib in brut-intervals do
            (inter-eq-cst-brut *sp* q p ib i) ; add a constraint to *sp* such that ib = p - q and i = |ib|
    )
)

(defun create-m-intervals-self (cp m-intervals m-intervals-brut)
    ;; (loop
    ;;     for p in (butlast cp)
    ;;     for q in (rest cp)
    ;;     for i in m-intervals
    ;;     for ib in m-intervals-brut do
    ;;         (inter-eq-cst-brut *sp* q p ib i) ; add a constraint to *sp* such that ib = p - q and i = |ib|
    ;; )
    (create-intervals (butlast cp) (rest cp) m-intervals m-intervals-brut)
)

; create an array of IntVar with the melodic interval between each arsis and its following thesis
(defun create-m-intervals-next-meas (cp-arsis cp m-intervals-arsis m-intervals-arsis-brut)
    ;; (loop
    ;;     for p in cp-arsis
    ;;     for q in (rest cp)
    ;;     for i in m-intervals-arsis
    ;;     for ib in m-intervals-arsis-brut do
    ;;         (inter-eq-cst-brut *sp* q p ib i) ; add a constraint to *sp* such that ib = p - q and i = |ib|
    ;; )
    (create-intervals cp-arsis (rest cp) m-intervals-arsis m-intervals-arsis-brut)
)

(defun create-m2-intervals (cp m2-intervals m2-intervals-brut)
    ;; (loop
    ;;     for p in (butlast (butlast cp))
    ;;     for q in (rest (rest cp))
    ;;     for i in m2-intervals
    ;;     for ib in m2-intervals-brut do
    ;;         (inter-eq-cst-brut *sp* q p ib i) ; add a constraint to *sp* such that ib = p - q and i = |ib|
    ;; )
    (create-intervals (butlast (butlast cp)) (rest (rest cp)) m2-intervals m2-intervals-brut)
)

(defun create-m-intervals-in-meas (cp cp-arsis ta-intervals ta-intervals-brut)
    (create-intervals (butlast cp) cp-arsis ta-intervals ta-intervals-brut)
)


; (defun create-m2-intervals (cp cp-arsis m2-intervals m2-intervals-brut))

(defun create-cf-brut-m-intervals (cf cf-brut-m-intervals)
    (loop
        for p in (butlast cf)
        for q in (rest cf)
        for i in cf-brut-m-intervals do
            (let (
                (ib (inter q p t))
            )
                (gil::g-rel *sp* i gil::IRT_EQ ib)
            )
    )
)

(defun create-is-p-cons-arr (h-intervals is-p-cons-arr)
    (loop
        for i in h-intervals
        for p in is-p-cons-arr
        do
            (let (
                (b-7 (gil::add-bool-var *sp* 0 1))
                (b-0 (gil::add-bool-var *sp* 0 1))
            )
                (gil::g-rel-reify *sp* i gil::IRT_EQ 7 b-7) ; b-7 = (i == 7) -> the interval is a fifth
                (gil::g-rel-reify *sp* i gil::IRT_EQ 0 b-0) ; b-0 = (i == 0) -> the interval is an octave
                (gil::g-op *sp* b-0 gil::BOT_OR b-7 p) ; p = b-7 || b-0
            )
    )
)

;; (defun create-cf-rel-pitch (cp cf cf-rel-pitch is-cf-bass-arr)
;;     (loop
;;         for p in cp
;;         for q in cf
;;         for o in cf-rel-pitch
;;         for b in is-cf-bass-arr
;;         do
;;             (gil::g-rel-reify *sp* p gil::IRT_GQ q b) ; b = (p >= q)
;;             (gil::g-ite *sp* b ZERO ONE o) ; o = (b ? 0 : 1)
;;     )
;; )

(defun create-is-cf-bass-arr (cp cf is-cf-bass-arr)
    (loop
        for p in cp
        for q in cf
        for b in is-cf-bass-arr
        do
            (gil::g-rel-reify *sp* p gil::IRT_GQ q b) ; b = (p >= q)
    )
)

; create an array of BoolVar such that is-ta-dim-arr is true if the note is a diminution:
; 1 -> inter(thesis, arsis) == 1 or 2 && inter(thesis, thesis + 1) == 3 or 4 && inter(arsis, thesis + 1) == 1 or 2
; @m-intervals-ta: the melodic interval between each thesis and its following arsis
; @m-intervals: the melodic interval between each thesis and its following thesis
; @m-intervals-arsis: the melodic interval between each arsis and its following thesis
; @is-ta-dim-arr: the array of BoolVar to fill
(defun create-is-ta-dim-arr (m-intervals-ta m-intervals m-intervals-arsis is-ta-dim-arr)
    (loop
        for mta in m-intervals-ta ; inter(thesis, arsis)
        for mtt in m-intervals ; inter(thesis, thesis + 1)
        for mat in m-intervals-arsis ; inter(arsis, thesis + 1)
        for b in is-ta-dim-arr ; the BoolVar to create
        do
            (let (
                (btt3 (gil::add-bool-var *sp* 0 1)) ; for mtt == 3
                (btt4 (gil::add-bool-var *sp* 0 1)) ; for mtt == 4
                ; (bta1 (gil::add-bool-var *sp* 0 1))
                ; (bta2 (gil::add-bool-var *sp* 0 1))
                ; (bat1 (gil::add-bool-var *sp* 0 1))
                ; (bat2 (gil::add-bool-var *sp* 0 1))
                (bta-second (gil::add-bool-var *sp* 0 1)) ; for mat <= 2
                (btt-third (gil::add-bool-var *sp* 0 1)) ; for mtt == 3 or 4
                (bat-second (gil::add-bool-var *sp* 0 1)) ; for mta <= 2
                (b-and (gil::add-bool-var *sp* 0 1)) ; temporary BoolVar
            )
                (gil::g-rel-reify *sp* mtt gil::IRT_EQ 3 btt3) ; btt3 = (mtt == 3)
                (gil::g-rel-reify *sp* mtt gil::IRT_EQ 4 btt4) ; btt4 = (mtt == 4)
                ; (gil::g-rel-reify *sp* mta gil::IRT_EQ 1 bta1) ; bta1 = (mta == 1)
                ; (gil::g-rel-reify *sp* mta gil::IRT_EQ 2 bta2) ; bta2 = (mta == 2)
                (gil::g-rel-reify *sp* mta gil::IRT_LQ 2 bta-second) ; bta2 = (mta <= 2)
                ; (gil::g-rel-reify *sp* mat gil::IRT_EQ 1 bat1) ; bat1 = (mat == 1)
                ; (gil::g-rel-reify *sp* mat gil::IRT_EQ 2 bat2) ; bat2 = (mat == 2)
                (gil::g-rel-reify *sp* mat gil::IRT_LQ 2 bat-second) ; bat1 = (mat <= 2)
                ; (gil::g-op *sp* bta1 gil::BOT_OR bta2 bta-second) ; bta-second = bta1 || bta2
                (gil::g-op *sp* btt3 gil::BOT_OR btt4 btt-third) ; btt-third = btt3 || btt4
                ; (gil::g-op *sp* bat1 gil::BOT_OR bat2 bat-second) ; bat-second = bat1 || bat2
                (gil::g-op *sp* bta-second gil::BOT_AND btt-third b-and) ; temporay operation
                (gil::g-op *sp* b-and gil::BOT_AND bat-second b) ; b = bta-second && btt-third && bat-second
            )
    )
)

; create an array of BoolVar
; 1 -> inter(cp, cf) <= 4 && cf getting closer to cp
(defun create-is-nbour-arr (h-intervals-abs is-cf-bass-arr cf-brut-m-intervals is-nbour-arr)
    (loop
        for hi in (butlast h-intervals-abs)
        for bass in (butlast is-cf-bass-arr)
        for mi in cf-brut-m-intervals
        for n in is-nbour-arr
        do
            (let (
                (b-hi (gil::add-bool-var *sp* 0 1)) ; for (hi <= 4)
                (b-cfu (gil::add-bool-var *sp* 0 1)) ; for cf going up
                (b-cfgc (gil::add-bool-var *sp* 0 1)) ; for cf getting closer to cp
            )
                (gil::g-rel-reify *sp* hi gil::IRT_LQ 4 b-hi) ; b-hi = (hi <= 4)
                (gil::g-rel-reify *sp* mi gil::IRT_GQ 0 b-cfu) ; b-cfu = (mi >= 0)
                (gil::g-op *sp* bass gil::BOT_EQV b-cfu b-cfgc) ; b-cfgc = (bass == b-cfu)
                (gil::g-op *sp* b-hi gil::BOT_AND b-cfgc n) ; n = b-hi && b-cfgc
            )
    )
)

; TODO: new version below should be used instead of this one
; create an array of BoolVar
; 1 -> 5 quarter notes strictly ups or downs and are linked by joint degrees
; Note: the rule is applied measure by measure
(defun create-is-5qn-linked-arr (m-all-intervals m-all-intervals-brut is-5qn-linked-arr)
    (loop
    for i from 0 to (- (length m-all-intervals) 3)
    for m1 in m-all-intervals
    for m2 in (rest m-all-intervals)
    for m3 in (rest (rest m-all-intervals))
    for m4 in (rest (rest (rest m-all-intervals)))
    for mb1 in m-all-intervals-brut
    for mb2 in (rest m-all-intervals-brut)
    for mb3 in (rest (rest m-all-intervals-brut))
    for mb4 in (rest (rest (rest m-all-intervals-brut)))
    for b in is-5qn-linked-arr
    do
        (if (= (mod i 4) 0)
            ; then
            (let (
                (b1 (gil::add-bool-var *sp* 0 1)) ; (m1 <= 2)
                (b2 (gil::add-bool-var *sp* 0 1)) ; (m2 <= 2)
                (b3 (gil::add-bool-var *sp* 0 1)) ; (m3 <= 2)
                (b4 (gil::add-bool-var *sp* 0 1)) ; (m4 <= 2)
                (bb1 (gil::add-bool-var *sp* 0 1)) ; (mb1 > 0)
                (bb2 (gil::add-bool-var *sp* 0 1)) ; (mb2 > 0)
                (bb3 (gil::add-bool-var *sp* 0 1)) ; (mb3 > 0)
                (bb4 (gil::add-bool-var *sp* 0 1)) ; (mb4 > 0)
                (b-and1 (gil::add-bool-var *sp* 0 1)) ; (b1 && b2)
                (b-and2 (gil::add-bool-var *sp* 0 1)) ; (b3 && b4)
                (b-and3 (gil::add-bool-var *sp* 0 1)) ; (b-and1 && b-and2)
                (b-eq1 (gil::add-bool-var *sp* 0 1)) ; (mb1 == mb2)
                (b-eq2 (gil::add-bool-var *sp* 0 1)) ; (mb3 == mb3)
                (b-eq3 (gil::add-bool-var *sp* 0 1)) ; (b-eq1 == b-eq2)
            )
                (gil::g-rel-reify *sp* m1 gil::IRT_LQ 2 b1) ; b1 = (m1 <= 2)
                (gil::g-rel-reify *sp* m2 gil::IRT_LQ 2 b2) ; b2 = (m2 <= 2)
                (gil::g-rel-reify *sp* m3 gil::IRT_LQ 2 b3) ; b3 = (m3 <= 2)
                (gil::g-rel-reify *sp* m4 gil::IRT_LQ 2 b4) ; b4 = (m4 <= 2)
                (gil::g-rel-reify *sp* mb1 gil::IRT_GQ 0 bb1) ; bb1 = (mb1 > 0)
                (gil::g-rel-reify *sp* mb2 gil::IRT_GQ 0 bb2) ; bb2 = (mb2 > 0)
                (gil::g-rel-reify *sp* mb3 gil::IRT_GQ 0 bb3) ; bb3 = (mb3 > 0)
                (gil::g-rel-reify *sp* mb4 gil::IRT_GQ 0 bb4) ; bb4 = (mb4 > 0)
                (gil::g-op *sp* b1 gil::BOT_AND b2 b-and1) ; b-and1 = b1 && b2
                (gil::g-op *sp* b3 gil::BOT_AND b4 b-and2) ; b-and2 = b3 && b4
                (gil::g-op *sp* b-and1 gil::BOT_AND b-and2 b-and3) ; b-and3 = b-and1 && b-and2
                (gil::g-op *sp* bb1 gil::BOT_EQV bb2 b-eq1) ; b-eq1 = (bb1 == bb2)
                (gil::g-op *sp* bb3 gil::BOT_EQV bb4 b-eq2) ; b-eq2 = (bb3 == bb4)
                (gil::g-op *sp* b-eq1 gil::BOT_EQV b-eq2 b-eq3) ; b-eq3 = (b-eq1 == b-eq2)
                (gil::g-op *sp* b-and3 gil::BOT_AND b-eq3 b) ; b = b-and3 && b-eq3
            )
        )
    )
)

; create an array of BoolVar representing if the second note is not cambiata
(defun create-is-not-cambiata-arr (is-cons-arr2 is-cons-arr3 m-intervals is-not-cambiata-arr)
    (loop
    for b2 in is-cons-arr2
    for b3 in is-cons-arr3
    for m in m-intervals
    for b in is-not-cambiata-arr
    do
        (let (
            (b-m (gil::add-bool-var *sp* 0 1)) ; (m <= 2)
            (b-and (gil::add-bool-var *sp* 0 1)) ; (b2 && b3)
        )
            (gil::g-op *sp* b2 gil::BOT_AND b3 b-and) ; b-and = b2 && b3
            ; (gil::g-rel-reify *sp* m gil::IRT_LQ 4 b-m) ; b-m = (m <= 4)
            (gil::g-rel-reify *sp* m gil::IRT_LQ 2 b-m) ; b-m = (m <= 2)
            (gil::g-op *sp* b-and gil::BOT_AND b-m b) ; b = b-and && b-m
        )
    )
)

(defun add-is-member-cst (candidate member-list b-member)
    (let (
        (results (gil::add-int-var-array *sp* (length member-list) 0 1)) ; where candidate == m
        (sum (gil::add-int-var *sp* 0 (length member-list))) ; sum(results)
    )
        (loop
        for m in member-list
        for r in results
        do
            (let (
                (b1 (gil::add-bool-var *sp* 0 1)) ; b1 = (candidate == m)
            )
                (gil::g-rel-reify *sp* candidate gil::IRT_EQ m b1) ; b1 = (candidate == m)
                (gil::g-ite *sp* b1 ONE ZERO r) ; r = (b1 ? 1 : 0)
            )
        )
        (gil::g-sum *sp* sum results) ; sum = sum(results)
        (gil::g-rel-reify *sp* sum gil::IRT_GR 0 b-member) ; b-member = (sum >= 1)
    )
)

; create an array of BoolVar
; 1 -> the harmonic interval is member of the set (consonances set by default)
(defun create-is-member-arr (h-intervals cons-arr &optional (cons-set ALL_CONS))
    (loop
    for h in h-intervals
    for b in cons-arr
    do
        (add-is-member-cst h cons-set b)
    )
)

; attention: cf-penult-index is the index of penultimate note in the counterpoint
(defun add-h-cons-cst (len cf-penult-index h-intervals &optional (penult-dom-var PENULT_CONS_VAR))
    (loop for i from 0 below len do
        (if (= i cf-penult-index) ; if it is the penultimate note
            ; then add major sixth + minor third by default
            (gil::g-member *sp* penult-dom-var (nth i h-intervals))
            ; else add all consonances
            (if (not (null (nth i h-intervals)))
                (gil::g-member *sp* ALL_CONS_VAR (nth i h-intervals))
            )
        )
    )
)

; add the constraint such that is-cst-arr[i] => is-cons-arr[i] is true
; -is-cons-arr: array of BoolVar, 1 -> the harmonic interval is a consonance
; -is-cst-arr: array of BoolVar, 1 -> the note is constrained by a species
(defun add-h-cons-cst-if (is-cons-arr is-cst-arr)
    (loop
    for is-cons in is-cons-arr
    for is-cst in is-cst-arr
    do
        (gil::g-op *sp* is-cst gil::BOT_IMP is-cons 1) ; (is-cst => is-cons) = 1
    )
)

; TODO for future work: should use not(nth i is-cons-arr) instead of add a constraint for each dissonance in DIS
; -len: length of the harmonic array
; -cf-penult-index: index of the penultimate note in the counterpoint
; -h-intervals-arsis: harmonic intervals of the arsis of the counterpoint
; -is-ta-dim-arr: array of BoolVar, 1 -> the note in arsis is a diminution
; -penult-dom-var: domain of the penultimate note
(defun add-h-cons-arsis-cst (len cf-penult-index h-intervals-arsis is-ta-dim-arr &optional (penult-dom-var PENULT_CONS_VAR))
    (loop
    for i from 0 below len
    for b in is-ta-dim-arr
    do
        (if (= i cf-penult-index) ; if it is the penultimate note
            ; then add major sixth + minor third
            (gil::g-member *sp* penult-dom-var (nth i h-intervals-arsis))
            ; else dissonance implies there is a diminution
            (loop for d in DIS do
                (gil::g-rel-reify *sp* (nth i h-intervals-arsis) gil::IRT_EQ d b gil::RM_PMI)
            )
        )
    )
)

; TODO: change to new version below
; any dissonant note implies that it is surrounded by consonant notes
(defun add-h-dis-imp-cons-cst (is-cons-before is-cons-current is-cons-after)
    (loop
    for b1 in is-cons-before
    for b2 in is-cons-current
    for b3 in is-cons-after
    do
        (let (
            (b2-not (gil::add-bool-var *sp* 0 1)) ; b2-not = !b2
        )
            (gil::g-op *sp* b2 gil::BOT_EQV FALSE b2-not) ; b2-not = !b2
            (gil::g-op *sp* b2-not gil::BOT_IMP b1 1)
            (gil::g-op *sp* b2-not gil::BOT_IMP b3 1)
        )
    )
)

; TODO: new version, check if it works (just diminution ?)
; add the constraint such that (c3 OR (c2 AND c4)) AND (c3 OR dim) is true,
; where : - cn represents if the nth note of the measure is consonant
;         - dim represents if the 3rd note is a diminution
(defun add-h-dis-or-cons-3rd-cst (is-cons-2nd is-cons-3rd is-cons-4th is-dim &optional (is-cst-arr nil))
    (loop
    for b-c2nd in is-cons-2nd
    for b-c3rd in is-cons-3rd
    for b-c4th in is-cons-4th
    for b-dim in is-dim
    ; for i from 0 below (length is-cons-2nd)
    do
        (let (
            (b-and1 (gil::add-bool-var *sp* 0 1)) ; s.f. b-c2nd AND b-c4th
            ; (b-and2 (gil::add-bool-var *sp* 0 1)) ; s.f. b-c2nd AND b-c4th AND b-dim
            ; (b-or (gil::add-bool-var *sp* 0 1)) ; s.f. b-c3rd OR b-and2
        )
            (gil::g-op *sp* b-c2nd gil::BOT_AND b-c4th b-and1) ; b-and1 = b-c2nd AND b-c4th
            ; (gil::g-op *sp* b-and1 gil::BOT_AND b-dim b-and2) ; b-and2 = b-c2nd AND b-c4th AND b-dim

            ; (gil::g-op *sp* b-c3rd gil::BOT_OR b-and1 1) ; b-and2 = b-c2nd AND b-c4th AND b-dim
            (gil::g-op *sp* b-c3rd gil::BOT_OR b-dim 1) ; b-and2 = b-c2nd AND b-c4th AND b-dim
            
            ; (gil::g-op *sp* b-c3rd gil::BOT_OR b-and2 b-or) ; b-or = b-c3rd OR b-and2
            ; (gil::g-op *sp* b-c3rd gil::BOT_OR b-and2 1) ; b-c3rd OR b-and2 = TRUE
            ; (gil::g-op *sp* (true-if-null (nth i is-cst-arr)) gil::BOT_IMP b-or 1) ; (TRUE or is-cst => b-or) = TRUE
        )
    )
)

; add constraints such that
;   any dissonant note implies that it is followed by the next consonant note below
; @m-succ-intervals-brut: list of IntVar, s.f. brut melodic intervals between thesis and arsis
; @is-cons-arr: list of BoolVar, s.f. 1 -> the note is consonant
; @is-cst-arr: list of BoolVar, s.f. 1 -> the note is constrained by a species
(defun add-h-dis-imp-cons-below-cst (m-succ-intervals-brut is-cons-arr &optional (is-cst-arr nil))
    (loop
    for m in m-succ-intervals-brut
    for b in is-cons-arr
    for i from 0 below (length m-succ-intervals-brut)
    do
        (let (
            (b-not (gil::add-bool-var *sp* 0 1)) ; s.f. !b (dissonance)
            (is-cst (true-if-null is-cst-arr i)) ; s.f. is-cst = 1 -> the note is constrained by a species
            (b-and (gil::add-bool-var *sp* 0 1)) ; s.f. b-not && is-cst
        )
            (gil::g-op *sp* b gil::BOT_EQV FALSE b-not) ; b-not = !b (dissonance)
            (gil::g-op *sp* b-not gil::BOT_AND is-cst b-and) ; b-and = b-not && is-cst
            (gil::g-rel-reify *sp* m gil::IRT_LE 0 b-and gil::RM_IMP) ; b-and => m < 0
            (gil::g-rel-reify *sp* m gil::IRT_GQ -2 b-and gil::RM_IMP) ; b-and => m >= -2
        )
    )
)

; is-5qn-linked-arr implies that is-cons-arr1 (supposed to always be true) and is-cons-arr3 are true
(defun add-linked-5qn-cst (is-cons-arr3 is-5qn-linked-arr)
    (loop
    ; for b1 in is-cons-arr1
    for b3 in is-cons-arr3
    for b in is-5qn-linked-arr
    do
        ; (gil::g-rel-reify *sp* b1 gil::IRT_EQ 1 b gil::RM_IMP) ; b => b1
        (gil::g-op *sp* b gil::BOT_IMP b3 1) ; b => b3
    )
)

; add the constraint such that there cp is never equal to cf
(defun add-no-unisson-at-all-cst (cp cf &optional (is-cst-arr nil))
    (loop
        for p in cp
        for q in cf
        for i from 0 below (length cp)
        do
            ; (gil::g-rel *sp* p gil::IRT_NQ q)
            (rel-reify-if p gil::IRT_NQ q (nth i is-cst-arr))
    )
)

; add the constraint such that there is no unisson unless it is the first or last note
(defun add-no-unisson-cst (cp cf)
    (add-no-unisson-at-all-cst (restbutlast cp) (restbutlast cf))
)

; add the constraint such that the first harmonic interval is a perfect consonance
(defun add-p-cons-start-cst (h-intervals)
    ; (inter-member-cst *sp* (first cp) (first cf) P_CONS_VAR)
    (gil::g-member *sp* P_CONS_VAR (first h-intervals))
)

; add the constraint such that the last harmonic interval is a perfect consonance
(defun add-p-cons-end-cst (h-intervals)
    ; (inter-member-cst *sp* (lastone cp) (lastone cf) P_CONS_VAR)
    (gil::g-member *sp* P_CONS_VAR (lastone h-intervals))
)

; TODO check if it's working
; add the constraint such that the first and last harmonic interval are 0 if cp is at the bass
;   not(is-cf-bass[0, 0]) => h-interval[0, 0] = 0
;   not(is-cf-bass[-1, -1]) => h-interval[-1, -1] = 0
; @h-interval: the harmonic interval array
; @is-cf-bass-arr: boolean variables indicating if cf is at the bass
(defun add-tonic-tuned-cst (h-interval is-cf-bass-arr)
    (let (
        (bf-not (gil::add-bool-var *sp* 0 1)) ; for !(first is-cf-bass-arr)
        (bl-not (gil::add-bool-var *sp* 0 1)) ; for !(lastone is-cf-bass-arr)
    )
        (gil::g-op *sp* (first is-cf-bass-arr) gil::BOT_EQV FALSE bf-not) ; bf-not = !(first is-cf-bass-arr)
        (gil::g-op *sp* (lastone is-cf-bass-arr) gil::BOT_EQV FALSE bl-not) ; bl-not = !(lastone is-cf-bass-arr)
        (gil::g-rel-reify *sp* (first h-interval) gil::IRT_EQ 0 bf-not gil::RM_IMP) ; bf-not => h-interval[0, 0] = 0
        (gil::g-rel-reify *sp* (lastone h-interval) gil::IRT_EQ 0 bl-not gil::RM_IMP) ; bl-not => h-interval[-1, -1] = 0
    )
)

; add the constraint such that the harmonic interval is a perfect consonance if it is constrained by a species
(defun add-p-cons-cst-if (h-inter is-cst)
    (let (
        (b-fifth (gil::add-bool-var *sp* 0 1)) ; b-fifth = h-inter is a fifth
        (b-octave (gil::add-bool-var *sp* 0 1)) ; b-octave = h-inter is an octave
        (b-p-cons (gil::add-bool-var *sp* 0 1)) ; b-p-cons = h-inter is a perfect consonance
    )
        (gil::g-rel-reify *sp* h-inter gil::IRT_EQ 7 b-fifth) ; b-fifth = h-inter is a fifth
        (gil::g-rel-reify *sp* h-inter gil::IRT_EQ 0 b-octave) ; b-octave = h-inter is an octave
        (gil::g-op *sp* b-fifth gil::BOT_OR b-octave b-p-cons) ; b-p-cons = b-fifth or b-octave
        (gil::g-op *sp* is-cst gil::BOT_IMP b-p-cons 1) ; is-cst => b-p-cons
    )
)

(defun add-penult-cons-cst (b-bass h-interval &optional (and-cond nil))
    (if (null and-cond)
        (gil::g-ite *sp* b-bass NINE THREE h-interval)
        (and-ite b-bass NINE THREE h-interval and-cond)
    )
)

; add a constraint such that there is no seventh harmonic interval if cf is at the top
(defun add-no-seventh-cst (h-intervals is-cf-bass-arr &optional (is-cst-arr nil))
    (loop
    for h in h-intervals
    for b in is-cf-bass-arr
    for i from 0 below (length h-intervals)
    do
        (let (
            (b-not (gil::add-bool-var *sp* 0 1)) ; b-not = !b
            (is-cst (nth i is-cst-arr)) ; is-cst = is-cst-arr[i]
            (b-and (gil::add-bool-var *sp* 0 1)) ; b-and = b-not and is-cst
        )
            (gil::g-op *sp* b gil::BOT_EQV FALSE b-not) ; b-not = !b
            (if (null is-cst)
                (gil::g-op *sp* b-not gil::BOT_AND TRUE b-and) ; b-and = b-not
                (gil::g-op *sp* b-not gil::BOT_AND is-cst b-and) ; b-and = b-not and is-cst
            )
            (gil::g-rel-reify *sp* h gil::IRT_NQ 10 b-and gil::RM_IMP) ; b-and => h != 10
            (gil::g-rel-reify *sp* h gil::IRT_NQ 11 b-and gil::RM_IMP) ; b-and => h != 11
        )
    )
)

; add a constraint such that there is no second harmonic interval if:
;   - cf is at the bass AND
;   - octave/unisson harmonic interval precedes it
(defun add-no-second-cst (h-intervals-arsis h-intervals-thesis is-cf-bass-arr &optional (is-cst-arr nil))
    (loop
    for ia in h-intervals-arsis
    for it in h-intervals-thesis
    for b in is-cf-bass-arr
    for i from 0 below (length h-intervals-arsis)
    do
        (let (
            (b-uni (gil::add-bool-var *sp* 0 1)) ; b-uni = (ia == 0)
            (b-and (gil::add-bool-var *sp* 0 1)) ; b-and = b AND b-uni
            (is-cst (true-if-null is-cst-arr i)) ; is-cst = is-cst-arr[i] or TRUE
            (b-and-cst (gil::add-bool-var *sp* 0 1)) ; b-and-cst = b-and AND is-cst
        )
            (gil::g-rel-reify *sp* ia gil::IRT_EQ 0 b-uni) ; b-uni = (ia == 0)
            (gil::g-op *sp* b gil::BOT_AND b-uni b-and) ; b-and = b AND b-uni
            (gil::g-op *sp* b-and gil::BOT_AND is-cst b-and-cst) ; b-and-cst = b-and AND is-cst
            ; (gil::g-rel-reify *sp* it gil::IRT_NQ 1 b-and gil::RM_IMP)
            ; (gil::g-rel-reify *sp* it gil::IRT_NQ 2 b-and gil::RM_IMP)
            (gil::g-rel-reify *sp* it gil::IRT_NQ 1 b-and-cst gil::RM_IMP)
            (gil::g-rel-reify *sp* it gil::IRT_NQ 2 b-and-cst gil::RM_IMP)
        )
    )
)

; (defun add-penult-cons-cst (is-cf-bass-arr h-intervals)
;     (gil::g-ite *sp* (penult is-cf-bass-arr) NINE THREE (penult h-intervals))
; )

; (defun add-last-cons-cst (is-cf-bass-arr h-intervals)
;     (gil::g-ite *sp* (penult is-cf-bass-arr) NINE THREE (lastone h-intervals))
; )

(defun add-no-m-jump-cst (m-intervals &optional (jump 8))
    (gil::g-rel *sp* m-intervals gil::IRT_LQ jump)
)

; add a constraint such that m-intervals does not belong to [9, 10, 11]
(defun add-no-m-jump-extend-cst (m-intervals &optional (is-cst-arr nil))
    (if (null is-cst-arr)
        ; then
        (progn
        (gil::g-rel *sp* m-intervals gil::IRT_NQ 9)
        (gil::g-rel *sp* m-intervals gil::IRT_NQ 10)
        (gil::g-rel *sp* m-intervals gil::IRT_NQ 11)
        )
        ; else
        (progn
        (loop
            for m in m-intervals
            for b in is-cst-arr
            do
                (gil::g-rel-reify *sp* m gil::IRT_NQ 9 b gil::RM_IMP)
                (gil::g-rel-reify *sp* m gil::IRT_NQ 10 b gil::RM_IMP)
                (gil::g-rel-reify *sp* m gil::IRT_NQ 11 b gil::RM_IMP)
        )
        )
    )
)

; add melodic interval constraints such that:
;   - minor sixth intervals and octave intervals implies that is-nbour is true
;   - no seventh intervals
(defun add-m-inter-arsis-cst (m-intervals-ta is-nbour-arr)
    (loop
        for m in m-intervals-ta
        for n in is-nbour-arr
        do
            (let (
                (b-maj-six (gil::add-bool-var *sp* 0 1)) ; for (m = 9)
                (b-min-sev (gil::add-bool-var *sp* 0 1)) ; for (m == 10)
                (b-maj-sev (gil::add-bool-var *sp* 0 1)) ; for (m == 11)
                (b-or (gil::add-bool-var *sp* 0 1)) ; temporary variable for (b-min-sev or b-maj-sev)
            )
                ; (gil::g-rel-reify *sp* m gil::IRT_EQ 9 n gil::RM_PMI) ; m == 9 implies n is true
                (gil::g-rel-reify *sp* m gil::IRT_EQ 12 n gil::RM_PMI) ; m == 12 implies n is true
                (gil::g-rel-reify *sp* m gil::IRT_EQ 9 b-maj-six) ; b-maj-six = (m == 9)
                (gil::g-rel-reify *sp* m gil::IRT_EQ 10 b-min-sev) ; b-min-sev = (m == 10)
                (gil::g-rel-reify *sp* m gil::IRT_EQ 11 b-maj-sev) ; b-maj-sev = (m == 11)
                (gil::g-op *sp* b-min-sev gil::BOT_OR b-maj-sev b-or) ; b-or = (b-min-sev or b-maj-sev)
                (gil::g-op *sp* b-or gil::BOT_OR b-maj-six 0) ; not (b-min-sev || b-maj-sev)
            )
    )
)

; add melodic interval constraints such that there is no chromatic interval:
;   - no m1 == 1 and m2 == 2 OR
;   - no m1 == -1 and m2 == -2
(defun add-no-chromatic-m-cst (m-intervals-brut m2-intervals-brut)
    (loop
        for m1 in (rest m-intervals-brut)
        for m2 in m2-intervals-brut do
        (let (
            (b1 (gil::add-bool-var *sp* 0 1)) ; for (m1 == 1)
            (b2 (gil::add-bool-var *sp* 0 1)) ; for (m2 == 2)
            (b3 (gil::add-bool-var *sp* 0 1)) ; for (m1 == -1)
            (b4 (gil::add-bool-var *sp* 0 1)) ; for (m2 == -2)
        )
            (gil::g-rel-reify *sp* m1 gil::IRT_EQ 1 b1) ; b1 = (m1 == 1)
            (gil::g-rel-reify *sp* m2 gil::IRT_EQ 2 b2) ; b2 = (m2 == 2)
            (gil::g-op *sp* b1 gil::BOT_AND b2 0) ; not(b1 and b2)
            (gil::g-rel-reify *sp* m1 gil::IRT_EQ -1 b3) ; b3 = (m1 == -1)
            (gil::g-rel-reify *sp* m2 gil::IRT_EQ -2 b4) ; b4 = (m2 == -2)
            (gil::g-op *sp* b3 gil::BOT_AND b4 0) ; not(b3 and b4)
        )
    )
)

; add melodic interval constraints such that there is no chromatic interval:
;   - no m1 == 1 and m2 == 1 OR
;   - no m1 == -1 and m2 == -1
; @m-intervals-brut: list of all the melodic intervals
(defun add-no-chromatic-allm-cst (m-intervals-brut)
    (loop
        for m1 in m-intervals-brut
        for m2 in (rest m-intervals-brut) do
        (let (
            (b1 (gil::add-bool-var *sp* 0 1)) ; for (m1 == 1)
            (b2 (gil::add-bool-var *sp* 0 1)) ; for (m2 == 1)
            (b3 (gil::add-bool-var *sp* 0 1)) ; for (m1 == -1)
            (b4 (gil::add-bool-var *sp* 0 1)) ; for (m2 == -1)
        )
            (gil::g-rel-reify *sp* m1 gil::IRT_EQ 1 b1) ; b1 = (m1 == 1)
            (gil::g-rel-reify *sp* m2 gil::IRT_EQ 1 b2) ; b2 = (m2 == 1)
            (gil::g-op *sp* b1 gil::BOT_AND b2 0) ; not(b1 and b2)
            (gil::g-rel-reify *sp* m1 gil::IRT_EQ -1 b3) ; b3 = (m1 == -1)
            (gil::g-rel-reify *sp* m2 gil::IRT_EQ -1 b4) ; b4 = (m2 == -1)
            (gil::g-op *sp* b3 gil::BOT_AND b4 0) ; not(b3 and b4)
        )
    )
)

(defun create-motions (m-intervals-brut cf-brut-m-intervals motions)
    (loop
        for p in m-intervals-brut
        for q in cf-brut-m-intervals
        for m in motions
        do
            (let (
                ; boolean variables
                (b-pu (gil::add-bool-var *sp* 0 1)) ; boolean p up
                (b-qu (gil::add-bool-var *sp* 0 1)) ; boolean q up
                (b-ps (gil::add-bool-var *sp* 0 1)) ; boolean p stays
                (b-qs (gil::add-bool-var *sp* 0 1)) ; boolean q stays
                (b-pd (gil::add-bool-var *sp* 0 1)) ; boolean p down
                (b-qd (gil::add-bool-var *sp* 0 1)) ; boolean q down
                ; direct motion
                (b-both-up (gil::add-bool-var *sp* 0 1)) ; boolean both up
                (b-both-stays (gil::add-bool-var *sp* 0 1)) ; boolean both stays
                (b-both-down (gil::add-bool-var *sp* 0 1)) ; boolean both down
                (dm-or1 (gil::add-bool-var *sp* 0 1)) ; temporary boolean
                (dm-or2 (gil::add-bool-var *sp* 0 1)) ; temporary boolean
                ; oblique motion
                (b-pu-qs (gil::add-bool-var *sp* 0 1)) ; boolean p up and q stays
                (b-pd-qs (gil::add-bool-var *sp* 0 1)) ; boolean p down and q stays
                (b-ps-qu (gil::add-bool-var *sp* 0 1)) ; boolean p stays and q up
                (b-ps-qd (gil::add-bool-var *sp* 0 1)) ; boolean p stays and q down
                (om-or1 (gil::add-bool-var *sp* 0 1)) ; temporary boolean
                (om-or2 (gil::add-bool-var *sp* 0 1)) ; temporary boolean
                (om-or3 (gil::add-bool-var *sp* 0 1)) ; temporary boolean
                ; contrary motion
                (b-pu-qd (gil::add-bool-var *sp* 0 1)) ; boolean p up and q down
                (b-pd-qu (gil::add-bool-var *sp* 0 1)) ; boolean p down and q up
                (cm-or1 (gil::add-bool-var *sp* 0 1)) ; temporary boolean
            )
                ; (print (format nil "q = ~a" q))
                (gil::g-rel-reify *sp* p gil::IRT_LE 0 b-pd) ; b-pd = (p < 0)
                (gil::g-rel-reify *sp* p gil::IRT_EQ 0 b-ps) ; b-ps = (p == 0)
                (gil::g-rel-reify *sp* p gil::IRT_GR 0 b-pu) ; b-pu = (p > 0)
                (gil::g-rel-reify *sp* q gil::IRT_LE 0 b-qd) ; b-qd = (q < 0)
                (gil::g-rel-reify *sp* q gil::IRT_EQ 0 b-qs) ; b-qs = (q == 0)
                (gil::g-rel-reify *sp* q gil::IRT_GR 0 b-qu) ; b-qu = (q > 0)
                ; direct motion
                ; (print "Direct movemement")
                (gil::g-op *sp* b-pu gil::BOT_AND b-qu b-both-up) ; b-both-up = (b-pu and b-qu)
                (gil::g-op *sp* b-ps gil::BOT_AND b-qs b-both-stays) ; b-both-stays = (b-ps and b-qs)
                (gil::g-op *sp* b-pd gil::BOT_AND b-qd b-both-down) ; b-both-down = (b-pd and b-qd)
                (gil::g-op *sp* b-both-up gil::BOT_OR b-both-stays dm-or1) ; dm-or1 = (b-both-up or b-both-stays)
                (gil::g-op *sp* dm-or1 gil::BOT_OR b-both-down dm-or2) ; dm-or2 = (dm-or1 or b-both-down)
                (gil::g-rel-reify *sp* m gil::IRT_EQ DIRECT dm-or2) ; m = 1 if dm-or2
                ; oblique motion
                ; (print "Oblique motion")
                (gil::g-op *sp* b-pu gil::BOT_AND b-qs b-pu-qs) ; b-pu-qs = (b-pu and b-qs)
                (gil::g-op *sp* b-pd gil::BOT_AND b-qs b-pd-qs) ; b-pd-qs = (b-pd and b-qs)
                (gil::g-op *sp* b-ps gil::BOT_AND b-qu b-ps-qu) ; b-ps-qu = (b-ps and b-qu)
                (gil::g-op *sp* b-ps gil::BOT_AND b-qd b-ps-qd) ; b-ps-qd = (b-ps and b-qd)
                (gil::g-op *sp* b-pu-qs gil::BOT_OR b-pd-qs om-or1) ; om-or1 = (b-pu-qs or b-pd-qs)
                (gil::g-op *sp* om-or1 gil::BOT_OR b-ps-qu om-or2) ; om-or2 = (om-or1 or b-ps-qu)
                (gil::g-op *sp* om-or2 gil::BOT_OR b-ps-qd om-or3) ; om-or3 = (om-or2 or b-ps-qd)
                (gil::g-rel-reify *sp* m gil::IRT_EQ OBLIQUE om-or3) ; m = 0 if om-or3
                ; contrary motion
                ; (print "Contrary motion")
                (gil::g-op *sp* b-pu gil::BOT_AND b-qd b-pu-qd) ; b-pu-qd = (b-pu and b-qd)
                (gil::g-op *sp* b-pd gil::BOT_AND b-qu b-pd-qu) ; b-pd-qu = (b-pd and b-qu)
                (gil::g-op *sp* b-pu-qd gil::BOT_OR b-pd-qu cm-or1) ; cm-or1 = (b-pu-qd or b-pd-qu)
                (gil::g-rel-reify *sp* m gil::IRT_EQ CONTRARY cm-or1) ; m = -1 if cm-or1
            )
    )
)

; create the motion list variable as it is perceived by the human ear,
; i.e. if the interval between the thesis and the arsis note is greater than a third,
; then the motion is perceived from the arsis note and not from the thesis note
; @m-intervals-ta: melodic intervals between the thesis and the arsis note
; @motions: motions perceived from the thesis note
; @motions-arsis: motions perceived from the arsis note
; @real-motions: motions perceived by the human ear
(defun create-real-motions (m-intervals-ta motions motions-arsis real-motions)
    (loop
        for tai in m-intervals-ta
        for t-move in motions
        for a-move in motions-arsis
        for r-move in real-motions
        do
            (let (
                (b (gil::add-bool-var *sp* 0 1)) ; for (tai > 4)
            )
                (gil::g-rel-reify *sp* tai gil::IRT_GR 4 b) ; b = (tai > 4)
                (gil::g-ite *sp* b a-move t-move r-move) ; r-move = (b ? a-move : t-move)
            )
    )
)

(defun add-no-direct-move-to-p-cons-cst (motions is-p-cons-arr &optional (r t))
    (loop
        for m in motions
        for b in (rest-if is-p-cons-arr r)
        do
            (gil::g-rel-reify *sp* m gil::IRT_NQ DIRECT b gil::RM_IMP)
    )
)

(defun rest-if (l b)
    (if b
        (rest l)
        l
    )
)

; TODO pass to new version function below
; add the constraint such that there is no battuta kind of motion, i.e.:
;   - contrary motion
;   - skip in the upper voice
;   - lead to an octave
(defun add-no-battuta-cst (motions h-intervals m-intervals-brut is-cf-bass-arr &optional (is-cst-arr nil))
    (loop
    for move in motions
    for hi in (rest h-intervals)
    for mi in m-intervals-brut
    for b in (butlast is-cf-bass-arr)
    for i from 0 below *cf-last-index
    do
        (let (
            (is-cm (gil::add-bool-var *sp* 0 1)) ; is contrary motion
            (is-oct (gil::add-bool-var *sp* 0 1)) ; is moving to octave
            (is-cp-down (gil::add-bool-var *sp* 0 1)) ; is counterpoint going down
            (b-and1 (gil::add-bool-var *sp* 0 1)) ; temporary boolean
            (b-and2 (gil::add-bool-var *sp* 0 1)) ; temporary boolean
            (b-and3 (gil::add-bool-var *sp* 0 1)) ; temporary boolean
        )
            (gil::g-rel-reify *sp* move gil::IRT_EQ CONTRARY is-cm) ; is-cm = (m == -1)
            (gil::g-rel-reify *sp* hi gil::IRT_EQ 0 is-oct) ; is-oct = (hi == 0)
            (gil::g-rel-reify *sp* mi gil::IRT_LE -4 is-cp-down) ; is-cp-down = (mi < -4)
            (gil::g-op *sp* is-cm gil::BOT_AND is-oct b-and1) ; b-and1 = (is-cm and is-oct)
            (gil::g-op *sp* b-and1 gil::BOT_AND is-cp-down b-and2) ; b-and2 = (b-and1 and is-cp-down)
            (if (null is-cst-arr)
                ; then constraint is always added
                (gil::g-op *sp* b-and2 gil::BOT_AND b 0) ; (is-cm and is-oct and is-cp-down and b) = FALSE
                ; else constraint is added only if the current note is constrained
                (progn
                    (gil::g-op *sp* b-and2 gil::BOT_AND b b-and3) ; b-and3 = (b-and2 and b)
                    ; is-cst => (b-and3 == 0) can be written as not (is-cst and b-and3)
                    (gil::g-op *sp* (nth i is-cst-arr) gil::BOT_AND b-and3 0)
                )
            )
        )
    )
)

; TODO TEST new version
; add the constraint such that there is no battuta kind of motion, i.e.:
;   - contrary motion
;   - skip in the upper voice
;   - lead to an octave
(defun add-no-battuta-bis-cst (motions h-intervals m-intervals-brut cf-brut-m-intervals is-cf-bass-arr &optional (is-cst-arr nil))
    (loop
    for move in motions
    for hi in (rest h-intervals)
    for mi in m-intervals-brut
    for cf-mi in cf-brut-m-intervals
    for b in (butlast is-cf-bass-arr)
    for i from 0 below *cf-last-index
    do
        (let (
            (is-cm (gil::add-bool-var *sp* 0 1)) ; is contrary motion
            (is-oct (gil::add-bool-var *sp* 0 1)) ; is moving to octave
            (is-cp-down (gil::add-bool-var *sp* 0 1)) ; is counterpoint going down more than 4 semi-tones
            (is-cf-down (gil::add-bool-var *sp* 0 1)) ; is cantus firmus going down more than 4 semi-tones
            (b-not (gil::add-bool-var *sp* 0 1)) ; !b = cantus firmus is not the bass
            (b-and1 (gil::add-bool-var *sp* 0 1)) ; temporary boolean
            (b-and2 (gil::add-bool-var *sp* 0 1)) ; temporary boolean
            (b-and3 (gil::add-bool-var *sp* 0 1)) ; temporary boolean
        )
            (gil::g-rel-reify *sp* move gil::IRT_EQ CONTRARY is-cm) ; is-cm = (m == 0)
            (gil::g-rel-reify *sp* hi gil::IRT_EQ 0 is-oct) ; is-oct = (hi == 0)
            (gil::g-rel-reify *sp* mi gil::IRT_LE -4 is-cp-down) ; is-cp-down = (mi < -4)
            (gil::g-rel-reify *sp* cf-mi gil::IRT_LE -4 is-cf-down) ; is-cf-down = (cf-mi < -4)
            (gil::g-op *sp* b gil::BOT_EQV FALSE b-not) ; b-not = !b
            (gil::g-op *sp* is-cm gil::BOT_AND is-oct b-and1) ; b-and1 = (is-cm and is-oct)
            (gil::g-op *sp* b gil::BOT_AND is-cp-down b-and2) ; b-and2 = (b-and1 and is-cp-down)
            (gil::g-op *sp* b-not gil::BOT_AND is-cf-down b-and3) ; b-and3 = (b-not and is-cf-down)

            (if (null is-cst-arr)
                ; then constraint is always added
                (progn
                    ; first case: (is-cm and is-oct and b and is-cp-down) = FALSE
                    (gil::g-op *sp* b-and1 gil::BOT_AND b-and2 0)
                    ; second case: (is-cm and is-oct and b-not and is-cf-down) = FALSE
                    (gil::g-op *sp* b-and1 gil::BOT_AND b-and3 0)
                )
                ; else constraint is added only if the current note is constrained
                (progn (let (
                    (b-and4 (gil::add-bool-var *sp* 0 1)) ; first case
                    (b-and5 (gil::add-bool-var *sp* 0 1)) ; second case
                )
                    (gil::g-op *sp* b-and1 gil::BOT_AND b-and2 b-and4) ; first case: b-and4 = (b-and1 and b-and2)
                    (gil::g-op *sp* b-and1 gil::BOT_AND b-and3 b-and5) ; second case: b-and5 = (b-and1 and b-and3)
                    ; is-cst => (b-and == 0) can be written as not (is-cst and b-and)
                    (gil::g-op *sp* (nth i is-cst-arr) gil::BOT_AND b-and4 0) ; first case
                    (gil::g-op *sp* (nth i is-cst-arr) gil::BOT_AND b-and5 0) ; second case
                ))
            )
        )
    )
)

;; 5th species methods
; add the constraint such that the selected notes are the same as the midi-selected notes
(defun add-selected-notes-cst (selected midi-selected cp)
    (print "Adding selected notes constraint")
    (print selected)
    (print midi-selected)
    (loop
    for i in selected
    for ms in midi-selected
    do
        (setq i+1 (+ i 1))
        (gil::g-rel *sp* (nth i cp) gil::IRT_EQ (first ms))
        (gil::g-rel *sp* (nth i+1 cp) gil::IRT_EQ (second ms))
    )
)

; add constraints such that the boolean array is true if the simple constraint is respected
(defun create-simple-boolean-arr (candidate-arr rel-type cst b-arr)
    (loop
        for c in candidate-arr
        for b in b-arr
        do
            (gil::g-rel-reify *sp* c rel-type cst b)
    )
)

; do the gil::g-ite constraint but only if and-cond is true
(defun and-ite (test then else var and-cond)
    (let (
        (b-and-then (gil::add-bool-var *sp* 0 1)) ; b-and-then = test and and-cond
        (test-not (gil::add-bool-var *sp* 0 1)) ; test-not = !test
        (b-and-else (gil::add-bool-var *sp* 0 1)) ; b-and-else = !test and and-cond
    )
        (gil::g-op *sp* test gil::BOT_AND and-cond b-and-then) ; b-and-then = test and and-cond
        (gil::g-op *sp* test gil::BOT_EQV FALSE test-not) ; test-not = !test
        (gil::g-op *sp* test-not gil::BOT_AND and-cond b-and-else) ; b-and-else = !test and and-cond
        (gil::g-rel-reify *sp* var gil::IRT_EQ then b-and-then gil::RM_IMP) ; b-and-then => var = then
        (gil::g-rel-reify *sp* var gil::IRT_EQ else b-and-else gil::RM_IMP) ; b-and-else => var = else
    )
)

; merge the boolean arrays with the and operator
(defun bot-merge-array (b-arr1 b-arr2 b-collect-arr &optional (bot gil::BOT_AND))
    (loop
    for b1 in b-arr1
    for b2 in b-arr2
    for b in b-collect-arr
    do
        (gil::g-op *sp* b1 bot b2 b)
    )
)

; merge the boolean arrays with the or operator and just return it
; note: cbma stands for collect boolean-operation-type merge array
(defun cbma (b-arr1 b-arr2 &optional (bot gil::BOT_AND))
    (let (
        (b-collect-arr (gil::add-bool-var-array *sp* (length b-arr1) 0 1))
    )
        (loop
        for b1 in b-arr1
        for b2 in b-arr2
        for b in b-collect-arr
        do
            (gil::g-op *sp* b1 bot b2 b)
        )
        ; (return b-collect-arr)
        b-collect-arr
    )
)


(defun collect-t-or-f-array (yes-arr no-arr)
    (cbma
                yes-arr
                (collect-not-array no-arr)
                gil::BOT_OR
    )
)

(defun collect-not-array (arr)
    (cbma arr (gil::add-bool-var-array *sp* (length arr) 0 0) gil::BOT_EQV)
)

; do the gil::g-rel-reify constraint but use the condition that (b AND and-cond) is true
(defun bot-reify (var rel-type cst b and-cond &optional (bot gil::BOT_AND) (mode gil::RM_EQV))
    (let (
        (b-and (gil::add-bool-var *sp* 0 1)) ; b-and = b and and-cond
    )
        (gil::g-op *sp* b bot and-cond b-and) ; b-and = b and and-cond
        (gil::g-rel-reify *sp* var rel-type cst b-and mode) ; b-and == var rel-type cst
    )
)

; return the index of a note as all the notes are in a row,
; i.e. return the total index of the note at the given measure at the given beat assuming that we are in 4 4 time
; the index is 0-based, same for measure and beat
(defun total-index (measure beat)
    (+ (* measure 4) beat)
)

; is-mostly-3rd is true if second, third and fourth notes are from 3rd species
; note that is-mostly-3rd-arr have a length 4 times shorter than is-3rd-species-arr
(defun create-is-mostly-3rd-arr (is-3rd-species-arr is-mostly-3rd-arr)
    (loop
    for meas from 0 below (length is-mostly-3rd-arr)
    do
        (let (
            (b-23 (gil::add-bool-var *sp* 0 1)) ; b-23 = is-3rd-species-arr[meas][1] AND is-3rd-species-arr[meas][2]
            ; (b-234 (gil::add-bool-var *sp* 0 1)) ; b-234 = b-23 AND is-3rd-species-arr[meas][3]
        )
            ; b-23
            (gil::g-op *sp* (nth (total-index meas 1) is-3rd-species-arr) gil::BOT_AND (nth (total-index meas 2) is-3rd-species-arr) b-23)
            ; b-23 and "b-4" are stocked in is-mostly-3rd-arr[meas]
            (gil::g-op *sp* b-23 gil::BOT_AND (nth (total-index meas 3) is-3rd-species-arr) (nth meas is-mostly-3rd-arr))
        )
    )
)

; collect elements all the 4 elements of the array, i.e. n, n+4, n+8, n+12, etc.
; note: n is the offset
(defun collect-by-4 (arr &optional (offset 0) (b nil) (up-bound 4))
    (setq len (if (= offset 0) *cf-len *cf-last-index))
    (if (null b)
        ; then make a boolean array
        (setq ret (gil::add-bool-var-array *sp* len 0 1))
        ; else make a integer array
        (setq ret (gil::add-int-var-array *sp* len 0 up-bound))
    )
    (loop
    for i from offset below (length arr) by 4
    for j from 0 below len
    do
        ; (print (format nil "i: ~a, j: ~a" i j))
        (gil::g-rel *sp* (nth i arr) gil::IRT_EQ (nth j ret))
    )
    ret
)

(defun create-by-4 (arr-from arr-to &optional (offset 0))
    (loop
    for i from offset below (length arr-from) by 4
    for j in arr-to
    do
        (gil::g-rel *sp* (nth i arr-from) gil::IRT_EQ j)
    )
)

(defun rel-reify-if (var rel-type cst &optional (b nil) (rm gil::RM_IMP))
    (if (null b)
        (gil::g-rel *sp* var rel-type cst)
        (gil::g-rel-reify *sp* var rel-type cst b rm)
    )
)

; return BoolVar true if nil element
(defun true-if-null (arr i)
    (if (null arr)
        ; then
        TRUE
        ; else
        (nth i arr)
    )
)

; add the constraint such that if sp3 is 4th species, then sp4 is 0 and the next sp1 is 4th species
; and vice versa (cannot have 4th species in first position without 4th species in third position)
; - sp-arr3: array of IntVar for species at the third position
; - sp-arr4: array of IntVar for species at the fourth position
; - sp-arr1: array of IntVar for species at the first position
(defun add-4th-rythmic-cst (sp-arr3 sp-arr4 sp-arr1)
    (print "Adding 4th rythmic constraint...")
    (loop
    for sp3 in sp-arr3
    for sp4 in sp-arr4
    for sp1 in (rest sp-arr1)
    do
        (let (
            (b-34 (gil::add-bool-var *sp* 0 1)) ; b-34 = sp3 == 4th species
            ; (b-40 (gil::add-bool-var *sp* 0 1)) ; b-40 = sp4 == 0
            (b-14 (gil::add-bool-var *sp* 0 1)) ; b-14 = sp1 == 4th species
        )
            (gil::g-rel-reify *sp* sp3 gil::IRT_EQ 4 b-34) ; b-34 = sp3 == 4th species
            (gil::g-rel-reify *sp* sp1 gil::IRT_EQ 4 b-14) ; b-14 = sp1 == 4th species
            (gil::g-rel-reify *sp* sp4 gil::IRT_EQ 0 b-34 gil::RM_IMP) ; b-34 => sp4 == 0
            (gil::g-op *sp* b-34 gil::BOT_EQV b-14 1) ; b-34 <=> b-14
        )
    )
)

; add the constraint such that if n belongs to @species, then n+m have to exist (not 0)
; by default, the constraint is added for the third species
; - species-arr: array of IntVar for species
; - spec: species to check
; - offset: offset to check
(defun add-no-silence-cst (species-arr &key (spec 3) (offset 1))
    (loop
    for n in species-arr
    for n+m in (nthcdr offset species-arr)
    do
        (let (
            (b (gil::add-bool-var *sp* 0 1)) ; b = (n == species)
        )
            (gil::g-rel-reify *sp* n gil::IRT_EQ spec b) ; b = (n == spec)
            (gil::g-rel-reify *sp* n+m gil::IRT_NQ 0 b gil::RM_IMP) ; b => (n+m != 0)
        )
    )
)

; add all constraints to create a rythmic and select what species to use
; mandatory rules are:
; - 4th species is only used in third and first position
; - 4th species in third position is followed by a 0 (no note/constraint) and then a 4th species
; - no 3rd species followed by 0
; classic rules are:
; - first and penultimate measure are 4th species
; - only 3rd and 4th species are used
; - 3rd species should represent at least 1/3 of the notes
; - 4th species should represent at least 1/4 of the notes
(defun create-species-arr (species-arr &key (min-3rd-pc 0.33) (min-4th-pc 0.25))
    (print "Create species array...")
    (let* (
        (count-3rd (gil::add-int-var-array *sp* *total-cp-len 0 1))
        (count-4th (gil::add-int-var-array *sp* *total-cp-len 0 1))
        (n-3rd-int (floor (* *total-cp-len min-3rd-pc))) ; minimum number of 3rd species
        (n-4th-int (floor (* *total-cp-len min-4th-pc))) ; minimum number of 4th species
        (sum-3rd (gil::add-int-var *sp* n-3rd-int *total-cp-len)) ; set the bounds of sum-3rd
        (sum-4th (gil::add-int-var *sp* n-4th-int *total-cp-len)) ; set the bounds of sum-4th
    )
        (setq *sp-arr (list
            (collect-by-4 species-arr 0 t)
            (collect-by-4 species-arr 1 t)
            (collect-by-4 species-arr 2 t)
            (collect-by-4 species-arr 3 t)
        ))

        (print "Counting 3rd and 4th species...")
        ; count the number of 3rd and 4th species
        (add-cost-cst species-arr gil::IRT_EQ 3 count-3rd)
        (add-cost-cst species-arr gil::IRT_EQ 4 count-4th)
        ; sum the number of 3rd and 4th species
        (gil::g-sum *sp* sum-3rd count-3rd)
        (gil::g-sum *sp* sum-4th count-4th)

        ; 4th species is only used in third and first position
        (gil::g-rel *sp* (second *sp-arr) gil::IRT_NQ 4) ; second position not 4th species
        (gil::g-rel *sp* (fourth *sp-arr) gil::IRT_NQ 4) ; fourth position not 4th species
        
        ; 4th species in third position is followed by a 0 (no note/constraint) and then a 4th species
        (add-4th-rythmic-cst (third *sp-arr) (fourth *sp-arr) (first *sp-arr))

        ; only 3rd and 4th species are used
        (gil::g-rel *sp* species-arr gil::IRT_NQ 1) ; not 1st species
        (gil::g-rel *sp* species-arr gil::IRT_NQ 2) ; not 2nd species

        ; first and penultimate measure are 4th species
        ; first measure = [0 0 4 0]
        (gil::g-rel *sp* (first (first *sp-arr)) gil::IRT_EQ 0) ; first note is silent
        (gil::g-rel *sp* (first (second *sp-arr)) gil::IRT_EQ 0) ; second note is silent
        (gil::g-rel *sp* (first (third *sp-arr)) gil::IRT_EQ 4) ; third note is 4th species
        ; penultimate measure = [4 0 4 0]
        (gil::g-rel *sp* (penult (first *sp-arr)) gil::IRT_EQ 4) ; first note is 4th species
        (gil::g-rel *sp* (lastone (second *sp-arr)) gil::IRT_EQ 0) ; second note does not exist
        (gil::g-rel *sp* (lastone (third *sp-arr)) gil::IRT_EQ 4) ; third note is 4th species

        ; no silence after 3rd species notes
        (add-no-silence-cst species-arr)

        ; no silence after 4th species notes in n+4 position
        (add-no-silence-cst species-arr :spec 4 :offset 4) ; TODO too restrictive because 4 does not necessarily mean the start of a new note
    )
)

; find the next @type note in the borrowed scale,
; if there is no note in the range then return the note of the other @type
; - note: integer for the current note
; - type: atom [lower | higher] for the type of note to find
; note: this function has noting to do with GECODE
(defun find-next-note (note type)
    (let (
        ; first sort the scale corresponding to the type
        (sorted-scale (if (eq type 'lower)
            (sort *extended-cp-domain #'>)
            (sort *extended-cp-domain #'<)
        ))
    )
        (if (eq type 'lower)
            ; then we search the first note in the sorted scale that is lower than the current note
            (progn
            (loop for n in sorted-scale do
                (if (< n note) (return-from find-next-note n))
            )
            ; no note so we return the penultimate element of the sorted scale
            (penult sorted-scale)
            )
            ; else we search the first note in the sorted scale that is higher than the current note
            (progn
            (loop for n in sorted-scale do
                (if (> n note) (return-from find-next-note n))
            )
            ; no note so we return the penultimate element of the sorted scale
            (penult sorted-scale)
            )
        )
    )
)

; parse the species array to get the corresponding rythmic pattern for open music
; - species-arr: array of integer for species (returned by the next-solution algorithm)
; - cp-arr: array of integer for counterpoint notes (returned by the next-solution algorithm)
; note: this function has noting to do with GECODE
(defun parse-species-to-om-rythmic (species-arr cp-arr)
    ; replace the last element of the species array by 1
    (setf (first (last species-arr)) 1)
    (build-rythmic-pattern species-arr cp-arr)
)

; build the rythmic pattern for open music from the species array
; - species-arr: array of integer for species
; - cp-arr: array of integer for counterpoint notes
; - rythmic-arr: array of integer for the rythmic (supposed to be nil and then filled by the recursive function)
; - notes-arr: array of interger for notes (supposed to be nil and then filled by the recursive function)
; - b-debug: boolean to print debug info
; note: this function has noting to do with GECODE
(defun build-rythmic-pattern (species-arr cp-arr &optional (rythmic-arr nil) (notes-arr nil) (b-debug nil))
    ; print debug info
    (if b-debug
        (progn
        (print "Current species and notes:")
        (print species-arr)
        (print cp-arr)
        (print "Current answer:")
        (print rythmic-arr)
        (print notes-arr)
        )
    )
    ; base case
    (if (null species-arr)
        ; then return the rythmic pattern
        (list rythmic-arr notes-arr)
    )

    (let (
        (sn (first species-arr)) ; current species
        (sn+1 (second species-arr)) ; next species
        (sn+2 (third species-arr)) ; next next species
        (sn+3 (fourth species-arr)) ; next next next species
        (cn (first cp-arr)) ; current counterpoint note
        (cn+1 (second cp-arr)) ; next counterpoint note
        (cn+2 (third cp-arr)) ; next next counterpoint note
        (cn+3 (fourth cp-arr)) ; next next next counterpoint note
    )
        ; replace all nil by -1 for the species
        (if (null sn) (setf sn -1))
        (if (null sn+1) (setf sn+1 -1))
        (if (null sn+2) (setf sn+2 -1))
        (if (null sn+3) (setf sn+3 -1))
        ; replace all nil by -1 for the counterpoint
        (if (null cn) (setf cn -1))
        (if (null cn+1) (setf cn+1 -1))
        (if (null cn+2) (setf cn+2 -1))
        (if (null cn+3) (setf cn+3 -1))

        (if b-debug
            (progn
            (print (format nil "sn: ~a, sn+1: ~a, sn+2: ~a, sn+3: ~a" sn sn+1 sn+2 sn+3))
            (print (format nil "cn: ~a, cn+1: ~a, cn+2: ~a, cn+3: ~a" cn cn+1 cn+2 cn+3))
            )
        )

        (cond
            ; 1 if it is the last note [1 -1 ...]
            ((and (= sn 1) (= sn+1 -1))
                (list (append rythmic-arr (list 1)) (append notes-arr (list cn)))
            )

            ; if [4 0 4 ...] -> which syncope ?
            ((and (= sn 4) (= sn+1 0) (= sn+2 4))
            (if (/= cn cn+2) ; syncopation but different notes ?
                ; then same as half note
                (if (= sn+3 3)
                    ; then 1/2 + 1/4 if [4 0 4 3] (syncopation catch up by a quarter note)
                    (build-rythmic-pattern
                        (nthcdr 3 species-arr)
                        (nthcdr 3 cp-arr)
                        (append rythmic-arr (list 1/2 1/4))
                        (append notes-arr (list cn cn+2))
                    )
                    ; else 1/2 + 1/2 if [4 0 4 0] (basic syncopation)
                    (build-rythmic-pattern
                        (nthcdr 4 species-arr)
                        (nthcdr 4 cp-arr)
                        (append rythmic-arr (list 1/2 1/2))
                        (append notes-arr (list cn cn+2))
                    )
                )
                ; else same as full note syncopated
                (if (= sn+3 3)
                    ; then 3/4 if [4 0 4 3] (syncopation catch up by a quarter note)
                    (build-rythmic-pattern
                        (nthcdr 3 species-arr)
                        (nthcdr 3 cp-arr)
                        (append rythmic-arr (list 3/4))
                        (append notes-arr (list cn))
                    )
                    ; else 1 if [4 0 4 0] (basic syncopation)
                    (build-rythmic-pattern
                        (nthcdr 4 species-arr)
                        (nthcdr 4 cp-arr)
                        (append rythmic-arr (list 1))
                        (append notes-arr (list cn))
                    )
                )
            )
            )

            ; eighth note (croche) if cn == cn+1 AND [!0 3 ...]
            ((and (= cn cn+1) (/= sn 0) (= sn+1 3))
                (if (>= (lastone notes-arr) cn)
                    ; then eighth note with the next lower note
                    (build-rythmic-pattern
                        (nthcdr 1 species-arr)
                        (nthcdr 1 cp-arr)
                        (append rythmic-arr (list 1/8 1/8))
                        (append notes-arr (list cn (find-next-note cn 'lower)))
                    )
                    ; else eighth note with the next higher note
                    (build-rythmic-pattern
                        (nthcdr 1 species-arr)
                        (nthcdr 1 cp-arr)
                        (append rythmic-arr (list 1/8 1/8))
                        (append notes-arr (list cn (find-next-note cn 'higher)))
                    )
                )
            )

            ; silence if [0 0 ...]
            ((and (= sn 0) (= sn+1 0))
                (build-rythmic-pattern
                    (nthcdr 2 species-arr)
                    (nthcdr 2 cp-arr)
                    (append rythmic-arr (list -1/2))
                    notes-arr
                )
            )

            ; 1 if [1 0 0 0] (full note)
            ((and (= sn 1) (= sn+1 0) (= sn+2 0) (= sn+3 0))
                (build-rythmic-pattern
                    (nthcdr 4 species-arr)
                    (nthcdr 4 cp-arr)
                    (append rythmic-arr (list 1))
                    (append notes-arr (list cn))
                )
            )
            
            ; 1/2 if [2 0 ...] (half note)
            ((and (= sn 2) (= sn+1 0))
                (build-rythmic-pattern
                    (nthcdr 2 species-arr)
                    (nthcdr 2 cp-arr)
                    (append rythmic-arr (list 1/2))
                    (append notes-arr (list cn))
                )
            )

            ; 1/4 if [3 ...] (quarter note)
            ((= sn 3)
                (build-rythmic-pattern
                    (nthcdr 1 species-arr)
                    (nthcdr 1 cp-arr)
                    (append rythmic-arr (list 1/4))
                    (append notes-arr (list cn))
                )
            )

            ; 1/2 if [4 0 1 ...] (penultimate note for the 4th species)
            ((and (= sn 4) (= sn+1 0) (= sn+2 1))
                (build-rythmic-pattern
                    (nthcdr 2 species-arr)
                    (nthcdr 2 cp-arr)
                    (append rythmic-arr (list 1/2))
                    (append notes-arr (list cn))
                )
            )
        )
    )
)

; get the basic rythmic pattern for a given species
; - species: the species [1 2 3 4]
; - len: the length of the counterpoint
; examples:
; (1 5) -> (1 1 1 1 1)
; (2 5) -> (1/2 1/2 1/2 1/2 1/2 1/2 1/2 1/2 1)
; (3 5) -> (1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1)
; (4 5) -> ~(-1/2 1 1 1 1/2 1/2 1) depending on the counterpoint
(defun get-basic-rythmic (species len &optional (cp nil))
    (setq len-1 (- len 1))
    (setq len-2 (- len 2))
    (setq cp-len (+ (* 4 len-1) 1))
    (case species
        (1 (make-list len :initial-element 1))
        (2 (append (make-list (* 2 len-1) :initial-element 1/2) '(1)))
        (3 (append (make-list (* 4 len-1) :initial-element 1/4) '(1)))
        (4 (build-rythmic-pattern
                (get-4th-species-array len-2)
                (get-4th-notes-array cp cp-len)
        ))
    )
)

; return a species array for a 4th species counterpoint
; - len-2: the length of the counterpoint - 2
(defun get-4th-species-array (len-2)
    (append (list 0 0) (get-n-4040 len-2) (list 4 0 1))
)

; return a note array for a 4th species counterpoint
; - len: the length of the cantus firmus
(defun get-4th-notes-array (cp len)
    (let* (
        (notes (make-list len :initial-element 0)) ; notes that we don't care about can be 0
    )
        (loop
        for n from 2 below len by 2 ; we move from 4 to 4 (4 0 4 ...) after the silence (0 0) at the start
        for p in cp
        do
            (setf (nth n notes) p)
        )
        notes
    )
)

; return a list with n * (4 0 4 0), used to build the rythmic pattern for the 4th species
; - n: the number of times the pattern is repeated
(defun get-n-4040 (n)
    (if (= n 0)
        nil
        (append (list 4 0 4 0) (get-n-4040 (- n 1)))
    )
)

; tibo
; return the tone offset of the voice
; => [0, ...,  11]
; 0 = C, 1 = C#, 2 = D, 3 = D#, 4 = E, 5 = F, 6 = F#, 7 = G, 8 = G#, 9 = A, 10 = A#, 11 = B
; TODO: check if the tone is major or minor by checking the third element of the mode list (m:300 or M:400),
; if it's minor, then add 3 to the offset
(defun get-tone-offset (voice)
    (print (list "get-tone-offset..." voice))
    (let (
        (tone (om::tonalite voice))
    )
        ; (print tone)
        (if (eq tone nil)
            ; then
            0 ; default to C major
            ; else check if the mode is major or minor
            (let (
                (mode (om::mode tone))
            )
                (if (= (third mode) 300)
                    (midicent-to-midi-offset (+ (om::tonmidi tone) 300))
                    (midicent-to-midi-offset (om::tonmidi tone))
                )
            )
        )
    )
)

; tibo
; converts a midicent value to the corresponding offset midi value
; note:[0, 12700] -> [0, 11]
; 0 corresponds to C, 11 to B
(defun midicent-to-midi-offset (note)
    (print (list "midicent-to-midi-offset..." note))
    (mod (/ note 100) 12)
)

; tibo
; return the absolute difference between two midi notes modulo 12
; or the brut interval if b is true
(defun inter (n1 n2 &optional (b nil))
    (if b
        (- n1 n2)
        (mod (abs (- n1 n2)) 12)
    )
)

; tibo
; add constraint in sp such that the interval between the two notes is a member of interval-set
(defun inter-member-cst (sp n1-var n2-val interval-set)
    (let (
        (t1 (gil::add-int-var-expr sp n1-var gil::IOP_SUB n2-val)) ; t1 = n1 - n2
        (t2 (gil::add-int-var sp 0 127)) ; used to store the absolute value of t1
        note-inter
    )
        ; (print (list "inter-member-cst..." n1-var n2-val interval-set))
        ; (print (list "note-inter :" note-inter))
        (gil::g-abs sp t1 t2) ; t2 = |t1|
        ; (print "g-abs done")
        ; (gil::g-mod sp t1 modulo note-inter) ; note-inter = t1 % 12
        (setq note-inter (gil::add-int-var-expr sp t1 gil::IOP_MOD 12)) ; note-inter = t1 % 12
        ; (print "g-mod done")
        (gil::g-member sp interval-set note-inter) ; note-inter in interval-set
        ; (print "g-member done")
    )
)

; tibo
; add constraint such that n3-var = |n1-var - n2-val| % 12
(defun inter-eq-cst (sp n1-var n2-val n3-var)
    ; (print (list "inter-eq-cst..." n1-var n2-val n3-var))
    (let (
        (t1 (gil::add-int-var-expr sp n1-var gil::IOP_SUB n2-val)) ; t1 = n1 - n2
        (t2 (gil::add-int-var sp 0 127)) ; used to store the absolute value of t1
        (modulo (gil::add-int-var-dom sp '(12))) ; the IntVar just used to store 12
    )
        (gil::g-abs sp t1 t2) ; t2 = |t1|
        (gil::g-mod sp t2 modulo n3-var) ; n3-var = t2 % 12
    )
)

; tibo
; add constraint such that
; brut-var = n1-var - n2
; abs-var = |brut-var|
(defun inter-eq-cst-brut (sp n1-var n2 brut-var abs-var)
    (let (
        (t1 (gil::add-int-var-expr sp n1-var gil::IOP_SUB n2)) ; t1 = n1-var - n2
    )
        (gil::g-rel sp t1 gil::IRT_EQ brut-var) ; t1 = brut-var
        (gil::g-abs sp t1 abs-var) ; abs-var = |t1|
    )
)

; tibo
; return the last element of a list
(defun lastone (l)
    (first (last l))
)

; tibo
; return the rest of a list without its last element
(defun restbutlast (l)
    (butlast (rest l))
)

; tibo
; return the penultimate element of a list
(defun penult (l)
    ; (lastone (butlast l))
    (nth (- (length l) 2) l)
)

; build the list of acceptable pitch based on the scale and a key offset
(defun build-scaleset (scale offset)
    (let ((major-modified (adapt-scale scale))
          (scaleset (list)))
        (loop :for octave :from -1 :below 11 :by 1 append
              (setq scaleset (nconc scaleset (mapcar (lambda (n) (+ (+ n (* octave 12)) offset)) major-modified)))
        )
        (setq scaleset (remove-if 'minusp scaleset))
        ;; tibo: remove notes higher than 127
        (setq scaleset (remove 127 scaleset :test #'<))
    )
)

; returns the list of intervals defining a given mode
(defun get-scale (&optional (mode "ionian (major)"))
    (cond
        ((string-equal mode "ionian (major)")
            (list 2 2 1 2 2 2 1)
        )
        ((string-equal mode "dorian")
            (list 2 1 2 2 2 1 2)
        )
        ((string-equal mode "phrygian")
            (list 1 2 2 2 1 2 2)
        )
        ((string-equal mode "lydian")
            (list 2 2 2 1 2 2 1)
        )
        ((string-equal mode "mixolydian")
            (list 2 2 1 2 2 1 2)
        )
        ((string-equal mode "aeolian (natural minor)")
            (list 2 1 2 2 1 2 2)
        )
        ((string-equal mode "locrian")
            (list 1 2 2 1 2 2 2)
        )
        ((string-equal mode "harmonic minor")
            (list 2 1 2 2 1 3 1)
        )
        ((string-equal mode "pentatonic")
            (list 2 2 3 2 3)
        )
        ((string-equal mode "chromatic")
            (list 1 1 1 1 1 1 1 1 1 1 1 1)
        )
        ((string-equal mode "borrowed")
            (list 5 4 2 1)
        )
    )
)

; reformat a scale to be a canvas of pitch and not intervals
(defun adapt-scale (scale)
    (let ((major-modified (list (first scale))))
         (loop :for i :from 1 :below (length scale) :by 1 :do
            (setq major-modified (nconc major-modified (list (+ (nth i scale) (nth (- i 1) major-modified)))))
         )
    (return-from adapt-scale major-modified)
    )
)


; build the list of acceptable pitch based on the scale and a key offset
(defun build-notesets (chord offset)
    (let ((chord-modified (adapt-scale chord))
          (notesets (list)))
        (loop :for i :from 0 :below (length chord-modified) :by 1 :do
            (setq noteset (list))
            (loop :for octave :from -1 :below 11 :by 1 append
                  (setq noteset (nconc noteset (list (+ (+ (nth i chord-modified) (* octave 12)) offset))))
            )
            (setq noteset (remove-if 'minusp noteset))
            (setq notesets (nconc notesets (list noteset)))
        )
        notesets
    )
)

; <chords> a list of chord object
; Return the list of pitch contained in chords in midi format
(defun to-pitch-list (chords)
     (loop :for n :from 0 :below (length chords) :by 1 collect (to-midi (om::lmidic (nth n chords))))
)

; converts a list of MIDI values to MIDIcent
(defun to-midicent (l)
    (if (null l)
        nil
        (cons (* 100 (first l)) (to-midicent (rest l)))
    )
)

; convert from MIDIcent to MIDI
(defun to-midi (l)
    (if (null l)
        nil
        (cons (/ (first l) 100) (to-midi (rest l)))
    )
)

; create a list from min to max by step
(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))