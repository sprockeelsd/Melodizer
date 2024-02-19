(in-package :fuxcp)

; Author: Thibault Wafflard
; Date: June 3, 2023
; This file contains all the functions adding constraints to the CSP.
; They are all called from the different species.


;================================================ CP CONSTRAINTS UTILS ============================


; add a single cost regarding if the relation rel-type(tested, cst-val) is true
(defun add-single-cost-cst (tested rel-type cst-val cost &optional (cost-value ONE))
    (let (
        (b (gil::add-bool-var *sp* 0 1)) ; to store the result of the test
    )
        (gil::g-rel-reify *sp* tested rel-type cst-val b) ; test the relation
        (gil::g-ite *sp* b cost-value ZERO cost) ; add the cost if the test is true
    )
)

; add a cost regarding if the relation rel-type(tested-var, cst-val) is true
(defun add-cost-cst (tested-var-arr rel-type cst-val costs &optional (cost-value ONE))
    (loop
        for cost in costs
        for tested in tested-var-arr
        do
            (add-single-cost-cst tested rel-type cst-val cost cost-value)
    )
)

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

; add a cost regarding if the relation rel-type(tested-var, cst-val) is true AND is-cst is true
(defun add-cost-cst-if (tested-var-arr rel-type cst-val is-cst-arr costs &optional (cost-value ONE))
    (loop
        for cost in costs
        for tested in tested-var-arr
        for is-cst in is-cst-arr
        do
            (add-single-cost-cst-if tested rel-type cst-val is-cst cost cost-value)
    )
)

(defun add-single-cost-cst-if (tested rel-type cst-val is-cst cost cost-value)
    (let (
        (b (gil::add-bool-var *sp* 0 1)) ; to store the result of the test
        (b-and (gil::add-bool-var *sp* 0 1)) ; b and cst
    )
        (gil::g-rel-reify *sp* tested rel-type cst-val b)
        (gil::g-op *sp* b gil::BOT_AND is-cst b-and) ; b-and = b and cst
        (gil::g-ite *sp* b-and cost-value ZERO cost) ; add the cost if the test is true
    )
)

; add a cost regarding if the booleans are true in bool-arr
(defun add-cost-bool-cst (bool-arr costs &optional (cost-value ONE))
    (loop
        for b in bool-arr
        for cost in costs
        do
            (gil::g-ite *sp* b cost-value ZERO cost)
    )
)

; add a cost regarding if the booleans are true in bool-arr AND if is-cst is true in is-cst-arr
(defun add-cost-bool-cst-if (bool-arr is-cst-arr costs &optional (cost-value ONE))
    (loop
        for b in bool-arr
        for cst in is-cst-arr
        for cost in costs
        do
            (add-single-cost-bool-cst-if b cst cost cost-value)
    )
)

; add a cost regarding if b is true AND if cst is true
(defun add-single-cost-bool-cst-if (b cst cost cost-value)
    (let (
        (b-and (gil::add-bool-var *sp* 0 1)) ; b and cst
    )
        (gil::g-op *sp* b gil::BOT_AND cst b-and) ; b-and = b and cst
        (gil::g-ite *sp* b-and cost-value ZERO cost) ; add the cost if the test is true
    )
)

; add a cost regarding only if b AND cst are true (do not force ZERO if false)
(defun add-single-cost-bool-cst-eqv (b cst cost cost-value)
    (let (
        (b-and (gil::add-bool-var *sp* 0 1)) ; b and cst
    )
        (gil::g-op *sp* b gil::BOT_AND cst b-and) ; b-and = b and cst
        (gil::g-rel-reify *sp* cost gil::IRT_EQ cost-value b-and gil::RM_IMP) ; add the cost if the test is true
    )
)

; add constraints such that costs =
;   - 0 if m-degree in [0, 1, 2]
;   - 1 if m-degree in [3, 4, 12]
;   - 2 otherwise
; @m-all-intervals: all the melodic intervals of cp in a row
; @m-degrees-cost: the cost of each melodic interval
(defun add-m-degrees-cost-cst (m-all-intervals m-degrees-cost m-degrees-type &optional (is-cst-arr nil))
    (loop
    for m in m-all-intervals
    for c in m-degrees-cost
    for d in m-degrees-type
    do
        (let (
            (b-l3 (gil::add-bool-var *sp* 0 1)) ; true if m < 3
            (b-3 (gil::add-bool-var *sp* 0 1)) ; true if m == 3
            (b-4 (gil::add-bool-var *sp* 0 1)) ; true if m == 4
            (b-34 (gil::add-bool-var *sp* 0 1)) ; true if m in [3, 4]
            (b-5 (gil::add-bool-var *sp* 0 1)) ; true if m == 5
            (b-6 (gil::add-bool-var *sp* 0 1)) ; true if m == 6
            (b-7 (gil::add-bool-var *sp* 0 1)) ; true if m == 7
            (b-8 (gil::add-bool-var *sp* 0 1)) ; true if m == 8
            (b-9 (gil::add-bool-var *sp* 0 1)) ; true if m == 9
            (b-89 (gil::add-bool-var *sp* 0 1)) ; true if m in [8, 9]
            (b-10 (gil::add-bool-var *sp* 0 1)) ; true if m == 10
            (b-11 (gil::add-bool-var *sp* 0 1)) ; true if m == 11
            (b-1011 (gil::add-bool-var *sp* 0 1)) ; true if m in [10, 11]
            (b-12 (gil::add-bool-var *sp* 0 1)) ; true if m == 12
        )
            (gil::g-rel-reify *sp* m gil::IRT_LE 3 b-l3) ; m < 3
            (gil::g-rel-reify *sp* m gil::IRT_EQ 3 b-3) ; m = 3
            (gil::g-rel-reify *sp* m gil::IRT_EQ 4 b-4) ; m = 4
            (gil::g-op *sp* b-3 gil::BOT_OR b-4 b-34) ; m in [3, 4]
            (gil::g-rel-reify *sp* m gil::IRT_EQ 5 b-5) ; m = 5
            (gil::g-rel-reify *sp* m gil::IRT_EQ 6 b-6) ; m = 6
            (gil::g-rel-reify *sp* m gil::IRT_EQ 7 b-7) ; m = 7
            (gil::g-rel-reify *sp* m gil::IRT_EQ 8 b-8) ; m = 8
            (gil::g-rel-reify *sp* m gil::IRT_EQ 9 b-9) ; m = 9
            (gil::g-op *sp* b-8 gil::BOT_OR b-9 b-89) ; m in [8, 9]
            (gil::g-rel-reify *sp* m gil::IRT_EQ 10 b-10) ; m = 10
            (gil::g-rel-reify *sp* m gil::IRT_EQ 11 b-11) ; m = 11
            (gil::g-op *sp* b-10 gil::BOT_OR b-11 b-1011) ; m in [10, 11]
            (gil::g-rel-reify *sp* m gil::IRT_EQ 12 b-12) ; m = 12
            ; set costs
            (gil::g-rel-reify *sp* c gil::IRT_EQ *m-step-cost* b-l3 gil::RM_IMP)
            (gil::g-rel-reify *sp* c gil::IRT_EQ *m-third-cost* b-34 gil::RM_IMP)
            (gil::g-rel-reify *sp* c gil::IRT_EQ *m-fourth-cost* b-5 gil::RM_IMP)
            (gil::g-rel-reify *sp* c gil::IRT_EQ *m-tritone-cost* b-6 gil::RM_IMP)
            (gil::g-rel-reify *sp* c gil::IRT_EQ *m-fifth-cost* b-7 gil::RM_IMP)
            (gil::g-rel-reify *sp* c gil::IRT_EQ *m-sixth-cost* b-89 gil::RM_IMP)
            (gil::g-rel-reify *sp* c gil::IRT_EQ *m-seventh-cost* b-1011 gil::RM_IMP)
            (gil::g-rel-reify *sp* c gil::IRT_EQ *m-octave-cost* b-12 gil::RM_IMP)
            ; set types
            (gil::g-rel-reify *sp* d gil::IRT_EQ 2 b-l3 gil::RM_IMP)
            (gil::g-rel-reify *sp* d gil::IRT_EQ 3 b-34 gil::RM_IMP)
            (gil::g-rel-reify *sp* d gil::IRT_EQ 4 b-5 gil::RM_IMP)
            (gil::g-rel-reify *sp* d gil::IRT_EQ 1 b-6 gil::RM_IMP)
            (gil::g-rel-reify *sp* d gil::IRT_EQ 5 b-7 gil::RM_IMP)
            (gil::g-rel-reify *sp* d gil::IRT_EQ 6 b-89 gil::RM_IMP)
            (gil::g-rel-reify *sp* d gil::IRT_EQ 7 b-1011 gil::RM_IMP)
            (gil::g-rel-reify *sp* d gil::IRT_EQ 8 b-12 gil::RM_IMP)
        )
    )
)

; add cost constraints such that a cost is added when a fifth or an octave is present in the 1st beat
; except for the 4th species where it is the 3rd beat
; @is-sync: true means it is the 4th species
(defun add-p-cons-cost-cst (&optional (is-sync nil))
    (setq *fifth-cost  (gil::add-int-var-array-dom *sp* *cf-penult-index (getparam-dom 'h-fifth-cost))) ; IntVar array representing the cost to have fifths
    (setq *octave-cost (gil::add-int-var-array-dom *sp* *cf-penult-index (getparam-dom 'h-octave-cost))) ; IntVar array representing the cost to have octaves
    (if is-sync
        ; then 4th species
        (add-h-inter-cost-cst (rest (third *h-intervals)))
        ; else
        (add-h-inter-cost-cst (restbutlast (first *h-intervals)))
    )
    (add-cost-to-factors *fifth-cost)
    (add-cost-to-factors *octave-cost)
)

; add cost constraints such that a cost is added when a fifth or an octave is present in @h-intervals
(defun add-h-inter-cost-cst (h-intervals)
        (add-cost-cst h-intervals gil::IRT_EQ 7 *fifth-cost *h-fifth-cost*) ; *fifth-cost = 1 if *h-interval == 7
        (add-cost-cst h-intervals gil::IRT_EQ 0 *octave-cost *h-octave-cost*) ; *octave-cost = 1 if *h-interval == 0
)

; Get the minimum cost possible for a counterpoint depending on the costs of the melodic intervals
; @m-len: number of melodic intervals
(defun get-min-m-cost (m-len)
    ; get the minimum cost for skips
    (setq min-skip-cost (min
        (getparam 'm-third-cost)
        (getparam 'm-fourth-cost)
        (getparam 'm-tritone-cost)
        (getparam 'm-fifth-cost)
        (getparam 'm-sixth-cost)
        (getparam 'm-seventh-cost)
        (getparam 'm-octave-cost)
    ))
    ; get the minimum number of skips
    (setq int-min-skip (ceiling (* (getparam 'min-skips-slider) m-len)))
    ; return the minimum cost
    (+ 
        (* int-min-skip min-skip-cost)
        (* (- m-len int-min-skip) (min (getparam 'm-step-cost) min-skip-cost))
    )
)

; setup the cost factors with the minimum cost possible
(defun set-cost-factors ()
    (setq m-len (length *m-all-intervals))
    (setq lb-max (max (ceiling (/ *cf-len 4)) (get-min-m-cost m-len)))
    ; (print (list "lb-max: " lb-max))
    (setq lb-i (floor (* (getparam 'irreverence-slider) (* 2 m-len))))
    ; (print (list "lb-i: " lb-i))
    (defparameter COST_LB (+ lb-max lb-i))
    ; (print '("COST_LB: " COST_LB))
    ; IntVar array representing all the cost factors
    (setq *cost-factors (gil::add-int-var-array *sp* *N-COST-FACTORS 0 COST_UB))
    ; IntVar representing the *total *cost
    (setq *total-cost (gil::add-int-var *sp* COST_LB COST_UB))
    (print 'debug123)
)

; add general costs for most of the species
(defun set-general-costs-cst (&optional (cp-len *total-cp-len) (is-cst-arr1 nil) (is-cst-arr2 nil))
    (let (
        (m-len (- cp-len 1))
    )
        ; 2) sharps and flats should be used sparingly
        (print "Sharps and flats should be used sparingly...")
        (setq *off-key-cost (gil::add-int-var-array-dom *sp* cp-len (getparam-dom 'borrow-cost))) ; IntVar array representing the cost to have off-key notes
        (if (null is-cst-arr1)
            ; then
            (add-cost-bool-cst *is-cp-off-key-arr *off-key-cost *borrow-cost*)
            ; else
            (add-cost-bool-cst-if *is-cp-off-key-arr is-cst-arr1 *off-key-cost *borrow-cost*)
        )
        ; sum of the cost of the off-key notes
        (add-cost-to-factors *off-key-cost)

        ; 3) melodic intervals should be as small as possible
        (print "Melodic intervals should be as small as possible...")
        ; IntVar array representing the cost to have melodic large intervals
        (setq degrees-cost-domain
            (remove-duplicates (mapcar (lambda (x) (getparam x))
                (list 'm-step-cost 'm-third-cost 'm-fourth-cost 'm-tritone-cost 'm-fifth-cost 'm-sixth-cost 'm-seventh-cost 'm-octave-cost)
            ))
        )
        (setq *m-degrees-cost (gil::add-int-var-array-dom *sp* m-len degrees-cost-domain))
        (setq *m-degrees-type (gil::add-int-var-array *sp* m-len 1 8))
        (add-m-degrees-cost-cst *m-all-intervals *m-degrees-cost *m-degrees-type is-cst-arr2)
        (add-cost-to-factors *m-degrees-cost)
        (gil::g-count *sp* *m-degrees-type 2 gil::IRT_LQ (floor (* (- 1 (getparam 'min-skips-slider)) m-len)))
    )
)

; merge lists intermittently such that the first element of the first list is followed by the first element of the second list, etc.
; attention: cp-len is lenght of the first list in cp-list and it should be 1 more than the lenght of the other lists
(defun merge-cp (cp-list total-cp)
    (let (
        (cp-len-1 (- (length (first cp-list)) 1))
        (n-list (length cp-list))
    )
        (loop
        for i from 0 below cp-len-1
        do
            (loop for j from 0 below n-list do
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
    )
        (loop
        for i from 0 below cp-len
        do
            (loop for j from 0 below n-list do
                (setf (nth (+ (* i n-list) j) total-cp) (nth i (nth j cp-list)))
            )
        )
    )
)

; create the harmonic intervals between @cp and @cf in @h-intervals
(defun create-h-intervals (cp cf h-intervals)
    (loop
        for p in cp
        for q in cf
        for i in h-intervals do
            (inter-eq-cst *sp* p q i) ; add a constraint to *sp* such that i = |p - q| % 12
    )
)

; create the intervals between @line1 and @line2 in @intervals and @brut-intervals
(defun create-intervals (line1 line2 intervals brut-intervals)
    (loop
        for p in line1
        for q in line2
        for i in intervals
        for ib in brut-intervals
        do
            (inter-eq-cst-brut *sp* q p ib i) ; add a constraint to *sp* such that ib = p - q and i = |ib|
    )
)

; create the intervals between @line1 and @line2 in @intervals and @brut-intervals where @is-cst-arr is true
(defun create-intervals-for-cst (line1 line2 intervals brut-intervals is-cst-arr)
    (loop
        for p in line1
        for q in line2
        for i in intervals
        for ib in brut-intervals
        for is-cst in is-cst-arr
        do
            (inter-eq-cst-brut-for-cst *sp* q p ib i is-cst) ; add a constraint to *sp* such that ib = p - q and i = |ib|
    )
)

; create the melodic intervals of @cp in @m-intervals and @m-intervals-brut
; @is-cst-arr is a list of booleans indicating whether the melodic interval is constrained or not
(defun create-m-intervals-self (cp m-intervals m-intervals-brut &optional (is-cst-arr nil))
    (if is-cst-arr
        ; then
        (create-intervals-for-cst (butlast cp) (rest cp) m-intervals m-intervals-brut is-cst-arr)
        ; else
        (create-intervals (butlast cp) (rest cp) m-intervals m-intervals-brut)
    )
)

; create an array of IntVar with the melodic interval between each arsis and its following thesis
(defun create-m-intervals-next-meas (cp-arsis cp m-intervals-arsis m-intervals-arsis-brut)
    (create-intervals cp-arsis (rest cp) m-intervals-arsis m-intervals-arsis-brut)
)

; create the melodic intervals two positions apart of @cp in @m2-intervals and @m2-intervals-brut 
(defun create-m2-intervals (cp m2-intervals m2-intervals-brut)
    (create-intervals (butlast (butlast cp)) (rest (rest cp)) m2-intervals m2-intervals-brut)
)

; create the melodic intervals between the thesis of @cp and the arsis of @cp-arsis in @m-intervals and @m-intervals-brut
(defun create-m-intervals-in-meas (cp cp-arsis ta-intervals ta-intervals-brut)
    (create-intervals (butlast cp) cp-arsis ta-intervals ta-intervals-brut)
)

; create the brut melodic intervals of @cf in @cf-brut-m-intervals
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

; create the boolean array @is-p-cons-arr indicating if the interval is a perfect consonance or not
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

; create the boolean array @is-cf-bass-arr indicating if the cantus firmus is the bass or not
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
                (bta-second (gil::add-bool-var *sp* 0 1)) ; for mat <= 2
                (btt-third (gil::add-bool-var *sp* 0 1)) ; for mtt == 3 or 4
                (bat-second (gil::add-bool-var *sp* 0 1)) ; for mta <= 2
                (b-and (gil::add-bool-var *sp* 0 1)) ; temporary BoolVar
            )
                (gil::g-rel-reify *sp* mtt gil::IRT_EQ 3 btt3) ; btt3 = (mtt == 3)
                (gil::g-rel-reify *sp* mtt gil::IRT_EQ 4 btt4) ; btt4 = (mtt == 4)
                (gil::g-rel-reify *sp* mta gil::IRT_LQ 2 bta-second) ; bta2 = (mta <= 2)
                (gil::g-rel-reify *sp* mat gil::IRT_LQ 2 bat-second) ; bat1 = (mat <= 2)
                (gil::g-op *sp* btt3 gil::BOT_OR btt4 btt-third) ; btt-third = btt3 || btt4
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
        (if (eq (mod i 4) 0)
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
            (gil::g-rel-reify *sp* m gil::IRT_LQ 2 b-m) ; b-m = (m <= 2)
            (gil::g-op *sp* b-and gil::BOT_AND b-m b) ; b = b-and && b-m
        )
    )
)

; create an array of BoolVar representing if there is no syncopation
(defun create-is-no-syncope-arr (m-intervals is-no-syncope-arr)
    (loop
    for m in (butlast m-intervals)
    for b in is-no-syncope-arr
    do
        (gil::g-rel-reify *sp* m gil::IRT_NQ 0 b)
    )
)

; add constraints such that @b-member is true iff @candidate is a member of @member-list
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

; add the constraint such that the harmonies in @h-intervals are consonances expect the penultimate note (specific rule)
; @len: the length of the counterpoint
; @cf-penult-index: the index of penultimate note in the counterpoint
; @h-intervals: the array of harmonic intervals
; @penult-dom-var: the domain of the penultimate note
(defun add-h-cons-cst (len cf-penult-index h-intervals &optional (penult-dom-var PENULT_CONS_VAR))
    (loop for i from 0 below len do
        (setq h-interval (nth i h-intervals))
        (if (eq i cf-penult-index) ; if it is the penultimate note
            ; then add major sixth + minor third by default
            (add-penult-dom-cst h-interval penult-dom-var)
            ; else add all consonances
            (if (not (null h-interval))
                (gil::g-member *sp* ALL_CONS_VAR h-interval)
            )
        )
    )
)

; add the constraint such that the penultimate note belongs to the domain @penult-dom-var
(defun add-penult-dom-cst (h-interval penult-dom-var)
    (if (getparam 'penult-rule-check)
        (gil::g-member *sp* penult-dom-var h-interval)
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

; add the constraint such that h-intervals[i] belongs to ALL_CONS_VAR is-no-syncope-arr[i] is true
; in other words, if there is no syncopation the note cannot be dissonant
(defun add-no-sync-h-cons (h-intervals is-no-syncope-arr)
    (loop
    for h in h-intervals
    for b in is-no-syncope-arr
    do
        (loop for d in DIS do
            (gil::g-rel-reify *sp* h gil::IRT_NQ d b gil::RM_IMP) ; b => (h != d)
        )
    )
)

; for future work: should use not(nth i is-cons-arr) instead of add a constraint for each dissonance in DIS
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
        (if (eq i cf-penult-index) ; if it is the penultimate note
            ; then add major sixth + minor third
            (add-penult-dom-cst (nth i h-intervals-arsis) penult-dom-var)
            ; else dissonance implies there is a diminution
            (loop for d in DIS do
                (gil::g-rel-reify *sp* (nth i h-intervals-arsis) gil::IRT_EQ d b gil::RM_PMI)
            )
        )
    )
)

; add the constraint such that (c3 OR (c2 AND c4)) AND (c3 OR dim) is true,
; where : - cn represents if the nth note of the measure is consonant
;         - dim represents if the 3rd note is a diminution
(defun add-h-dis-or-cons-3rd-cst (is-cons-2nd is-cons-3rd is-cons-4th is-dim &optional (is-cst-arr nil))
    (loop
    for b-c2nd in is-cons-2nd
    for b-c3rd in is-cons-3rd
    for b-c4th in is-cons-4th
    for b-dim in is-dim
    do
        (let (
            (b-and1 (gil::add-bool-var *sp* 0 1)) ; s.f. b-c2nd AND b-c4th
        )
            (gil::g-op *sp* b-c2nd gil::BOT_AND b-c4th b-and1) ; b-and1 = b-c2nd AND b-c4th
            (gil::g-op *sp* b-c3rd gil::BOT_OR b-dim 1) ; b-and2 = b-c2nd AND b-c4th AND b-dim
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

; add constraints such that if a melodic interval is greater than one step (2)
; then the next melodic interval should be one step and in the opposite direction
(defun add-contrary-step-after-skip-cst (m-all-intervals m-all-intervals-brut)
    (if (not (getparam 'con-m-after-skip-check))
        (return-from add-contrary-step-after-skip-cst)
    )
    (loop
    for m in m-all-intervals
    for m+1 in (rest m-all-intervals)
    for mb in m-all-intervals-brut
    for mb+1 in (rest m-all-intervals-brut)
    do
        (let (
            (b-skip (gil::add-bool-var *sp* 0 1)) ; m > 2
            (b-mb-up (gil::add-bool-var *sp* 0 1)) ; mb > 0
            (b-mb+1-down (gil::add-bool-var *sp* 0 1)) ; mb+1 < 0
            (b-contrary (gil::add-bool-var *sp* 0 1)) ; b-mb-up <=> b-mb+1-down
        )
            (gil::g-rel-reify *sp* m gil::IRT_GR 2 b-skip) ; b-skip := m > 2
            (gil::g-rel-reify *sp* mb gil::IRT_GR 0 b-mb-up) ; b-mb-up := mb > 0
            (gil::g-rel-reify *sp* mb+1 gil::IRT_LE 0 b-mb+1-down) ; b-mb+1-down := mb+1 < 0
            (gil::g-op *sp* b-mb-up gil::BOT_EQV b-mb+1-down b-contrary) ; b-contrary := b-mb-up <=> b-mb+1-down
            (gil::g-rel-reify *sp* m+1 gil::IRT_LQ 2 b-skip gil::RM_IMP) ; b-skip => m+1 <= 2
            (gil::g-op *sp* b-skip gil::BOT_IMP b-contrary 1) ; b-skip => b-contrary
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
            (rel-reify-if p gil::IRT_NQ q (nth i is-cst-arr))
    )
)

; add the constraint such that there is no unisson unless it is the first or last note
(defun add-no-unisson-cst (cp cf)
    (add-no-unisson-at-all-cst (restbutlast cp) (restbutlast cf))
)

; add the constraint such that the first harmonic interval is a perfect consonance
(defun add-p-cons-start-cst (h-intervals)
    (gil::g-member *sp* P_CONS_VAR (first h-intervals))
)

; add the constraint such that the last harmonic interval is a perfect consonance
(defun add-p-cons-end-cst (h-intervals)
    (gil::g-member *sp* P_CONS_VAR (lastone h-intervals))
)

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
    (if (getparam 'penult-rule-check)
        (if (null and-cond)
            (gil::g-ite *sp* b-bass NINE THREE h-interval)
            (and-ite b-bass NINE THREE h-interval and-cond)
        )  
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
            (gil::g-rel-reify *sp* it gil::IRT_NQ 1 b-and-cst gil::RM_IMP)
            (gil::g-rel-reify *sp* it gil::IRT_NQ 2 b-and-cst gil::RM_IMP)
        )
    )
)

; add a constraint such that there is no melodic interval greater than @jump (8, minor 6th by default)
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

(defun create-motions (m-intervals-brut cf-brut-m-intervals motions costs)
    (loop
        for p in m-intervals-brut
        for q in cf-brut-m-intervals
        for m in motions
        for c in costs
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
                (gil::g-rel-reify *sp* p gil::IRT_LE 0 b-pd) ; b-pd = (p < 0)
                (gil::g-rel-reify *sp* p gil::IRT_EQ 0 b-ps) ; b-ps = (p == 0)
                (gil::g-rel-reify *sp* p gil::IRT_GR 0 b-pu) ; b-pu = (p > 0)
                (gil::g-rel-reify *sp* q gil::IRT_LE 0 b-qd) ; b-qd = (q < 0)
                (gil::g-rel-reify *sp* q gil::IRT_EQ 0 b-qs) ; b-qs = (q == 0)
                (gil::g-rel-reify *sp* q gil::IRT_GR 0 b-qu) ; b-qu = (q > 0)
                ; direct motion
                (gil::g-op *sp* b-pu gil::BOT_AND b-qu b-both-up) ; b-both-up = (b-pu and b-qu)
                (gil::g-op *sp* b-ps gil::BOT_AND b-qs b-both-stays) ; b-both-stays = (b-ps and b-qs)
                (gil::g-op *sp* b-pd gil::BOT_AND b-qd b-both-down) ; b-both-down = (b-pd and b-qd)
                (gil::g-op *sp* b-both-up gil::BOT_OR b-both-stays dm-or1) ; dm-or1 = (b-both-up or b-both-stays)
                (gil::g-op *sp* dm-or1 gil::BOT_OR b-both-down dm-or2) ; dm-or2 = (dm-or1 or b-both-down)
                (gil::g-rel-reify *sp* m gil::IRT_EQ DIRECT dm-or2) ; m = 1 if dm-or2
                (gil::g-rel-reify *sp* c gil::IRT_EQ *dir-motion-cost* dm-or2) ; add the cost of direct motion
                ; oblique motion
                (gil::g-op *sp* b-pu gil::BOT_AND b-qs b-pu-qs) ; b-pu-qs = (b-pu and b-qs)
                (gil::g-op *sp* b-pd gil::BOT_AND b-qs b-pd-qs) ; b-pd-qs = (b-pd and b-qs)
                (gil::g-op *sp* b-ps gil::BOT_AND b-qu b-ps-qu) ; b-ps-qu = (b-ps and b-qu)
                (gil::g-op *sp* b-ps gil::BOT_AND b-qd b-ps-qd) ; b-ps-qd = (b-ps and b-qd)
                (gil::g-op *sp* b-pu-qs gil::BOT_OR b-pd-qs om-or1) ; om-or1 = (b-pu-qs or b-pd-qs)
                (gil::g-op *sp* om-or1 gil::BOT_OR b-ps-qu om-or2) ; om-or2 = (om-or1 or b-ps-qu)
                (gil::g-op *sp* om-or2 gil::BOT_OR b-ps-qd om-or3) ; om-or3 = (om-or2 or b-ps-qd)
                (gil::g-rel-reify *sp* m gil::IRT_EQ OBLIQUE om-or3) ; m = 0 if om-or3
                (gil::g-rel-reify *sp* c gil::IRT_EQ *obl-motion-cost* om-or3) ; add the cost of oblique motion
                ; contrary motion
                (gil::g-op *sp* b-pu gil::BOT_AND b-qd b-pu-qd) ; b-pu-qd = (b-pu and b-qd)
                (gil::g-op *sp* b-pd gil::BOT_AND b-qu b-pd-qu) ; b-pd-qu = (b-pd and b-qu)
                (gil::g-op *sp* b-pu-qd gil::BOT_OR b-pd-qu cm-or1) ; cm-or1 = (b-pu-qd or b-pd-qu)
                (gil::g-rel-reify *sp* m gil::IRT_EQ CONTRARY cm-or1) ; m = -1 if cm-or1
                (gil::g-rel-reify *sp* c gil::IRT_EQ *con-motion-cost* cm-or1) ; add the cost of contrary motion
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
(defun create-real-motions (m-intervals-ta motions motions-arsis real-motions motions-costs motions-arsis-costs real-motions-costs)
    (loop
        for tai in m-intervals-ta
        for t-move in motions
        for a-move in motions-arsis
        for r-move in real-motions
        for t-c in motions-costs
        for a-c in motions-arsis-costs
        for r-c in real-motions-costs
        do
            (let (
                (b (gil::add-bool-var *sp* 0 1)) ; for (tai > 4)
            )
                (gil::g-rel-reify *sp* tai gil::IRT_GR 4 b) ; b = (tai > 4)
                (gil::g-ite *sp* b a-move t-move r-move) ; r-move = (b ? a-move : t-move)
                (gil::g-ite *sp* b a-c t-c r-c) ; r-c = (b ? a-c : t-c)
            )
    )
)

; add the constraint such that there is no perfect consonance in thesis that is reached by direct motion
(defun add-no-direct-move-to-p-cons-cst (motions is-p-cons-arr &optional (r t))
    (loop
        for m in motions
        for b in (rest-if is-p-cons-arr r)
        do
            (gil::g-rel-reify *sp* m gil::IRT_NQ DIRECT b gil::RM_IMP)
    )
)

; return the rest of the list if the boolean is true, else return the list
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

; TEST new version
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
(defun collect-bot-array (b-arr1 b-arr2 &optional (bot gil::BOT_AND))
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
        b-collect-arr
    )
)


(defun collect-t-or-f-array (yes-arr no-arr)
    (collect-bot-array
                yes-arr
                (collect-not-array no-arr)
                gil::BOT_OR
    )
)

(defun collect-not-array (arr)
    (collect-bot-array arr (gil::add-bool-var-array *sp* (length arr) 0 0) gil::BOT_EQV)
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
    (setq len (if (eq offset 0) *cf-len *cf-last-index))
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
        (gil::g-rel *sp* (nth i arr) gil::IRT_EQ (nth j ret))
    )
    ret
)

; create an array for one beat from the entire array
(defun create-by-4 (arr-from arr-to &optional (offset 0))
    (loop
    for i from offset below (length arr-from) by 4
    for j in arr-to
    do
        (gil::g-rel *sp* (nth i arr-from) gil::IRT_EQ j)
    )
)

; add a reify constraint if @b is not nil, else add a rel constraint
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
    (loop
    for sp3 in sp-arr3
    for sp4 in sp-arr4
    for sp1 in (rest sp-arr1)
    do
        (let (
            (b-34 (gil::add-bool-var *sp* 0 1)) ; b-34 = sp3 == 4th species
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

; add the constraint such that there is maximum 2 consecutive measures without 4th species
(defun add-min-syncope-cst (third-sp-arr)
    (loop
    for sp1 in (nthcdr 1 third-sp-arr)
    for sp2 in (nthcdr 2 third-sp-arr)
    for sp3 in (nthcdr 3 third-sp-arr)
    do
        (let (
            (b1-not-4 (gil::add-bool-var *sp* 0 1)) ; b1-not-4 = sp1 != 4
            (b2-not-4 (gil::add-bool-var *sp* 0 1)) ; b2-not-4 = sp2 != 4
            (b-and (gil::add-bool-var *sp* 0 1)) ; b-and = b1-not-4 && b2-not-4
        )
            (gil::g-rel-reify *sp* sp1 gil::IRT_NQ 4 b1-not-4) ; b1-not-4 = sp1 != 4
            (gil::g-rel-reify *sp* sp2 gil::IRT_NQ 4 b2-not-4) ; b2-not-4 = sp2 != 4
            (gil::g-op *sp* b1-not-4 gil::BOT_AND b2-not-4 b-and) ; b-and = b1-not-4 && b2-not-4
            (gil::g-rel-reify *sp* sp3 gil::IRT_EQ 4 b-and gil::RM_IMP) ; b-and => sp3 == 4
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
(defun create-species-arr (species-arr &key (min-3rd-pc (* (- 1 (getparam 'pref-species-slider)) 0.66)) (min-4th-pc (* (getparam 'pref-species-slider) 0.5)))
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
        (add-no-silence-cst species-arr :spec 4 :offset 4)

        ; maximum two consecutive measures without 4th species
        (add-min-syncope-cst (third *sp-arr))
    )
)

; add constraints such that the non-constrained notes have only one possible value
(defun add-one-possible-value-cst (cp is-not-cst-arr)
    (loop
    for p in cp
    for p+1 in (nthcdr 1 cp)
    for b-not-cst in is-not-cst-arr
    do
        (gil::g-rel-reify *sp* p gil::IRT_EQ p+1 b-not-cst gil::RM_IMP) ; TODO the value of the note
    )
)

; add constraints such that consecutives syncopations cannot be the same
; depending on @is-syncope-arr which is true if the note is a syncopation
(defun add-no-same-syncopation-cst (cp-thesis cp-arsis is-syncope-arr)
    (loop
    for th in (rest cp-thesis)
    for ar in (rest cp-arsis)
    for b in (rest is-syncope-arr)
    do
        (gil::g-rel-reify *sp* th gil::IRT_NQ ar b gil::RM_IMP)
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
            ((and (eq sn 1) (eq sn+1 -1))
                (list (append rythmic-arr (list 1)) (append notes-arr (list cn)))
            )

            ; if [4 0 4 ...] -> which syncope ?
            ((and (eq sn 4) (eq sn+1 0) (eq sn+2 4))
            (if (/= cn cn+2) ; syncopation but different notes ?
                ; then same as half note
                (if (eq sn+3 3)
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
                (if (eq sn+3 3)
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
            ))

            ; 1/8 note (croche) if cn == cn+1 AND [!0 (3 or 4) ...]
            ((and (eq cn cn+1) (/= sn 0) (or (eq sn+1 3) (eq sn+1 4)))
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
            ((and (eq sn 0) (eq sn+1 0))
                (build-rythmic-pattern
                    (nthcdr 2 species-arr)
                    (nthcdr 2 cp-arr)
                    (append rythmic-arr (list -1/2))
                    notes-arr
                )
            )

            ; 1 if [1 0 0 0] (full note)
            ((and (eq sn 1) (eq sn+1 0) (eq sn+2 0) (eq sn+3 0))
                (build-rythmic-pattern
                    (nthcdr 4 species-arr)
                    (nthcdr 4 cp-arr)
                    (append rythmic-arr (list 1))
                    (append notes-arr (list cn))
                )
            )
            
            ; 1/2 if [2 0 ...] (half note)
            ((and (eq sn 2) (eq sn+1 0))
                (build-rythmic-pattern
                    (nthcdr 2 species-arr)
                    (nthcdr 2 cp-arr)
                    (append rythmic-arr (list 1/2))
                    (append notes-arr (list cn))
                )
            )

            ; 1/4 if [3 ...] (quarter note)
            ((eq sn 3)
                (build-rythmic-pattern
                    (nthcdr 1 species-arr)
                    (nthcdr 1 cp-arr)
                    (append rythmic-arr (list 1/4))
                    (append notes-arr (list cn))
                )
            )

            ; 1/2 if [4 0 1 ...] (penultimate note for the 4th species)
            ((and (eq sn 4) (eq sn+1 0) (eq sn+2 1))
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
    (if (eq n 0)
        nil
        (append (list 4 0 4 0) (get-n-4040 (- n 1)))
    )
)

; return the tone offset of the voice
; => [0, ...,  11]
; 0 = C, 1 = C#, 2 = D, 3 = D#, 4 = E, 5 = F, 6 = F#, 7 = G, 8 = G#, 9 = A, 10 = A#, 11 = B
(defun get-tone-offset (voice)
    (let (
        (tone (om::tonalite voice))
    )
        (if (eq tone nil)
            ; then default to C major
            0
            ; else check if the mode is major or minor
            (let (
                (mode (om::mode tone))
            )
                (if (eq (third mode) 300)
                    (midicent-to-midi-offset (+ (om::tonmidi tone) 300))
                    (midicent-to-midi-offset (om::tonmidi tone))
                )
            )
        )
    )
)

; converts a midicent value to the corresponding offset midi value
; note:[0, 12700] -> [0, 11]
; 0 corresponds to C, 11 to B
(defun midicent-to-midi-offset (note)
    (print (list "midicent-to-midi-offset..." note))
    (mod (/ note 100) 12)
)

; return the absolute difference between two midi notes modulo 12
; or the brut interval if b is true
(defun inter (n1 n2 &optional (b nil))
    (if b
        (- n1 n2)
        (mod (abs (- n1 n2)) 12)
    )
)

; add constraint in sp such that the interval between the two notes is a member of interval-set
(defun inter-member-cst (sp n1-var n2-val interval-set)
    (let (
        (t1 (gil::add-int-var-expr sp n1-var gil::IOP_SUB n2-val)) ; t1 = n1 - n2
        (t2 (gil::add-int-var sp 0 127)) ; used to store the absolute value of t1
        note-inter
    )
        (gil::g-abs sp t1 t2) ; t2 = |t1|
        (setq note-inter (gil::add-int-var-expr sp t1 gil::IOP_MOD 12)) ; note-inter = t1 % 12
        (gil::g-member sp interval-set note-inter) ; note-inter in interval-set
    )
)

; add constraint such that n3-var = |n1-var - n2-val| % 12
(defun inter-eq-cst (sp n1-var n2-val n3-var)
    (let (
        (t1 (gil::add-int-var-expr sp n1-var gil::IOP_SUB n2-val)) ; t1 = n1 - n2
        (t2 (gil::add-int-var sp 0 127)) ; used to store the absolute value of t1
        (modulo (gil::add-int-var-dom sp '(12))) ; the IntVar just used to store 12
    )
        (gil::g-abs sp t1 t2) ; t2 = |t1|
        (gil::g-mod sp t2 modulo n3-var) ; n3-var = t2 % 12
    )
)

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

; add constraint such that
; brut-var = n1-var - n2
; abs-var = |brut-var|
(defun inter-eq-cst-brut-for-cst (sp n1-var n2 brut-var abs-var is-cst)
    (let (
        (t1 (gil::add-int-var-expr sp n1-var gil::IOP_SUB n2)) ; t1 = n1-var - n2
        (t2 (gil::add-int-var sp 0 12)) ; store the absolute value of t1
    )
        (gil::g-abs sp t1 t2) ; t2 = |t1|
        (gil::g-ite sp is-cst t1 ZERO brut-var) ; brut-var = t1 if is-cst, else brut-var = 0
        (gil::g-ite sp is-cst t2 ZERO abs-var) ; abs-var = t2 if is-cst, else abs-var = 0
    )
)

; return the last element of a list
(defun lastone (l)
    (first (last l))
)

; return the rest of a list without its last element
(defun restbutlast (l)
    (butlast (rest l))
)

; return the penultimate element of a list
(defun penult (l)
    (nth (- (length l) 2) l)
)

; return an approximative checksum of pitches associated to a rythmic
; - p: the list of pitches
; - r: the list of rythmic values (with the -1/2 at the beginning)
(defun checksum-sol (p r)
    (let (
        (l (length p))
    )
        (mod (floor (reduce #'+
            (mapcar #'* (range (+ l 5) :min 5) (rest r) p)))
        (expt l 12))
    )
)

; add a the sum of the @factor-arr as a cost to the *cost-factors array and increment *n-cost-added
(defun add-cost-to-factors (factor-arr)
    (gil::g-sum *sp* (nth *n-cost-added *cost-factors) factor-arr)
    (incf *n-cost-added)
)