(cl:defpackage "gil"
  (:nicknames "GIL")
   (:use common-lisp :cl-user :cl :cffi))

(in-package :gil)

(print "Loading gecode-wrapper-ui...")

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Creating int variables ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass int-var ()
    ((id :initarg :id :accessor id))
)

(defmethod add-int-var (sp l h)
    "Adds a integer variable with domain [l,h] to sp"
    (make-instance 'int-var :id (add-int-var-low sp l h)))

(defmethod add-int-var-dom (sp dom)
    "Adds a integer variable with domain dom to sp"
    (make-instance 'int-var :id (add-int-var-dom-low sp dom)))

(defmethod add-int-var-array (sp n l h)
    "Adds an array of n integer variables with domain [l,h] to sp"
    (loop for v in (add-int-var-array-low sp n l h) collect
        (make-instance 'int-var :id v)))

(defmethod add-int-var-array-dom (sp n dom)
    "Adds an array of n integer variables with domain dom to sp"
    (loop for v in (add-int-var-array-dom-low sp n dom) collect
        (make-instance 'int-var :id v)))

(defmethod add-int-var-const-array (sp dom)
    "Adds a domain respresented by a list of integers to sp"
    (loop for v in dom collect
        (add-int-var-dom sp (list v))))

(defmethod g-specify-sol-variables (sp vids)
    "Specifies the variables that will contain the solution"
    (set-solution-vars sp (vid vids)))

(defmethod g-specify-percent-diff (sp diff)
    "Specifies the percent of modification when searching the next solution"
    (set-percent-diff sp diff))

;id getter
(defmethod vid ((self int-var))
    "Gets the vid of the variable self"
    (id self))

(defmethod vid ((self list))
    "Gets the vids of the variables in self"
    (loop for v in self collect (vid v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Creating bool variables ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bool-var ()
    ((id :initarg :id :accessor id))
)

(defmethod add-bool-var (sp l h)
    "Adds a boolean variable with domain [l,h] to sp"
    (make-instance 'bool-var :id (add-bool-var-range sp l h)))

(defmethod add-bool-var-array (sp n l h)
    "Adds an array of n boolean variables with domain [l,h] to sp"
    (loop for v in (add-bool-var-array-low sp n l h) collect
        (make-instance 'bool-var :id v)))

;id getter
(defmethod vid ((self bool-var)) (id self))

;;;;;;;;;;;;;;;;;;;;;;;;;;
; Creating set variables ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass set-var ()
    ((id :initarg :id :accessor id))
)

(defmethod add-set-var (sp lub-min lub-max card-min card-max)
    "Adds a set variable with minimum cardinality card-min and max card-max"
    (make-instance 'set-var :id (add-set-var-card sp lub-min lub-max card-min card-max)))

(defmethod add-set-var-array (sp n lub-min lub-max card-min card-max)
    "Adds an array of n set variables with cardinality card-min to card-max to sp"
    (loop for v in (add-set-var-array-card sp n lub-min lub-max card-min card-max) collect
        (make-instance 'set-var :id v)))

;id getter
(defmethod vid ((self set-var)) (id self))

(defmethod vid ((self list))
    "Gets the vids of the variables in self"
    (loop for v in self collect (vid v)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Methods for int constraints ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;REL
(defmethod g-rel (sp (v1 int-var) rel-type (v2 fixnum))
    "Post the constraint that v1 rel-type v2."
    (val-rel sp (vid v1) rel-type v2))

(defmethod g-rel (sp (v1 int-var) rel-type (v2 int-var))
    (var-rel sp (vid v1) rel-type (vid v2)))

(defmethod g-rel (sp (v1 list) rel-type (v2 null))
    (arr-rel sp (vid v1) rel-type))

(defmethod g-rel (sp (v1 list) rel-type (v2 fixnum))
    (arr-val-rel sp (vid v1) rel-type v2))

(defmethod g-rel (sp (v1 list) rel-type (v2 int-var))
    (arr-var-rel sp (vid v1) rel-type (vid v2)))

(defmethod g-rel (sp (v1 list) rel-type (v2 list))
    (arr-arr-rel sp (vid v1) rel-type (vid v2)))

(defmethod g-rel-reify (sp (v1 int-var) rel-type (v2 int-var) (v3 bool-var) &optional (mode gil::RM_EQV))
    "Post the constraint that b [<->, ->, <-] (v1 rel-type v2)"
    #| (if (not mode)
        (setf mode gil::RM_EQV)) |#
    (var-rel-reify sp (vid v1) rel-type (vid v2) (vid v3) mode))

(defmethod g-rel-reify (sp (v1 int-var) rel-type (v2 fixnum) (v3 bool-var) &optional (mode gil::RM_EQV))
    #| (if (not mode)
        (setf mode gil::RM_EQV)) |#
    (val-rel-reify sp (vid v1) rel-type v2 (vid v3) mode))

; IF-THEN-ELSE
(defmethod g-ite (sp (b-v1 bool-var) t-v2 e-v3 x-v4)
    "Post the constraints if b-v1 then x-v4 = t-v2 else x-v4 = e-v3."
    (ite-rel sp (vid b-v1) (vid t-v2) (vid e-v3) (vid x-v4)))

;DISTINCT
(defmethod g-distinct (sp vars)
    "Post the constraint that the given vars are distinct."
    (distinct sp (vid vars)))

;LINEAR
(defmethod g-linear (sp coeffs vars rel-type (v fixnum)) ; 
    "Post the linear relation coeffs*vars rel-type v."
    (val-linear sp coeffs (vid vars) rel-type v))

(defmethod g-linear (sp coeffs vars rel-type (v int-var))
    (var-linear sp coeffs (vid vars) rel-type (vid v)))

;ARITHMETICS
(defmethod g-abs (sp (v1 int-var) (v2 int-var))
    "Post the constraints that v2 = |v1|."
    (ge-abs sp (vid v1) (vid v2)))

(defmethod g-div (sp (v1 int-var) (v2 int-var) (v3 int-var))
    "Post the constraints that v3 = v1/v2."
    (ge-div sp (vid v1) (vid v2) (vid v3)))

(defmethod g-mod (sp (v1 int-var) (v2 int-var) (v3 int-var))
    "Post the constraints that v3 = v1%v2."
    (var-mod sp (vid v1) (vid v2) (vid v3)))

(defmethod g-divmod (sp (v1 int-var) (v2 int-var) (v3 int-var) (v4 int-var))
    "Post the constraints that v3 = v1/v2 and v4 = v1%v2."
    (ge-divmod sp (vid v1) (vid v2) (vid v3) (vid v4)))

(defmethod g-min (sp (v1 int-var) (v2 int-var) (v3 int-var) &rest vars)
    "Post the constraints that v1 = min(v2, v3, ...)."
    (cond
        ((null vars)
            (ge-min sp (vid v2) (vid v3) (vid v1)))
        (t (ge-arr-min sp (vid v1)
            (append (list (vid v2) (vid v3)) (vid vars))))))

(defmethod g-lmin (sp (v int-var) vars)
    "Post the constraints that v = min(vars)."
    (ge-arr-min sp  (vid v) (vid vars)))

(defmethod g-argmin (sp vars (v int-var))
    "Post the constraints that v = argmin(vars)."
    (ge-argmin sp (vid vars) (vid v)))

(defmethod g-max (sp (v1 int-var) (v2 int-var) (v3 int-var) &rest vars)
    "Post the constraints that v1 = max(v2, v3, ...)."
    (cond ((null vars) (ge-max sp (vid v2) (vid v3) (vid v1)))
          (t (ge-arr-max sp (vid v1) (append (list (vid v2) (vid v3)) (vid vars))))))

(defmethod g-lmax (sp (v int-var) vars)
    "Post the constraints that v = max(vars)."
    (ge-arr-max sp (vid v) (vid vars)))

(defmethod g-argmax (sp vars (v int-var))
    "Post the constraints that v2 = argmax(vars)."
    (ge-argmax sp (vid vars) (vid v)))

(defmethod g-mult (sp (v1 int-var) (v2 int-var) (v3 int-var))
    "Post the constraints that v3 = v1*v2."
    (ge-mult sp (vid v1) (vid v2) (vid v3)))

(defmethod g-sqr (sp (v1 int-var) (v2 int-var))
    "Post the constraints that v2 is the square of v1."
    (ge-sqr sp (vid v1) (vid v2)))

(defmethod g-sqrt (sp (v1 int-var) (v2 int-var))
    "Post the constraints that v2 square root of v1."
    (ge-sqrt sp (vid v1) (vid v2)))

(defmethod g-pow (sp (v1 int-var) n (v2 int-var))
    "Post the constraints that v2 nth power of v1."
    (ge-pow sp (vid v1) n (vid v2)))

(defmethod g-nroot (sp (v1 int-var) n (v2 int-var))
    "Post the constraints that v2 is the nth root of v1."
    (ge-nroot sp (vid v1) n (vid v2)))

(defmethod g-sum (sp (v int-var) vars)
    "Post the constraints that v = sum(vars)."
    (rel-sum sp (vid v) (vid vars)))

(defmethod g-sorted (sp vars1 vars2 vars3)
    "Post the constraint that vars1 = sorted(vars2)"
    (rel-sorted sp (vid vars1) (vid vars2) (vid vars3))
)

;DOM
(defmethod g-dom (sp (v int-var) dom)
    "Post the constraints that dom(v) = dom."
    (set-dom sp (vid v) dom))

(defmethod g-member (sp vars (v int-var))
    "Post the constraints that v is in vars."
    (set-member sp (vid vars) (vid v)))


;COUNT
(defmethod g-count (sp vars (v1 fixnum) rel-type (v2 fixnum))
    "Post the constraints that v2 is the number of times v1 occurs in vars."
    (count-val-val sp (vid vars) v1 rel-type v2))

(defmethod g-count (sp vars (v1 fixnum) rel-type (v2 int-var))
    (count-val-var sp (vid vars) v1 rel-type (vid v2)))

(defmethod g-count (sp vars (v1 int-var) rel-type (v2 fixnum))
    (count-var-val sp (vid vars) (vid v1) rel-type v2))

(defmethod g-count (sp vars (v1 int-var) rel-type (v2 int-var))
    (count-var-var sp (vid vars) (vid v1) rel-type (vid v2)))

(defmethod g-count (sp vars (s-set list) rel-type (val fixnum)); ajouté
    (count-var-set-val sp (vid vars) s-set rel-type val)
)

(defmethod g-count-array (sp vars (c list) rel-type (val fixnum)); ajouté
    (count-array-val sp (vid vars) c rel-type val)
)

(defmethod g-count-setvararray (sp (sva1 list) (sva2 list) (val fixnum)); added for melodizer-rock
    (count-setvararray-val sp (vid sva1) (vid sva2) SRT_EQ val)
)

;SEQUENCE
(defmethod g-sequence (sp vars (s-set list) (v1 fixnum) (v2 fixnum) (v3 fixnum))
    (sequence-var sp (vid vars) s-set v1 v2 v3)
)


;NUMBER OF VALUES
(defmethod g-nvalues (sp vars rel-type (v int-var))
    "Post the constraints that v is the number of distinct values in vars."
    (nvalues sp (vid vars) rel-type (vid v)))

;HAMILTONIAN PATH/CIRCUIT
(defmethod g-circuit (sp costs vars1 vars2 v)
    "Post the constraint that values of vars1 are the edges of an hamiltonian circuit in
    the graph formed by the n variables in vars1, vars2 are the costs of these edges described
    by costs, and v is the total cost of the circuit, i.e. sum(vars2)."
    (hcircuit sp costs (vid vars1) (vid vars2) (vid v)))

;VALUE PRECEDENCE
(defmethod g-precede (sp vars s u)
    "Post the constraint that if there exists j (0 ≤ j < |x|) such that x[j] = u,
    then there must exist i with i < j such that x[i] = s"
    (precede sp (vid vars) s u)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Methods for bool constraints ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;OP
(defmethod g-op (sp (v1 bool-var) bool-op (v2 bool-var) (v3 fixnum))
    "Post the constraints that v1 bool-op v2 = v3."
    (val-bool-op sp (vid v1) bool-op (vid v2) v3))

(defmethod g-op (sp (v1 bool-var) bool-op (v2 bool-var) (v3 bool-var))
    (var-bool-op sp (vid v1) bool-op (vid v2) (vid v3)))

;REL
(defmethod g-rel (sp (v1 bool-var) rel-type (v2 fixnum))
    "Post the constraints that v1 rel-type v2."
    (val-bool-rel sp (vid v1) rel-type v2))

(defmethod g-rel (sp (v1 bool-var) rel-type (v2 bool-var))
    (var-bool-rel sp (vid v1) rel-type (vid v2)))

(defmethod g-rel (sp bool-op (v1 list) (v2 fixnum))
    (val-arr-bool-op sp bool-op (vid v1)  v2))

(defmethod g-rel (sp bool-op (v1 list) (v2 bool-var))
    (var-arr-bool-op sp bool-op (vid v1)  (vid v2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Methods for setVar constraints ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;OP
(defmethod g-op (sp (v1 set-var) set-op (v2 set-var) (v3 set-var))
    (var-set-op sp (vid v1) set-op (vid v2) gil::SRT_EQ (vid v3)))

(defmethod g-set-op (sp (v1 set-var) set-op (v2 set-var) set_rel (v3 set-var))
    (var-set-op sp (vid v1) set-op (vid v2) set_rel (vid v3)))

(defmethod g-arr-op (sp set-op (v1 list) (v2 set-var))
    (arr-set-op sp set-op (vid v1) (vid v2)))

;REL
(defmethod g-rel (sp (v1 set-var) rel-type (v2 set-var))
    "Post the constraints that v1 rel-type v2."
    (var-set-rel sp (vid v1) rel-type (vid v2)))

(defmethod g-rel (sp (v1 set-var) rel-type dom)
    "Post the constraints that v1 rel-type domain dom."
    (val-set-rel sp (vid v1) rel-type dom))

(defmethod g-rel-reify (sp (v1 set-var) rel-type (dom list) r &optional (mode gil::RM_EQV))
    "Post the constraints that v1 rel-type domain dom."
    #| (if (not mode)
        (setf mode gil::RM_EQV)) |#
    (val-set-rel-reify sp (vid v1) rel-type dom (vid r) mode))

(defmethod g-rel-reify (sp (v1 set-var) rel-type (v2 set-var) r &optional (mode gil::RM_EQV))
    "Post the constraints that v1 rel-type domain dom."
    #| (if (not mode)
        (setf mode gil::RM_EQV)) |#
    (var-set-rel-reify sp (vid v1) rel-type (vid v2) (vid r) mode))

;DOM
(defmethod g-dom (sp (v1 set-var) (v2 set-var))
    "Post the constraints that dom(v) = dom."
    (set-setdom sp (vid v1) (vid v2)))

(defmethod g-dom-ints (sp (v1 set-var) rel-type i j)
    "Post the constraints that v1 rel-type domain {i, ..., j}."
    (ints-set-dom sp (vid v1) rel-type i j))

(defmethod g-empty (sp (v1 set-var))
    "Post the constraints that v1 is empty."
    (empty-set sp (vid v1)))

;CARDINALITY
(defmethod g-card (sp (v1 set-var) min-card max-card)
    (val-card sp (list (vid v1)) min-card max-card))

(defmethod g-card (sp (v list) min-card max-card)
    (val-card sp (vid v) min-card max-card))

(defmethod g-card-var (sp (v1 set-var) (v2 int-var))
    (var-card sp (vid v1) (vid v2)))

;CHANNEL
(defmethod g-channel (sp (v1 list) (v2 list))
    (channel-set sp (vid v1) (vid v2)))

(defmethod g-channel (sp (v1 list) (v2 set-var))
    (channel-set-bool sp (vid v1) (vid v2)))

;MINIMUM
(defmethod g-setmin (sp (v1 set-var))
    (make-instance 'int-var :id (set-min sp (vid v1))))

(defmethod g-setmin-reify (sp (v1 set-var) (v2 int-var) (r bool-var) &optional (mode gil::RM_EQV))
    #| (if (not mode)
        (setf mode gil::RM_EQV)) |#
    (set-min-reify sp (vid v1) (vid v2) (vid r) mode))

;MAXIMUM
(defmethod g-setmax (sp (v1 set-var))
    (make-instance 'int-var :id (set-max sp (vid v1))))

(defmethod g-setmax-reify (sp (v1 set-var) (v2 int-var) (r bool-var) &optional (mode gil::RM_EQV))
    #| (if (not mode)
        (setf mode gil::RM_EQV)) |#
    (set-max-reify sp (vid v1) (vid v2) (vid r) mode))

;SETUNION
(defmethod g-setunion (sp (v1 set-var) (v2 list))
    (set-union sp (vid v1) (vid v2)))

;ELEMENT
(defmethod g-element (sp set-op (v1 list) (v2 set-var) (v3 set-var))
    (element sp set-op (vid v1) (vid v2) (vid v3))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Modeling convenience methods ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; These methods stands for the MiniModel functions of Gecode

;; Boolean expressions
; Returns BoolVar for logic expressions

(defmethod add-bool-var-expr (sp (v1 int-var) rel-type (v2 fixnum))
    "Adds a boolean variable representing the expression
    v1 rel-type v2 to sp"
    (make-instance 'bool-var
        :id (add-bool-var-expr-val sp (vid v1) rel-type v2)))

(defmethod add-bool-var-expr (sp (v1 int-var) rel-type (v2 int-var))
    (make-instance 'bool-var
        :id (add-bool-var-expr-var sp (vid v1) rel-type (vid v2))))

;; Integer expressions
; Returns IntVar for arithmetic expressions

(defmethod add-int-var-expr (sp (v1 int-var) op (v2 int-var))
    "Adds an integer variable representing the expression
    v1 op v2 to sp"
    (make-instance 'int-var
        :id (add-int-var-expr-var sp (vid v1) op (vid v2))))

(defmethod add-int-var-expr (sp (v1 int-var) op (v2 fixnum))
    (make-instance 'int-var
        :id (add-int-var-expr-val sp (vid v1) op v2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Methods for exploration ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;INTVARS

;Variable branching strategies
(defparameter gil::INT_VAR_SIZE_MIN 0)    ; select first the variable with the smallest domain
(defparameter gil::INT_VAR_RND 1) ; select first a random variable
(defparameter gil::INT_VAR_DEGREE_MAX 2); select the variable with the highest degree
(defparameter gil::INT_VAR_NONE 3) ;select first unassigned

;Value branching strategies
(defparameter gil::INT_VAL_MIN 0)    ; select first the smallest value of the domain
(defparameter gil::INT_VAL_RND 1) ; select first a random value
(defparameter gil::INT_VAL_SPLIT_MIN 2) ; select the values not greater than the (min+max)/2
(defparameter gil::INT_VAL_SPLIT_MAX 3) ; select the values greater than (min+max)/2
(defparameter gil::INT_VAL_MED 4) ; selects the greatest value not bigger than the median

;;;SETVARS

;Variable branching strategies
(defparameter gil::SET_VAR_SIZE_MIN 0)    ; select first the variable with the smallest unknown domain
(defparameter gil::SET_VAR_RND 1) ; select first a random variable
(defparameter gil::SET_VAR_DEGREE_MAX 2); select the variable with the highest degree
(defparameter gil::SET_VAR_NONE 3) ;select first unassigned

;Value branching strategies
(defparameter gil::SET_VAL_MIN_INC 0)    ; select first the smallest value of the domain
(defparameter gil::SET_VAL_RND_INC 1) ; select first a random value
(defparameter gil::SET_VAL_MIN_EXC 2) ; select the values not greater than the (min+max)/2
(defparameter gil::SET_VAL_RND_EXC 3) ; select the values greater than (min+max)/2
(defparameter gil::SET_VAL_MED_INC 4) ; selects the greatest value not bigger than the median

(defmethod g-branch (sp (v int-var) var-strat val-strat)
    "Post a branching on v with strategies var-strat and val-strat."
    (branch sp (list (vid v)) var-strat val-strat))

(defmethod g-branch (sp (v bool-var) var-strat val-strat)
    (branch-b sp (list (vid v)) var-strat val-strat))

(defmethod g-branch (sp (v set-var) var-strat val-strat)
    (branch-set sp (list (vid v)) var-strat val-strat))

(defmethod g-branch (sp (v list) var-strat val-strat)
    (if (typep (car v) 'int-var)
        (branch sp (vid v) var-strat val-strat)
        (if (typep (car v) 'bool-var)
            (branch-b sp (vid v) var-strat val-strat)
            (branch-set sp (vid v) var-strat val-strat))))

;cost
(defmethod g-cost (sp (v-list list))
    "Defines that v is the cost of sp."
    (let ((vids '()))
        (dolist (v v-list)
            (push (vid v) vids)
        )
        (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
            (set-cost sp x (length vids))
        )
    )   
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Methods for search engines ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  

; Methods for search engine options

(defclass time-stop ()
    ((ts :initform nil :initarg ts :accessor ts)); ts is a void pointer to a WTimeStop object in Gecode
)

(defmethod t-stop ()
    (make-instance 'time-stop)
)

(defmethod time-stop-init (tstop max-time)
    (setf (ts tstop) (new-time-stop max-time))
)

(defmethod time-stop-reset (tstop)
    (reset-time-stop (ts tstop))
)

(defclass search-options ()
    ((opts :initform nil :initarg opts :accessor opts)); opts is a void pointer to a WSearchOptions object in Gecode
)

(defmethod search-opts ()
    (make-instance 'search-options)
)

(defmethod init-search-opts (sopts)
    (setf (opts sopts) (new-search-options))
)

(defmethod set-n-threads (s-opts nthreads)
    (set-nb-threads (opts s-opts) nthreads)
)

(defmethod set-time-stop (s-opts t-stop)
    (set-t-stop (opts s-opts) (ts t-stop))
)

;Search-engine types
(defparameter gil::DFS "dfs")
(defparameter gil::BAB "bab")

(defclass BAB-engine ()
    ((bab :initform nil :initarg :bab :accessor bab))
)

(defclass DFS-engine ()
    ((dfs :initform nil :initarg :dfs :accessor dfs))
)

(defmethod search-engine (sp opts se-type)
    "Creates a new search engine (dfs or bab)."
    (cond
        ((string-equal se-type gil::DFS) (make-instance 'DFS-engine :dfs (dfs-engine-low sp opts)))
        ((string-equal se-type gil::BAB) (make-instance 'BAB-engine :bab (bab-engine-low sp opts)))
    )
)

;solution exist?
(defun sol? (sol)
    "Existence predicate for a solution"
    (and (not (cffi::null-pointer-p sol)) sol))

;next solution
(defmethod search-next ((se BAB-engine))
    "Search the next solution of se."
    (print "Entering the BAB-search-next")
    (sol? (bab-next (bab se))))

(defmethod search-next ((se DFS-engine))
    (print "Entering the DFS-search-next")
    (sol? (dfs-next (dfs se))))

(defmethod search-next ((se null))
    nil)

;stopped
; returns 1 if stopped, 0 if not
(defmethod stopped ((se BAB-engine))
    (bab-stopped (bab se))
)

(defmethod stopped ((se DFS-engine))
    (dfs-stopped (dfs se))
)

;;;;;;;;;;;;;;;;;;;;;;;;;
; Methods for solutions ;
;;;;;;;;;;;;;;;;;;;;;;;;;

;values
(defmethod g-values (sp (v int-var))
    "Get the values assigned to v."
    (get-value sp (vid v)))

(defmethod g-values (sp (v bool-var))
    "Get the values assigned to v."
    (get-value-bool sp (vid v)))

(defmethod g-values (sp (v set-var))
    "Get the values assigned to v."
    (get-value-set sp (vid v) (g-value-size sp v)))

(defmethod g-value-size (sp (v set-var))
    "Get the size of a SetVar"
    (get-value-size sp (vid v)))

; Returns computed values of the list v in the space sp
; Boolean values don't work properly but the error should be in the C++ code
(defmethod g-values (sp (v list))
    (if (typep (first v) 'bool-var)
        (get-values-bool sp (vid v))
        (get-values sp (vid v))
    )
)

(defmethod g-values ((sp null) v)
    nil)

;print
(defmethod g-print (sp (v int-var))
    "Print v."
    (print-vars sp (list (vid v))))

(defmethod g-print (sp (v list))
    (print-vars sp (vid v)))

(defmethod g-print ((sp null) v)
    nil)
