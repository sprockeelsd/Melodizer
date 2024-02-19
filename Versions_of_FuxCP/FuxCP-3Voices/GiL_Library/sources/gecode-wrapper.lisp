(cl:defpackage "gil"
  (:nicknames "GIL")
   (:use common-lisp :cl-user :cl :cffi))

(in-package :gil)

(print "Loading gecode-wrapper...")

(cffi::defcfun ("computation_space" new-space) :pointer
    "Create a new computation space."
)

(cffi::defcfun ("bab_computation_space" new-bab-space) :pointer
    "Create a new BAB space"
)

(cffi::defcfun ("add_intVar" add-int-var-low) :int
    "Add an IntVar ranging from min to max to the specified space. Return the reference of this variable for this space."
    (sp :pointer)
    (min :int)
    (max :int)
)

(cffi::defcfun ("add_intVarWithDom" add-int-var-dom-aux) :int
    "Add an IntVar with domain dom of size s to the specified space. Return the reference of this variable for this space."
    (sp :pointer)
    (s :int)
    (dom :pointer)
)

(defun add-int-var-dom-low (sp dom)
    "Add an IntVar with domain dom to the specified space. Return the reference of this variable for this space."
    (let ((x (cffi::foreign-alloc :int :initial-contents dom)))
        (add-int-var-dom-aux sp (length dom) x))
)

(cffi::defcfun ("add_intVarArray" add-int-var-array-aux) :pointer
    "Add n IntVar ranging from min to max to the specified space."
    (sp :pointer)
    (n :int)
    (min :int)
    (max :int)
)

(defun add-int-var-array-low (sp n min max)
    "Add n IntVar ranging from min to max to the specified space. Return the references of those variables for this space"
    (let ((p (add-int-var-array-aux sp n min max)))
        (loop for i from 0 below n
            collect (cffi::mem-aref p :int i)))
)

(cffi::defcfun ("add_boolVarArray" add-bool-var-array-aux) :pointer
    "Add n boolVar ranging from min to max to the specified space."
    (sp :pointer)
    (n :int)
    (min :int)
    (max :int)
)

(defun add-bool-var-array-low (sp n min max)
    "Add n BoolVar ranging from min to max to the specified space. Return the references of those variables for this space"
    (let ((p (add-bool-var-array-aux sp n min max)))
        (loop for i from 0 below n
            collect (cffi::mem-aref p :int i)))
)

(cffi::defcfun ("add_intVarArrayWithDom" add-int-var-array-dom-aux) :pointer
    "Add n IntVar with domain dom of size s to the specified space."
    (sp :pointer)
    (n :int)
    (s :int)
    (dom :pointer)
)

(defun add-int-var-array-dom-low (sp n dom)
    "Add n IntVar with domain dom to the specified space. Return the references of those variables for this space"
    (let ((x (cffi::foreign-alloc :int :initial-contents dom))
           p)
        (setq p (add-int-var-array-dom-aux sp n (length dom) x))
        (loop for i from 0 below n
            collect (cffi::mem-aref p :int i)))
)

(cffi::defcfun ("add_intVar_expr_val" add-int-var-expr-val) :int
    "Add a IntVar representing the expression vid op val."
    (sp :pointer)
    (vid :int)
    (op :int)
    (val :int)
)

(cffi::defcfun ("add_intVar_expr_var" add-int-var-expr-var) :int
    "Add a IntVar representing the expression vid1 op vid2."
    (sp :pointer)
    (vid1 :int)
    (op :int)
    (vid2 :int)
)

(cffi::defcfun ("set_solution_vars" set-solution-vars-aux) :void
    (sp :pointer)
    (n :int)
    (vids :pointer)
)

(defun set-solution-vars (sp vids)
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (set-solution-vars-aux sp (length vids) x)
    )
)

(cffi::defcfun ("set_percent_diff" set-percent-diff) :void
    (sp :pointer)
    (diff :int)
)

(cffi::defcfun ("nvars" nvars) :int
    "Return the number of variables in the space."
    (sp :pointer)
)

;IntVar relation flags
(defparameter gil::IRT_EQ 0)    ; equality relation
(defparameter gil::IRT_NQ 1)    ; inequality
(defparameter gil::IRT_LQ 2)    ; Less or equal
(defparameter gil::IRT_LE 3)    ; Strictly lower
(defparameter gil::IRT_GQ 4)    ; Greater or equal
(defparameter gil::IRT_GR 5)    ; Strictly greater

;IntVar operation flags
(defparameter gil::IOP_ADD 0)   ; Addition
(defparameter gil::IOP_SUB 1)   ; Substraction
(defparameter gil::IOP_MUL 2)   ; Multiplication
(defparameter gil::IOP_DIV 3)   ; Division
(defparameter gil::IOP_MOD 4)   ; Modulo


(cffi::defcfun ("add_boolVar" add-bool-var-range) :int
    "Add a BoolVar ranging from l to h. Return the index to this BoolVar."
    (sp :pointer)
    (l :int)
    (h :int)
)

(cffi::defcfun ("add_boolVar_expr_val" add-bool-var-expr-val) :int
    "Add a BoolVar corresponding to the evalueation of rel-type(vid, val)."
    (sp :pointer)
    (vid :int)
    (rel-type :int)
    (val :int)
)

(cffi::defcfun ("add_boolVar_expr_var" add-bool-var-expr-var) :int
    "Add a BoolVar corresponding to the evalueation of rel-type(vid1, vid2)."
    (sp :pointer)
    (vid1 :int)
    (rel-type :int)
    (vid2 :int)
)

(cffi::defcfun ("add_setVar" add-set-var-card) :int
    "Add a SetVar ranging from l to h. Return the index to this BoolVar."
    (sp :pointer)
    (lub-min :int)
    (lub-max :int)
    (card-min :int)
    (card-max :int)
)

(cffi::defcfun ("add_setVarArray" add-set-var-array-aux) :pointer
    "Add n setVar with cardinality card-min to card-max to the specified space."
    (sp :pointer)
    (n :int)
    (lub-min :int)
    (lub-max :int)
    (card-min :int)
    (card-max :int)
)

(defun add-set-var-array-card (sp n lub-min lub-max card-min card-max)
    "Add n SetVar ranging cardinality from card-min to card-max to the specified space. Return the references of those variables for this space"
    (let ((p (add-set-var-array-aux sp n lub-min lub-max card-min card-max)))
        (loop for i from 0 below n
            collect (cffi::mem-aref p :int i)))
)

(cffi::defcfun ("val_rel" val-rel) :void
    "Post a variable/value rel constraint."
    (sp :pointer)
    (vid :int)
    (rel-type :int)
    (val :int)
)

(cffi::defcfun ("var_rel" var-rel) :void
    "Post a variable/variable rel constraint."
    (sp :pointer)
    (vid1 :int)
    (rel-type :int)
    (vid2 :int)
)

(cffi::defcfun ("var_rel_reify" var-rel-reify) :void
    "Post a variable/variable rel constraint with reification."
    (sp :pointer)
    (vid1 :int)
    (rel-type :int)
    (vid2 :int)
    (vid3 :int)
    (mode :int)
)

(cffi::defcfun ("val_rel_reify" val-rel-reify) :void
    "Post a variable/value rel constraint with reification."
    (sp :pointer)
    (vid1 :int)
    (rel-type :int)
    (val :int)
    (vid2 :int)
    (mode :int)
)

(cffi::defcfun ("arr_val_rel" arr-val-rel-aux) :void
    "Post a variable-array/value rel constraint."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (rel-type :int)
    (val :int)
)

(defun arr-val-rel (sp vids rel-type val)
    "Post a variable-array/value rel constraint."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (arr-val-rel-aux sp (length vids) x rel-type val))
)

(cffi::defcfun ("arr_var_rel" arr-var-rel-aux) :void
    "Post a variable-array/variable rel constraint."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (rel-type :int)
    (vid :int)
)

(defun arr-var-rel (sp vids rel-type vid)
    "Post a variable-array/variable rel constraint."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (arr-var-rel-aux sp (length vids) x rel-type vid))
)

(cffi::defcfun ("arr_rel" arr-rel-aux) :void
    "Post a variable-array rel constraint."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (rel-type :int)
)

(defun arr-rel (sp vids rel-type)
    "Post a variable-array rel constraint."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (arr-rel-aux sp (length vids) x rel-type))
)

(cffi::defcfun ("arr_arr_rel" arr-arr-rel-aux) :void
    "Post a variable-array/variable-array rel constraint."
    (sp :pointer)
    (n1 :int)
    (vids1 :pointer)
    (rel-type :int)
    (n2 :int)
    (vids2 :pointer)
)

(defun arr-arr-rel (sp vids1 rel-type vids2)
    "Post a variable-array/variable-array rel constraint."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids1))
          (y (cffi::foreign-alloc :int :initial-contents vids2)))
        (arr-arr-rel-aux sp (length vids1) x rel-type (length vids2) y))
)

(cffi::defcfun ("ite_rel" ite-rel) :void
    "Post a if-then-else constraint."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
    (vid4 :int)
)

(cffi::defcfun ("distinct" distinct-aux) :void
    "Post a distinct constraint on the n variables denoted in vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
)

(defun distinct (sp vids)
    "Post a distinct constraint on the variables denoted in vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (distinct-aux sp (length vids) x))
)

(cffi::defcfun ("val_linear" val-linear-aux) :void
    "Post a linear equation constraint."
    (sp :pointer)
    (n :int)
    (c :pointer)
    (vids :pointer)
    (rel-type :int)
    (val :int)
)

(defun val-linear (sp coeffs vars rel-type value)
    "Post a linear equation constraint. coeffs and vars must have the same number of elements."
    (let ((c (cffi::foreign-alloc :int :initial-contents coeffs))
          (x (cffi::foreign-alloc :int :initial-contents vars)))
        (val-linear-aux sp (length coeffs) c x rel-type value))
)

(cffi::defcfun ("var_linear" var-linear-aux) :void
    "Post a linear equation constraint."
    (sp :pointer)
    (n :int)
    (c :pointer)
    (vids :pointer)
    (rel-type :int)
    (vid :int)
)

(defun var-linear (sp coeffs vars rel-type vid)
    "Post a linear equation constraint. coeffs and vars must have the same number of elements."
    (let ((c (cffi::foreign-alloc :int :initial-contents coeffs))
          (x (cffi::foreign-alloc :int :initial-contents vars)))
        (var-linear-aux sp (length coeffs) c x rel-type vid))
)

(cffi::defcfun ("arithmetics_abs" ge-abs) :void
    "Post the constraint that |vid1| = vid2."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
)

(cffi::defcfun ("arithmetics_div" ge-div) :void
    "Post the constraint that vid3 = vid1/vid2."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
)

(cffi::defcfun ("arithmetics_mod" var-mod) :void
    "Post the constraint that vid1 % vid2 = vid3."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
)

(cffi::defcfun ("arithmetics_divmod" ge-divmod) :void
    "Post the constraint that vid3 = vid1/vid2 and vid4 = vid1 % vid2."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
    (vid4 :int)
)

(cffi:defcfun ("arithmetics_min" ge-min) :void
    "Post the constraint that vid3 = min(vid1, vid2)."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
)

(cffi:defcfun ("arithmetics_arr_min" ge-arr-min-aux) :void
    "Post the constraint that vid = min(vids)."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid :int)
)

(defun ge-arr-min (sp vid vids)
    "Post the constraint that vid = min(vids)."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (ge-arr-min-aux sp (length vids) x vid))
)

(cffi:defcfun ("arithmetics_argmin" ge-argmin-aux) :void
    "Post the constraint that vid = argmin(vids)."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid :int)
)

(defun ge-argmin (sp vids vid)
    "Post the constraint that vid = argmin(vids)."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (ge-argmin-aux sp (length vids) x vid))
)

(cffi:defcfun ("arithmetics_max" ge-max) :void
    "Post the constraint that vid3 = max(vid1, vid2)."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
)

(cffi:defcfun ("arithmetics_arr_max" ge-arr-max-aux) :void
    "Post the constraint that vid = max(vids)."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid :int)
)

(defun ge-arr-max (sp vid vids)
    "Post the constraint that vid = max(vids)."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (ge-arr-max-aux sp (length vids) x vid))
)

(cffi:defcfun ("arithmetics_argmax" ge-argmax-aux) :void
    "Post the constraint that vid = argmax(vids)."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid :int)
)

(defun ge-argmax (sp vids vid)
    "Post the constraint that vid = argmax(vids)."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (ge-argmax-aux sp (length vids) x vid))
)

(cffi:defcfun ("arithmetics_mult" ge-mult) :void
    "Post the constraint that vid3 = vid1 * vid2."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (vid3 :int)
)

(cffi:defcfun ("arithmetics_sqr" ge-sqr) :void
    "Post the constraint that vid2 = vid1^2."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
)

(cffi:defcfun ("arithmetics_sqrt" ge-sqrt) :void
    "Post the constraint that vid2 = vid1^(1/2)."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
)

(cffi:defcfun ("arithmetics_pow" ge-pow) :void
    "Post the constraint that vid2 = vid1^n."
    (sp :pointer)
    (vid1 :int)
    (n :int)
    (vid2 :int)
)

(cffi:defcfun ("arithmetics_nroot" ge-nroot) :void
    "Post the constraint that vid2 = vid1^(1/n)."
    (sp :pointer)
    (vid1 :int)
    (n :int)
    (vid2 :int)
)

(cffi::defcfun ("set_dom" set-dom-aux) :void
    "Post the constraint that dom(vid) = domain of size n."
    (sp :pointer)
    (vid :int)
    (n :int)
    (domain :pointer)
)

(defun set-dom (sp vid domain)
    "Post the constraint that dom(vid) = domain."
    (let ((x (cffi::foreign-alloc :int :initial-contents domain)))
        (set-dom-aux sp vid (length domain) x))
)

(cffi::defcfun ("set_member" set-member-aux) :void
    "Post the constraint that vid is a member vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid :int)
)

(defun set-member (sp vids vid)
    "Post the constraint that vid is a member vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (set-member-aux sp (length vids) x vid))
)

(cffi::defcfun ("rel_sum" rel-sum-aux) :void
    "Post the constraint that vid = sum(vids). n is the number of vars in vids."
    (sp :pointer)
    (vid :int)
    (n :int)
    (vids :pointer)
)

(defun rel-sum (sp vid vids)
    "Post the constraint that vid = sum(vids)."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (rel-sum-aux sp vid (length vids) x))
)

(cffi::defcfun ("rel_sorted" rel-sorted-aux) :void
    "Post the constraint that vid = sum(vids). n is the number of vars in vids."
    (sp :pointer)
    (n1 :int)
    (vids1 :pointer)
    (n2 :int)
    (vids2 :pointer)
    (n3 :int)
    (vids3 :pointer)
)

(defun rel-sorted (sp vids1 vids2 vids3)
    "Post the constraint that vids1 = sorted(vids2)."
    (let (
        (x (cffi::foreign-alloc :int :initial-contents vids1))
        (y (cffi::foreign-alloc :int :initial-contents vids2))
        (z (cffi::foreign-alloc :int :initial-contents vids3))
        )
        (rel-sorted-aux sp (length vids1) x (length vids2) y (length vids3) z)
    )
)

(cffi::defcfun ("count_val_val" count-val-val-aux) :void
    "Post the constraint that the number of variables in vids equal to val1 has relation
    rel-type with val2."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (val1 :int)
    (rel-type :int)
    (val2 :int)
)

(defun count-val-val (sp vids val1 rel-type val2)
    "Post the constraint that the number of variables in vids equal to val1 has relation
    rel-type with val2."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (count-val-val-aux sp (length vids) x val1 rel-type val2))
)

(cffi::defcfun ("count_val_var" count-val-var-aux) :void
    "Post the constraint that the number of variables in vids equal to val1 has relation
    rel-type with vid."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (val :int)
    (rel-type :int)
    (vid :int)
)

(defun count-val-var (sp vids val rel-type vid)
    "Post the constraint that the number of variables in vids equal to val has relation
    rel-type with vid."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (count-val-var-aux sp (length vids) x val rel-type vid))
)

(cffi::defcfun ("count_var_val" count-var-val-aux) :void
    "Post the constraint that the number of variables in vids equal to vid has relation
    rel-type with val."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid :int)
    (rel-type :int)
    (val :int)
)

(defun count-var-val (sp vids vid rel-type val)
    "Post the constraint that the number of variables in vids equal to vid has relation
    rel-type with val."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (count-var-val-aux sp (length vids) x vid rel-type val))
)

(cffi::defcfun ("count_var_var" count-var-var-aux) :void
    "Post the constraint that the number of variables in vids equal to vid1 has relation
    rel-type with vid2."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (vid1 :int)
    (rel-type :int)
    (vid2 :int)
)

(defun count-var-var (sp vids vid1 rel-type vid2)
    "Post the constraint that the number of variables in vids equal to vid1 has relation
    rel-type with vid2."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (count-var-var-aux sp (length vids) x vid1 rel-type vid2))
)

(cffi::defcfun ("count_var_set_val" count-var-set-val-aux) :void
    "Post the constraint that the number of variables in vids belonging to the set set has relation rel-type with val."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (s :int)
    (s-set :pointer)
    (rel-type :int)
    (val :int)
)

(defun count-var-set-val (sp vids s-set rel-type val)
"Post the constraint that the number of variables in vids belonging to the set set has relation rel-type with val."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids))
            (y (cffi::foreign-alloc :int :initial-contents s-set)))
        (count-var-set-val-aux sp (length vids) x (length s-set) y rel-type val))
)

(cffi::defcfun ("count_array_val" count-array-val-aux) :void
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (c :pointer)
    (rel-type :int)
    (val :int)
)

(defun count-array-val (sp vids c rel-type val)
"Post the constraint that the number of times that vars[i] = c[i] is equal to val"
        (let ((x (cffi::foreign-alloc :int :initial-contents vids))
                (y (cffi::foreign-alloc :int :initial-contents c)))
            (count-array-val-aux sp (length vids) x y rel-type val))
)

(cffi::defcfun ("count_setvararray_val" count-setvararray-val-aux) :void
    (sp :pointer)
    (n :int)
    (vids1 :pointer)
    (vids2 :pointer)
    (rel-type :int)
    (val :int)
)

(defun count-setvararray-val (sp vids1 vids2 rel-type val)
    "Post the constraint that the number of times that vids1[i] rel-type vids2[i] >= val"
    (let ((x (cffi::foreign-alloc :int :initial-contents vids1))
            (y (cffi::foreign-alloc :int :initial-contents vids2)))
        (count-setvararray-val-aux sp (min (length vids1) (length vids2)) x y rel-type val))
)

(cffi::defcfun ("sequence_var" sequence-var-aux) :void
    "Post the constraint that the number of occurences of s-set in every subsequence of length
    val1 in vids must be higher than val2 and lower than val3"
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (s :int)
    (s-set :pointer)
    (val1 :int)
    (val2 :int)
    (val3 :int)
)

(defun sequence-var (sp vids s-set val1 val2 val3)
    (let ((x (cffi::foreign-alloc :int :initial-contents vids))
            (y (cffi::foreign-alloc :int :initial-contents s-set)))
        (sequence-var-aux sp (length vids) x (length s-set) y val1 val2 val3))
)

(cffi::defcfun ("nvalues" nvalues-aux) :void
    "Post the constraint the number of distinct values in the n variables denoted by vids
     has the given rel-type relation with the variable vid."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (rel-type :int)
    (vid :int)
)

(defun nvalues (sp vids rel-type vid)
    "Post the constraint the number of distinct values in the n variables denoted by vids
    has the given rel-type relation with the variable vid."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (nvalues-aux sp (length vids) x rel-type vid))
)

(cffi::defcfun ("circuit" hcircuit-aux) :void
    "Post the constraint that values of vids1 are the edges of an hamiltonian circuit in
    the graph formed by the n variables in vids1, vids2 are the costs of these edges described
    by c, and vid is the total cost of the circuit, i.e. sum(vids2)."
    (sp :pointer)
    (n :int)
    (c :pointer)
    (vids1 :pointer)
    (vids2 :pointer)
    (vid :int)
)

(defun hcircuit (sp c vids1 vids2 vid)
    "Post the constraint that values of vids1 are the edges of an hamiltonian circuit in
    the graph formed by the variables in vids1, vids2 are the costs of these edges described
    by c, and vid is the total cost of the circuit, i.e. sum(vids2)."
    (let ((costs (cffi::foreign-alloc :int :initial-contents c))
          (x (cffi::foreign-alloc :int :initial-contents vids1))
          (y (cffi::foreign-alloc :int :initial-contents vids2)))
        (hcircuit-aux sp (length vids1) costs x y vid))
)

(cffi::defcfun ("precede" precede-aux) :void
    "Post the constraint that if there exists j (0 ≤ j < |x|) such that x[j] = u,
    then there must exist i with i < j such that x[i] = s"
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (s :int)
    (u :int)
)

(defun precede (sp vids s u)
    "Post the constraint that if there exists j (0 ≤ j < |x|) such that x[j] = u,
    then there must exist i with i < j such that x[i] = s"
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (precede-aux sp (length vids) x s u)
    )
)

;Reification mode
(defparameter gil::RM_EQV 0)    ; Equivalent
(defparameter gil::RM_IMP 1)    ; Implication
(defparameter gil::RM_PMI 2)    ; Inverse implication

;BoolVar operation flags
(defparameter gil::BOT_AND 0)    ; logical and
(defparameter gil::BOT_OR 1)     ; logical or
(defparameter gil::BOT_IMP 2)    ; logical implication
(defparameter gil::BOT_EQV 3)    ; logical equivalence
(defparameter gil::BOT_XOR 4)    ; logical exclusive or

(cffi::defcfun ("val_boolop" val-bool-op) :void
    "Post the constraint that val = bool-op(vid1, vid2)."
    (sp :pointer)
    (vid1 :int)
    (bool-op :int)
    (vid2 :int)
    (val :int)
)

(cffi::defcfun ("val_arr_boolop" val-arr-bool-op-aux) :void
    "Post the constraint that elements of vids bool_op val"
    (sp :pointer)
    (bool_op :int)
    (s :int)
    (vids :pointer)
    (val :int)
)

(defun val-arr-bool-op (sp bool-op vids val)
    "Post the constraint that elements of vids bool-op val"
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (val-arr-bool-op-aux sp bool-op (length vids) x val)
    )
)

(cffi::defcfun ("var_arr_boolop" var-arr-bool-op-aux) :void
    "Post the constraint that vid equals bool_op between all element of vids"
    (sp :pointer)
    (bool_op :int)
    (s :int)
    (vids :pointer)
    (vid :int)
)

(defun var-arr-bool-op (sp bool-op vids vid)
    "Post the constraint that vid equals bool_op between all element of vids"
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (var-arr-bool-op-aux sp bool-op (length vids) x vid)
    )
)

(cffi::defcfun ("var_boolop" var-bool-op) :void
    "Post the constraint that vid3 = bool-op(vid1, vid2)."
    (sp :pointer)
    (vid1 :int)
    (bool-op :int)
    (vid2 :int)
    (vid3 :int)
)

(cffi::defcfun ("val_boolrel" val-bool-rel) :void
    "Post boolean rel constraint."
    (sp :pointer)
    (vid :int)
    (rel-type :int)
    (val :int)
)

(cffi::defcfun ("var_boolrel" var-bool-rel) :void
    "Post boolean rel constraint."
    (sp :pointer)
    (vid1 :int)
    (rel-type :int)
    (vid2 :int)
)

;SetVar relation flags
(defparameter gil::SRT_EQ 0)    ; equality relation
(defparameter gil::SRT_NQ 1)    ; inequality
(defparameter gil::SRT_SUB 2)   ; Subset
(defparameter gil::SRT_SUP 3)   ; Superset
(defparameter gil::SRT_DISJ 4)  ; Disjoint
(defparameter gil::SRT_CMPL 5)  ; Complement
(defparameter gil::SRT_LQ 6)    ; Less or equal
(defparameter gil::SRT_LE 7)    ; Strictly lower
(defparameter gil::SRT_GQ 8)    ; Greater or equal
(defparameter gil::SRT_GR 9)    ; Strictly greater

;SetVar operation flags
(defparameter gil::SOT_UNION 0)   ; union
(defparameter gil::SOT_DUNION 1)  ; disjoint union
(defparameter gil::SOT_INTER 2)   ; intersection
(defparameter gil::SOT_MINUS 3)   ; difference

(cffi::defcfun ("var_setop" var-set-op) :void
    "Post the constraint that vid3 set_rel( set-op(vid1, vid2))."
    (sp :pointer)
    (vid1 :int)
    (set-op :int)
    (vid2 :int)
    (set-rel :int)
    (vid3 :int)
)

(cffi::defcfun ("arr_setop" arr-set-op-aux) :void
    "Post the constraint that vid2 set_op vid1."
    (sp :pointer)
    (set_op :int)
    (s :int)
    (vid1 :pointer)
    (vid2 :int)
)

(defun arr-set-op (sp set_op vid1 vid2)
 "Post the constraint that vid2 set_op vid1."
    (let ((x (cffi::foreign-alloc :int :initial-contents vid1)))
        (arr-set-op-aux sp set_op (length vid1) x vid2))
)

(cffi::defcfun ("var_setrel" var-set-rel) :void
    "Post setVar rel constraint."
    (sp :pointer)
    (vid1 :int)
    (rel-type :int)
    (vid2 :int)
)

(cffi::defcfun ("empty_set" empty-set) :void
    "post that vid1 has to be empty"
    (sp :pointer)
    (vid1 :int)
)

(cffi::defcfun ("val_setrel" val-set-rel-aux) :void
    "Post setVar rel constraint."
    (sp :pointer)
    (vid :int)
    (rel-type :int)
    (dom :pointer)
    (s :int)
)

(defun val-set-rel (sp vid1 rel-type dom)
    "Post the constraint that vid = min(vids)."
    (let ((x (cffi::foreign-alloc :int :initial-contents dom)))
        (val-set-rel-aux sp vid1 rel-type x (length dom)))
)

(cffi::defcfun ("val_setrel_reify" val-set-rel-reify-aux) :void
    "Post setVar rel constraint with reify."
    (sp :pointer)
    (vid :int)
    (rel-type :int)
    (dom :pointer)
    (s :int)
    (r :int)
    (mode :int)
)

(defun val-set-rel-reify (sp vid1 rel-type dom r mode)
    "Post the constraint that vid = min(vids)."
    (let ((x (cffi::foreign-alloc :int :initial-contents dom)))
        (val-set-rel-reify-aux sp vid1 rel-type x (length dom) r mode))
)

(cffi::defcfun ("var_setrel_reify" var-set-rel-reify) :void
    "Post setVar rel constraint with reify."
    (sp :pointer)
    (vid1 :int)
    (rel-type :int)
    (vid2 :int)
    (r :int)
    (mode :int)
)

(cffi::defcfun ("ints_setdom" ints-set-dom) :void
    "Post setVar dom constraint."
    (sp :pointer)
    (vid1 :int)
    (rel-type :int)
    (i :int)
    (j :int)
)

(cffi::defcfun ("set_setdom" set-setdom) :void
    "Post setVar dom constraint."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
)

(cffi::defcfun ("val_card" val-card-aux) :void
    "Post setVar cardinality constraint."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (min-card :int)
    (max-card :int)
)

(defun val-card (sp vids min-card max-card)
    "Post cardinality constraint on the SetVars denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (val-card-aux sp (length vids) x min-card max-card))
)

(cffi::defcfun ("var_card" var-card) :void
    "Post setVar cardinality constraint."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
)

(cffi::defcfun ("var_setrel" var-set-rel) :void
    "Post setVar rel constraint."
    (sp :pointer)
    (vid1 :int)
    (rel-type :int)
    (vid2 :int)
)

(cffi::defcfun ("channel_set" channel-set-aux) :void
    "Post setVar channel constraint."
    (sp :pointer)
    (n1 :int)
    (vids1 :pointer)
    (n2 :int)
    (vids2 :pointer)
)

(defun channel-set (sp vids1 vids2)
    "Post channel constraint on the SetVars denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids1))
          (y (cffi::foreign-alloc :int :initial-contents vids2)))
        (channel-set-aux sp (length vids1) x (length vids2) y))
)

(cffi::defcfun ("channel_set_bool" channel-set-bool-aux) :void
    "Post setVar channel constraint."
    (sp :pointer)
    (n1 :int)
    (vids1 :pointer)
    (vid2 :int)
)

(defun channel-set-bool (sp vids1 vid2)
    "Post channel constraint on the SetVar vid2 and boolVarArray vids1."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids1)))
        (channel-set-bool-aux sp (length vids1) x vid2))
)

(cffi::defcfun ("set_min" set-min) :int
    "Post minimum of SetVar constraint."
    (sp :pointer)
    (vid1 :int)
)

(cffi::defcfun ("set_max" set-max) :int
    "Post maximum of SetVar constraint."
    (sp :pointer)
    (vid1 :int)
)

(cffi::defcfun ("set_min_reify" set-min-reify) :void
    "Post minimum of SetVar constraint with reification."
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (r :int)
    (mode :int)
)

(cffi::defcfun ("set_max_reify" set-max-reify) :void
    "Post maximum of SetVar constraint with reification"
    (sp :pointer)
    (vid1 :int)
    (vid2 :int)
    (r :int)
    (mode :int)
)

(cffi::defcfun ("set_union" set-union-aux) :void
    "Post setVar cardinality constraint."
    (sp :pointer)
    (vid1 :int)
    (n :int)
    (vids :pointer)
)

(defun set-union (sp vid1 vids)
    "Post cardinality constraint on the SetVars denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (set-union-aux sp vid1 (length vids) x))
)

(cffi::defcfun ("element" element-aux) :void
    "Post setVar element constraint."
    (sp :pointer)
    (set-op :int)
    (n :int)
    (vids :pointer)
    (vid1 :int)
    (vid2 :int)
)

(defun element (sp set-op vids vid1 vid2)
    "Post cardinality constraint on the SetVars denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (element-aux sp set-op (length vids) x vid1 vid2))
)

(cffi::defcfun ("branch" branch-aux) :void
    "Post branching on the n IntVars denoted by vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (var-strat :int)
    (val-strat :int)
)

(defun branch (sp vids var-strat val-strat)
    "Post branching on the IntVars denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (branch-aux sp (length vids) x var-strat val-strat))
)

(cffi::defcfun ("branch_b" branch-b-aux) :void
    "Post branching on the n BoolVars denoted by vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (var-strat :int)
    (val-strat :int)
)

(defun branch-b (sp vids var-strat val-strat)
    "Post branching on the BoolVars denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (branch-b-aux sp (length vids) x var-strat val-strat))
)

(cffi::defcfun ("branch_set" branch-set-aux) :void
    "Post branching on the n SetVars denoted by vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
    (var_strat :int)
    (val_strat :int)
)

(defun branch-set (sp vids var_strat val_strat)
    "Post branching on the SetVars denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (branch-set-aux sp (length vids) x var_strat val_strat))
)

(cffi::defcfun ("cost" set-cost) :void
    "Define which variables is to be the cost."
    (sp :pointer)
    (vid :pointer :int)
    (amount :int)
)

(cffi::defcfun ("new_time_stop" new-time-stop) :pointer
    "Create a new TimeStop object to specify the time after which the search should stop"
    (max-time :int)
)

(cffi::defcfun ("reset_time_stop" reset-time-stop) :void
    "Reset the timer of the timeStop object"
    (t-stop :pointer)
)

(cffi::defcfun ("new_search_options" new-search-options) :pointer
    "Create a new options object to specify the search options"
)

(cffi::defcfun ("set_nb_threads" set-nb-threads) :int
    "Sets the number of threads to use during the search"
    (s-opts :pointer)
    (n-threads :int)
)

(cffi::defcfun ("set_time_stop" set-t-stop) :pointer
    "Sets the stop field of the Options object to the timeStop object"
    (s-opts :pointer)
    (t-stop :pointer)
)

(cffi::defcfun ("new_bab_engine" bab-engine-low) :pointer
    "Create a new branch and bound search-engine."
    (sp :pointer)
    (opts :pointer)
)

(cffi::defcfun ("bab_next" bab-next) :pointer
    "Find the next solution for the search-engine se."
    (se :pointer)
)

(cffi::defcfun ("bab_stopped" bab-stopped) :int
    "returns t if the search engine has been stopped, nil otherwise"
    (se :pointer)
)

(cffi::defcfun ("new_dfs_engine" dfs-engine-low) :pointer
    "Create a new depth-first search search-engine."
    (sp :pointer)
    (opts :pointer)
)

(cffi::defcfun ("dfs_next" dfs-next) :pointer
    "Find the next solution for the search-engine se."
    (se :pointer)
)

(cffi::defcfun ("dfs_stopped" dfs-stopped) :int
    "returns t if the search engine has been stopped, nil otherwise"
    (se :pointer)
)

(cffi::defcfun ("get_value" get-value) :int
    "Get the value of the variable denoted by vid."
    (sp :pointer)
    (vid :int)
)

(cffi::defcfun ("get_value_bool" get-value-bool) :int
    "Get the value of the variable denoted by vid."
    (sp :pointer)
    (vid :int)
)

(cffi::defcfun ("get_value_set" get-value-set-aux) :pointer
    "Get the value of the variable denoted by vid."
    (sp :pointer)
    (vid :int)
    (n :int)
)

(defun get-value-set (sp vid n)
    "get all the values of a SetVar"
    (let ((p (get-value-set-aux sp vid n)))
        (loop for i from 0 below n
            collect (cffi::mem-aref p :int i)))
)

(cffi::defcfun ("get_value_size" get-value-size) :int
    "Get the size of the solution of SetVar denoted by vid."
    (sp :pointer)
    (vid :int)
)

(cffi::defcfun ("get_values" get-values-aux) :pointer
    "Get the values of the n variables denoted by vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
)


(cffi::defcfun ("get_values_bool" get-values-bool-aux) :pointer
    "Get the boolean values of the n variables denoted by vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
)

(defun get-values (sp vids)
    "Print the values of the variables denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids))
           p)
        (setq p (get-values-aux sp (length vids) x))
        (loop for i from 0 below (length vids)
            collect (cffi::mem-aref p :int i)))
)

(defun get-values-bool (sp vids)
    "Print the boolean values of the variables denoted by vids."
    (let (
        (x (cffi::foreign-alloc :int :initial-contents vids))
        p
        )
        (setq p (get-values-bool-aux sp (length vids) x))
        (loop for i from 0 below (length vids)
            collect (cffi::mem-aref p :int i)))
)

(cffi::defcfun ("print_vars" print-vars-aux) :void
    "Print the values of the n variables denoted by vids."
    (sp :pointer)
    (n :int)
    (vids :pointer)
)

(defun print-vars (sp vids)
    "Print the values of the variables denoted by vids."
    (let ((x (cffi::foreign-alloc :int :initial-contents vids)))
        (print-vars-aux sp (length vids) x))
)
