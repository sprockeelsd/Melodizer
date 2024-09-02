(cl:defpackage "gilf"
  (:nicknames "GILF")
   (:use common-lisp :cl-user :cl :cffi))

(in-package :gilf)
(in-package :fuxcp)

(print "Loading gecode-wrapper...")

(defparameter DFS 0)
(defparameter BAB 1)
; corresponds to enum values in gecode_problem.h, but can be used graphically in om
(defun bab ()
    BAB
)
(defun dfs ()
    DFS
)

;;;;;;;;;;;;;;;;;;;;;
;; Problem methods ;;
;;;;;;;;;;;;;;;;;;;;;


;; This function is used to convert lisp lists into int pointers so they can be passed to c++
(defun new-ctp-problem (cf splist voicetypes borrowmode minskips generalcst motioncst melodiccst specificcst costimp toffset scale chromscale borscale)
    (let (
        (cfi (cffi::foreign-alloc :int :initial-contents cf))
        (spl (cffi::foreign-alloc :int :initial-contents splist))
        (vt (cffi::foreign-alloc :int :initial-contents voicetypes))
        (gen (cffi::foreign-alloc :int :initial-contents generalcst))
        (mot (cffi::foreign-alloc :int :initial-contents motioncst))
        (mel (cffi::foreign-alloc :int :initial-contents melodiccst))
        (spe (cffi::foreign-alloc :int :initial-contents specificcst))
        (cst (cffi::foreign-alloc :int :initial-contents costimp))
        (sca (cffi::foreign-alloc :int :initial-contents scale))
        (chr (cffi::foreign-alloc :int :initial-contents chromscale))
        (bor (cffi::foreign-alloc :int :initial-contents borscale))
    )
    (new-problem cfi (length cf) (length splist) spl vt borrowmode minskips gen mot mel spe cst toffset sca (length scale) chr (length chromscale) bor (length borscale))
    )
)

(cffi::defcfun ("create_new_problem" new-problem) :pointer
    "Creates a new instance of the problem. Returns a void* cast of a Problem*."

    (cantus-firmus          :pointer :int)  ; the cantus firmus, in MIDI values
    (cf-size                :int)           ; the size of the cantus firmus
    (n-counterpoints        :int)           ; number of counterpoints
    (species-list           :pointer :int)  ; list of the species of the counterpoints (size=n-counterpoints)
    (voice-types            :pointer :int)  ; list of the voice types of the counterpoints (size=n-counterpoints)
    (borrow-mode            :int)           ; borrowing mode : 0 if "None", 1 if "Major", 2 if "Minor"
    (min-skips-percent      :int)           ; min skips slider
    (general-cost-values    :pointer :int)  ; size = 8
    (motion-cost-values     :pointer :int)  ; size = 3
    (melodic-cost-values    :pointer :int)  ; size = 8
    (specific-cost-values   :pointer :int)  ; size = 7
    (cost-importances       :pointer :int)  ; size = 14
    (tonalite-offset        :int)
    (scale                  :pointer :int)  
    (scale-size             :int)
    (chromatic-scale        :pointer :int)
    (chromatic-scale-size   :int)
    (borrowed-scale         :pointer :int)
    (borrowed-scale-size    :int)
    ; TODO add here any additional arguments that your Problem constructor takes
)

(cffi::defcfun ("get_size" get-size) :int
    "Returns the size of the space."
    (sp :pointer) ; a void* cast of a Problem*
)


(cffi::defcfun ("delete_pointer" delete-pointer) :void
    "Deletes the pointer passed as argument. "
    (p :pointer) 
)

(cffi::defcfun ("delete_solver_pointer" delete-solver-pointer) :void
    "Deletes the pointer to a solver (search engine) passed as argument. "
    (p :pointer) 
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Search engine methods ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cffi::defcfun ("create_solver" create-solver) :pointer
    "Creates a DFS<Problem> object. Returns a void* cast of a DFS<Problem> object."
    (sp :pointer) ; a void* cast of a Problem*
    (solver-type :int); an integer representing the type of the solver (see above)
)

(cffi::defcfun ("return_next_solution_space" return-next-solution-space) :pointer
    "Returns a pointer to the next solution of the problem. Returns a void* cast of a Problem*."
    (solver :pointer) ; a void* cast of a Base<Problem>* pointer
)


(cffi::defcfun ("search_stopped" search-stopped) :int
    "Returns 1 if the seach was stopped by the TimeStop object, and 1 otherwise."
    (solver :pointer) ; a void* cast of a Base<Problem>* pointer
)


;;;;;;;;;;;;;;;;;;;;;;;
;; Solution handling ;;
;;;;;;;;;;;;;;;;;;;;;;;

(cffi::defcfun ("return_solution" return-solution) :pointer
    "Returns a int* that are the values of a solution."
        (sp :pointer) ; a void* cast of a Problem object that is a solution of the problem.
)


;; ============= NEEDED BY 5SP PARSER ==================

(cffi::defcfun ("return_species_array_5sp" return-species-array-5sp) :pointer
    "Returns a int* that are the values of the species array for the fifth species, of the ctp-index'th counterpoint."
        (sp :pointer) ; a void* cast of a Problem object that is a solution of the problem.
        (ctp-index :int) ; the index of the counterpoint (is it the first, the second or the third counterpoint)
)

(cffi::defcfun ("return_extended_cp_domain" return-extended-cp-domain) :pointer
    "Returns a int* that are the values of the extebded cp domain (for the fifth species parser), of the ctp-index'th counterpoint."
        (sp :pointer) ; a void* cast of a Problem object that is a solution of the problem.
        (ctp-index :int) ; the index of the counterpoint (is it the first, the second or the third counterpoint)
)

(cffi::defcfun ("get_extended_cp_domain_size" get-extended-cp-domain-size) :int
    "Returns the size of the extended cp domain."
        (sp :pointer) ; a void* cast of a Problem*
        (ctp-index :int)
)


(defun species-array-to-int-array (sp ctp-index cf-len)
    "Returns the values the variables have taken in the solution as a list of integers. Casts a int* into a list of numbers."
    "sp is a void* cast of a Problem* that is a solution to the problem. Calling this funciton on a non-solution 
        will result in an error."
    
        (if (cffi::null-pointer-p sp)
            (error "Error when retrieving species array for the fifth species.")
        )
    (let* (
            (size (- (* cf-len 4) 3))
            (ptr (return-species-array-5sp sp ctp-index))
        )
        (loop for i from 0 below size
            collect (cffi::mem-aref ptr :int i)
        )
    )
)

(defun ext-domain-to-int-array (sp ctp-index)
    "Returns the values the variables have taken in the solution as a list of integers. Casts a int* into a list of numbers."
    "sp is a void* cast of a Problem* that is a solution to the problem. Calling this funciton on a non-solution 
        will result in an error."
        (if (cffi::null-pointer-p sp) ; TODO check
            (error "No (more) solutions.")
        )
    (let* (
            (size (get-extended-cp-domain-size sp ctp-index))
            (ptr (return-extended-cp-domain sp ctp-index))
        )
        (loop for i from 0 below size
            collect (cffi::mem-aref ptr :int i)
        )
    )
)


;;  ======================================================

(defun solution-to-int-array (sp)
    "Returns the values the variables have taken in the solution as a list of integers. Casts a int* into a list of numbers."
    "sp is a void* cast of a Problem* that is a solution to the problem. Calling this funciton on a non-solution 
        will result in an error."
        (if (cffi::null-pointer-p sp) ; TODO check
            (error "No (more) solutions.")
        )
    (let* (
            (size (get-size sp))
            (ptr (return-solution sp))
        )
        (loop for i from 0 below size
            collect (cffi::mem-aref ptr :int i)
        )
    )
)


(print "finished loading gecode-wrapper")