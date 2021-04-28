(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;
; SAMPLE CONSTRAINTS ;
;;;;;;;;;;;;;;;;;;;;;;

; Constraints are defined as instance of the class <constraint>, which is a container
; for a function that posts the gecode constraints and its parameters.
; 
; To create a constraint, one must create an OM method (i.e. via defmethod!) that returns
; the constraint instance. 
; For readability reasons, the following constraints always return a function called "post-<cst>"
; that calls the <cst> by rearranging the args.


; Constraint REL-PULSE
; The specified positions must contain a note of duration with the specified relation 
; to the specified value (e.g. equal to 1/4 or greater than 1/16).

; ALL-DIFFERENT-NOTES constraint
; sp is the space
; notes is a list of IntVars
; ensures that all the variables in the list are different in terms of strict value, not in terms of notes 
; (e.g. 60 and 72 can be values taken by two variables simultaneously even though they both represent a C)
(defun all-different-notes (sp notes)
    (gil::g-distinct sp notes)
)

; this function is from rhythm-box 
; https://github.com/blapiere/Rhythm-Box

(defun rel-to-gil (rel)
"Convert a relation operator symbol to a GiL relation value."
    (cond
        ((eq rel '=) gil::IRT_EQ)
        ((eq rel '=/=) gil::IRT_NQ)
        ((eq rel '<) gil::IRT_LE)
        ((eq rel '=<) gil::IRT_LQ)
        ((eq rel '>) gil::IRT_GR)
        ((eq rel '>=) gil::IRT_GQ)
    )
)

