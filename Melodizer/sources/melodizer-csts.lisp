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


; ALL-DIFFERENT-NOTES constraint
; sp is the space
; notes is a list of IntVars
; ensures that all the variables in the list are different in terms of strict value, not in terms of notes 
; (e.g. 60 and 72 can be values taken by two variables simultaneously even though they both represent a C)
(defun all-different-notes (sp notes)
    (gil::g-distinct sp notes)
)

; INTERVAL-BETWEEN-ADJACENT-NOTES constraint
;ensures that the interval between two adjacent notes is valid
(defun interval-between-adjacent-notes (sp notes)
    (let (intervals valid-intervals n); array of IntVars representing the intervals between two adjacent notes
        (setq valid-intervals (list -1 -2 -3 -4 -5 -7 -8 -9 -12 0 1 2 3 4 5 7 8 9 12)); admissible values for the intervals)
        (setq n (- (length notes) 1))
        (setq intervals (gil::add-int-var-array sp n -12 12))
        (loop :for i :from 0 :below 3 :do
            (gil::g-dom sp (nth i intervals) valid-intervals)
        )
        (loop :for i :from 0 :below n :do ; for each interval
            (let (temp)
                (setq temp (gil::add-int-var-array sp 3 -12 108))
                (gil::g-dom-intvar sp (nth 0 temp) (nth i notes)); domain of temp[0] = domain of notes[i]
                (print n)
                (gil::g-dom-intvar sp (nth 1 temp) (nth (+ i 1) notes)); domain of temp[1] = domain of notes[i+1]
                (gil::g-dom-intvar sp (nth 2 temp) (nth i intervals)); domain of temp[2] = domain of intervals[i]

                ; linking the temporary variables to the ones they represent
                (gil::g-rel sp (nth 0 temp) (rel-to-gil =) (nth i notes)); note1 = notes[i]
                (gil::g-rel sp (nth 1 temp) (rel-to-gil =) (nth (+ i 1) notes))
                (gil::g-rel sp (nth 2 temp) (rel-to-gil =) (nth i intervals))

                ;adding the constraint on the interval
                (gil::g-linear sp (list 1 -1 -1) temp (rel-to-gil =) 0)
            )
        )
    )
)

; IN TONALITY constraint
; ensures that the notes are in the tonality specified by the user(e.g. C major)
; sp is the space
; notes is the variable array on which the constraint is executed
; key is the key, mode is the mode
; a mode of 0 represents major, and 1 represents minor
(defun in-tonality (sp notes key mode)
    (let (scale note admissible-notes i)
        ; set the scale to major or minor
        (if (= mode 0) 
            (setq scale (list 2 2 1 2 2 2 1)); major
            (setq scale (list 2 1 2 2 1 2 2)); minor
        )
        ; then, create a list and add the notes in it
        (setq note key)
        (setq i 0)
        (setq admissible-notes (list))
        ; add all notes over the key, then add all notes under the key
        (while (<= note 108) :do
            (setq admissible-notes (cons note admissible-notes)); add it to the list
            (if (>= i 7)
                (setq i 0)
            )
            (incf note (nth i scale)); note = note + scale[i mod 6]
            (incf i 1); i++
        )

        (setq note key)
        (decf note (nth (- 6 0) scale)); note = note - scale[6-i mod 6]
        (setq i 1)

        (while (>= note 21) :do
            (setq admissible-notes (cons note admissible-notes)); add it to the list
            (if (>= i 7)
                (setq i 0)
            )
            (decf note (nth (- 6 i) scale)); note = note - scale[6-i mod 6]
            (incf i 1); i++
        )
        ; set the domain of each variable
        (loop :for j :from 0 :below (length notes) :do
            (gil::g-dom sp (nth j notes) admissible-notes)
        )
    )
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

