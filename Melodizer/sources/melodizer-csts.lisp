(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;
; SAMPLE CONSTRAINTS ;
;;;;;;;;;;;;;;;;;;;;;;

; ALL-DIFFERENT-NOTES constraint WORKS
; sp is the space
; notes is a list of IntVars
; ensures that all the notes are different in terms of strict value, not in terms of notes
; (e.g. 60 and 72 can be values taken by two variables simultaneously even though they both represent a C)
(defun all-different-notes (sp notes)
    (gil::g-distinct sp notes)
)

; DISSONNANCE RESOLUTION constraint
; Ensures that every sensitive note (4th or 7th) is eventually followed by the fundamental
; if it is a seventh note, it is followed by the fundamental that is above it (+1 if major, +2 if minor)
; if it is a fourth note, it is followed by either of the fundamentals around it (+7 or -5)
(defun dissonnance-resolution (sp notes key mode)
    #| (if (string-equal mode "major")

    ) |#
)

; INTERVAL-BETWEEN-ADJACENT-NOTES constraint WORKS
; sp is the space
; notes is a list of IntVars representing the pitch of the notes
; intervals is a list of IntVars representing the intervals between successive notes from the notes argument
; Ensures that the interval between two adjacent notes is valid
; The interval must be everything up to an octave except for a tritone, a major seven or a minor seven
; ;(-1 -2 -3 -4 -5 -7 -8 -9 -12 0 1 2 3 4 5 7 8 9 12) admissible intervals
; IMPROVEMENT IDEA : PRIORITIZE SMALLER INTERVALS
(defun interval-between-adjacent-notes (sp notes intervals)
    (let ((valid-intervals '(-1 -2 -3 -4 -5 -7 -8 -9 -12 0 1 2 3 4 5 7 8 9 12)))
        (dolist (interval intervals);restrain the interval domains to acceptable values 
            (gil::g-dom sp interval valid-intervals)
        )
        (loop :for j :from 0 :below (length intervals) :do ;for each interval
            (let (temp)
                (setq temp (gil::add-int-var-array sp 3 -12 108)); temporary variables to make it easier to apply the linear constraint

                (gil::g-rel sp (first temp) gil::IRT_EQ (nth j notes)); temp[0] = notes[j]
                (gil::g-rel sp (second temp) gil::IRT_EQ (nth (+ j 1) notes)); temp[1] = notes[j+1]
                (gil::g-rel sp (third temp) gil::IRT_EQ (nth j intervals)); temp[2] = intervals[j]

                (gil::g-linear sp '(1 -1 -1) temp gil::IRT_EQ 0); notes[j] - notes[j+1] - intervals[j] = 0
            )
        )
    )
)


; IN TONALITY constraint WORKS
; Ensures that the notes are in the tonality specified by the user(e.g. C major)
; sp is the space
; notes is the variable array on which the constraint is executed
; key is the key 
;mode is the mode
(defun in-tonality (sp notes key mode)
    (let (scale note admissible-notes i)
        ; set the scale to major or minor
        (if (string-equal mode "major") ; maybe add a security so if the user types something wrong it doesn't set the mode to minor by default
            (setq scale (list 2 2 1 2 2 2 1)); major
            (setq scale (list 2 1 2 2 1 2 2)); minor
        )
        ; then, create a list and add the notes in it
        (setq note key)
        (setq i 0)
        (setq admissible-notes (list))
        ; add all notes over the key, then add all notes under the key
        (while (<= note 127) :do
            (setq admissible-notes (cons note admissible-notes)); add it to the list --(push note admissible-notes)?
            (if (>= i 7)
                (setq i 0)
            )
            (incf note (nth i scale)); note = note + scale[i mod 6]
            (incf i 1); i++
        )

        (setq note key)
        (decf note (nth (- 6 0) scale)); note = note - scale[6-i mod 6]
        (setq i 1)

        (while (>= note 0) :do
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

; old version
#| (defun interval-between-adjacent-notes (sp notes)
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
                (gil::g-linear sp (list 1 -1 -1) temp (rel-to-gil '=) 0)
            )
        )
    )
) |#

