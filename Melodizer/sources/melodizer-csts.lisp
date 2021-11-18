(in-package :mldz)

;;;;;;;;;;;;;;;;;;;;;;
; SAMPLE CONSTRAINTS ;
;;;;;;;;;;;;;;;;;;;;;;

; ALL-DIFFERENT-NOTES constraint WORKS
; <sp> is the space
; <notes> is a list of IntVars
; ensures that all the notes are different in terms of strict value, not in terms of notes
; (e.g. 60 and 72 can be values taken by two variables simultaneously even though they both represent a C)
(defun all-different-notes (sp notes)
    (gil::g-distinct sp notes)
)

; DISSONNANCE RESOLUTION constraint TODO + develop comments
; Ensures that every sensitive note (4th or 7th) is eventually followed by the fundamental
; if it is a seventh note, it is followed by the fundamental that is above it (+1 if major, +2 if minor)
; if it is a fourth note, it is followed by either of the fundamentals around it (+7 or -5)
(defun dissonnance-resolution (sp notes key mode)
    #| (if (string-equal mode "major")

    ) |#
)

; INTERVAL-BETWEEN-ADJACENT-NOTES constraint WORKS
; <sp> is the space
; <notes> is a list of IntVars representing the pitch of the notes
; <intervals> is a list of IntVars representing the intervals between successive notes from the notes argument
; Ensures that the interval between two adjacent notes is valid
; The interval must be everything up to a minor sixth (it can be an octave) except for a tritone, a major seven or a minor seven
; ;(-1 -2 -3 -4 -5 -7 -8 -9 -12 0 1 2 3 4 5 7 8 9 12) admissible intervals
; new version (-1 -4 -5 -7 -8 -9 -12 0 1 2 3 4 5 7 8 12)
; IMPROVEMENT IDEA : PRIORITIZE SMALLER INTERVALS
(defun interval-between-adjacent-notes (sp notes intervals)
    (let ((valid-intervals '(-1 -4 -5 -7 -8 -9 -12 0 1 2 3 4 5 7 8 12)))
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
; <sp> is the space
; <notes> is the variable array on which the constraint is executed
; <key> is the key 
; <mode> is the mode
; Ensures that the notes are in the tonality specified by the user(e.g. C major)
; TODO ADD OTHER MODES (NATURAL MINOR, ... )
(defun in-tonality (sp notes key mode)
    (let (admissible-notes)
        (setf admissible-notes (notes-from-tonality key mode))
        ; set the domain of each variable
        (loop :for j :from 0 :below (length notes) :do
            (gil::g-dom sp (nth j notes) admissible-notes)
        )
    )
)

; NOTE-ON-CHORDS constraint
; <sp> is the space
; <notes> is the variable array on which the constraint is executed
; <input-rhythm> is the voice object of the rhythm for the melody we want to find
; <chords> is the voice object of the chords
; Ensures that the notes played at the same time as a chord respect rules DEVELOP
(defun note-on-chord (sp notes input-rhythm chords)
    (let ((melody-starting-times (voice-onsets input-rhythm)); get the starting time of each of the notes of the melody
        (chords-starting-times (voice-onsets chords)); get the starting time of each of the notes of the chords
        (chord-counter 0); counter to know which chord we are currently looking at
        (variable-counter 0)); counter to keep track of which note we are currently looking at
        (dolist (c chords-starting-times); go through the chords starting times
            (dolist (m (subseq melody-starting-times variable-counter));go through the input-rhythm starting times
                (cond 
                    ((< m c) ; if the note is played before the chord, simply increment the counter for variables
                        (setf variable-counter (+ variable-counter 1))
                    )
                    ((= m c) ; if they are played at the same time, post the constraint on that specific variable
                        (apply-constraint-noc sp notes variable-counter chord-counter chords) 
                        (setf variable-counter (+ variable-counter 1))
                        (return )
                    )
                    ((> m c) ; if it is bigger, break the loop and go to the next chord
                        (return ) 
                    )
                )
            )
            (setf chord-counter (+ 1 chord-counter))
        )
    )
)

; get the set of notes that can be played on that chord (see c++ code) and restrain the domain of pitch[variable-id] to that
(defun apply-constraint-noc (sp notes variable-id chord-id input-chords)
    (let ((chord-pitch (to-midi (om::lmidic (nth chord-id (om::chords input-chords))))); get the values of the notes of the chord
            intervals mode inversion admissible-notes)
        (sort chord-pitch #'<) ; sort the note values in increasing order
        (setf intervals (list 
            (- (second chord-pitch) (first chord-pitch)) 
            (- (third chord-pitch) (second chord-pitch)))); get the intervals in semitones between the different notes of the chord
        ; change that so there is only one function call
        (setf mode (first (get-mode-and-inversion intervals))); get the mode of the chord from the intervals between the notes
        (setf inversion (second (get-mode-and-inversion intervals))); get the inversion of the chord
        ;get the notes playable on that chord
        (setf admissible-notes (get-admissible-notes chord-pitch mode))
        (gil::g-dom sp (nth variable-id notes) admissible-notes); post the constraint that the domain of that variable is one of the admissible notes
    )
)










; PRECEDENCE test
(defun precedence(sp notes val1 val2)
    (gil::g-precede sp notes val1 val2)
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

