(in-package :mldz)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ALL-DIFFERENT-NOTES constraint ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; <sp> is the space
; <notes> is a list of IntVars representing the pitch of the notes
; ensures that all the notes are different in terms of strict value, not in terms of notes
; (e.g. 60 and 72 can be values taken by two variables simultaneously even though they both represent a C)
(defun all-different-notes (sp notes)
    (gil::g-distinct sp notes)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; MELODY DIRECTION constraints ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; <sp> is the space
; <notes> is a list of IntVars representing the pitch of the notes
; posts the constraint that notes[i] < notes[i+1]
(defun strictly-increasing-pitch (sp notes)
    (gil::g-rel sp notes gil::IRT_LE nil) ; nil = v2
)

; <sp> is the space
; <notes> is a list of IntVars representing the pitch of the notes
; posts the constraint that notes[i] <= notes[i+1]
(defun increasing-pitch (sp notes)
    (gil::g-rel sp notes gil::IRT_LQ nil) ; nil = v2
)

; <sp> is the space
; <notes> is a list of IntVars representing the pitch of the notes
; <intervals> is a list if IntVars representing the intervals between consecutive notes
; <global-interval> is the global interval that the melody spans
; ensures that the melodic direction is mostly upwards
; TODO PEUT-ETRE EN FAIRE 2, CELLE-CI ET UNE QUI DIT QUE LES INTERVALLES DESCENDANTS DOIVENT ETRE PETITS
(defun mostly-increasing-pitch (sp notes intervals global-interval)
    (let (sum interval-domain-greater-than-zero) 
        (setq sum (gil::add-int-var sp -127 127)) ;  variable to hold the result of the sum of all intervals (cant' be bigger than 127)
        (setq interval-domain-greater-than-zero (loop :for n :from 1 :below 13 :by 1 collect n)); [1..12]

        (gil::g-sum sp sum intervals); sum = sum(intervals)
        (gil::g-rel sp sum gil::IRT_GQ (parse-integer global-interval)); sum >= global-interval
        (gil::g-count sp intervals interval-domain-greater-than-zero gil::IRT_GQ (ceiling (* 80 (length intervals)) 100)); (nb of intervals > 0) >= 0.8*size(intervals)
    )
)

; <sp> is the space
; <notes> is a list of IntVars representing the pitch of the notes
; posts the constraint that notes[i] > notes[i+1]
(defun strictly-decreasing-pitch (sp notes)
    (gil::g-rel sp notes gil::IRT_GR nil) ; nil = v2
)

; <sp> is the space
; <notes> is a list of IntVars representing the pitch of the notes
; posts the constraint that notes[i] >= notes[i+1]
(defun decreasing-pitch (sp notes)
    (gil::g-rel sp notes gil::IRT_GQ nil) ; nil = v2
)

; <sp> is the space
; <notes> is a list of IntVars representing the pitch of the notes
; <intervals> is a list if IntVars representing the intervals between consecutive notes
; <global-interval> is the global interval that the melody spans
; ensures that the melodic direction is mostly downwards
(defun mostly-decreasing-pitch (sp notes intervals global-interval)
    (let (sum interval-domain-smaller-than-zero)
        (setq sum (gil::add-int-var sp (- (* 127 (length intervals))) -1)); variable to hold the result of the sum of the intervals
        (setq interval-domain-smaller-than-zero (loop :for n :from 1 :to 13 :by 1 collect (- n))); [-12...-1]

        (gil::g-sum sp sum intervals); sum = sum(intervals)
        (gil::g-rel sp sum gil::IRT_LQ (- (parse-integer global-interval))); sum <= -global-interval
        (gil::g-count sp intervals interval-domain-smaller-than-zero gil::IRT_GQ (ceiling (* 80 (length intervals)) 100)); (nb of intervals < 0) >= 0.8*size(intervals)
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;
; AT LEAST N constraint ;
;;;;;;;;;;;;;;;;;;;;;;;;;

;<sp> is the space
;notes> is the array of variables representing the pitch
;<values> is the array of values 
;<n> is the number of variables such that notes[i] = values[i]
; Ensures that the number of times that notes[i] = values[i] is at least n
(defun at-least-n (sp notes values n)
    (gil::g-count-array sp notes values gil::IRT_EQ n)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; RANGE RESTRICTION constraint ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;<sp> is the space
;<notes> is the array of variables representing the pitch
;<input-chords> is the input chords in the form of a voice object
; Ensures that the melody is always higher than the lowest note from the chords and lower than the highest note of the chord + 2 octaves
(defun range-restriction (sp notes input-chords)
    (let ((chords (om::chords input-chords)); get the list of pitches for all the chords in the voice object
        vals min-note max-note)
        ; collect all the notes and put them in one big list
        (setf vals
                (to-midi (om::flat 
                    (loop :for n :from 0 :below (length chords) :by 1 collect (om::lmidic (nth n chords)))
                ))
        )
        (setf min-note (min-list vals)); get the lowest note
        (setf max-note (max-list vals)); get the highest note

        (dolist (note notes);restrain the interval domains to acceptable values 
            (gil::g-rel sp note gil::IRT_GQ min-note) ; note >= min-note
            (gil::g-rel sp note gil::IRT_LQ (+ max-note 24)) ; note <= max-note + 2 octaves (24 semitones); p-e retirer
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; INTERVAL-BETWEEN-ADJACENT-NOTES constraint ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; <sp> is the space
; <notes> is a list of IntVars representing the pitch of the notes
; <intervals> is a list of IntVars representing the intervals between successive notes from the notes argument
; Ensures that the interval between two adjacent notes is valid
; The interval must be everything up to a minor sixth (it can be an octave) except for a tritone, a major seven or a minor seven
; (-1 -4 -5 -7 -8 -9 -12 0 1 2 3 4 5 7 8 12) admissible intervals
; IMPROVEMENT IDEA : PRIORITIZE SMALLER INTERVALS
(defun interval-between-adjacent-notes (sp notes intervals)
    (let ((valid-intervals '(-1 -4 -5 -7 -8 -9 -12 0 1 2 3 4 5 7 8 12)))
        (dolist (interval intervals);restrain the interval domains to acceptable values 
            (gil::g-dom sp interval valid-intervals)
        )
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;
; IN TONALITY constraint ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

; <sp> is the space
; <notes> is the variable array on which the constraint is executed
; <key> is the key 
; <mode> is the mode
; Ensures that the notes are in the tonality specified by the user(e.g. C major)
(defun in-tonality (sp notes key mode)
    (let (admissible-notes)
        (setf admissible-notes (notes-from-tonality key mode)); call the function to get the list of notes in that tonality
        ; set the domain of each variable to the set of notes from the tonality
        (loop :for j :from 0 :below (length notes) :do
            (gil::g-dom sp (nth j notes) admissible-notes)
        )
    )
)

; returns the set of notes that are in the tonality specified by the arguments
(defun notes-from-tonality (key mode)
    (let (admissible-notes note scale i)
        ; get the scale in semi-tones
        (setq scale (get-scale mode))
        ; then, create a list and add the notes in it
        (setq note key)
        (setq i 0)
        (setq admissible-notes (list))
        ; add all notes above the key, then add all notes below the key
        (om::while (<= note 127) :do
            (setq admissible-notes (cons note admissible-notes)); add it to the list --(push note admissible-notes)?
            (if (>= i (length scale))
                (setq i 0)            
            )
            (incf note (nth i scale)); note = note + scale[i mod 6]
            (incf i 1); i++
        )
        (setq note key)
        (decf note (nth (- (- (length scale) 1) 0) scale)); note = note - scale[6-i mod 6]
        (setq i 1)

        (om::while (>= note 0) :do
            (setq admissible-notes (cons note admissible-notes)); add it to the list
            (if (>= i (length scale))
                (setq i 0)
            )
            (decf note (nth (- (- (length scale) 1) i) scale)); note = note - scale[6-i mod 6]
            (incf i 1); i++
        )
        admissible-notes
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; HARMONIC-INTERVAL-CHORD constraint ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; <sp> is the space
; <notes> is the variable array on which the constraint is executed
; <input-rhythm> is the voice object of the rhythm for the melody we want to find
; <chords> is the voice object of the chords
; enforces that the melodic interval for the melody in the context of a given chord is a maximum of an octave
(defun harmonic-interval-chord (sp notes input-rhythm chords)
    (let ((melody-starting-times (voice-onsets input-rhythm)); get the starting time of each of the notes of the melody
        (chords-starting-times (voice-onsets chords)); get the starting time of each of the notes of the chords
        (chord-counter 0); counter to know which chord we are currently looking at
        (variable-counter 0); counter to keep track of which note we are currently looking at
        (local-starting-note-melody 0); counter to keep track of which variable is the start of the melody over the given chord
        (starting-time-next-chord 0))
        (dolist (c chords-starting-times); go through the chords starting times
            (setf starting-time-next-chord (nth (+ 1 chord-counter) chords-starting-times)); get the starting time of the next chord
            (setf local-starting-note-melody variable-counter); keep a track of the first note in the context of this chord
            (cond 
                ((>= (+ 1 variable-counter) (length melody-starting-times)); we reached the last note
                        (return ); no need to go through notes if there are no more notes
                )
            )

            (dolist (m (subseq melody-starting-times variable-counter)); go through all the notes that we haven't seen yet
                (cond 
                    ((= variable-counter (- (length melody-starting-times) 1)); we reached the last note
                        (hic-constraint sp notes local-starting-note-melody variable-counter)
                    )
                    ((typep starting-time-next-chord 'null); if we are looking at the last chord
                        ; check qu'il reste des notes
                        (hic-constraint sp notes local-starting-note-melody (- (length melody-starting-times) 1)); post the constraint on all remaining notes
                        (return )
                    )
                    ((< m starting-time-next-chord); if the note is in the context of this chord
                        (incf variable-counter 1); simply increment the counter for the melody
                    )
                    ((>= m starting-time-next-chord); the note is not in the context of this chord
                        ; post the constraint on the corresponding notes
                        (if (>= (- variable-counter 1) local-starting-note-melody); if it is smaller, there is no note in the context of that chord
                            (hic-constraint sp notes local-starting-note-melody (- variable-counter 1))
                        )
                        (return )
                    )
                )
            )
            (setf chord-counter (+ 1 chord-counter))
        )
    )
)

; post the constraint that max(pitch[starting-note],..., pitch[ending-note]) - min(pitch[starting-note],..., pitch[ending-note]) <= 12 (an octave)
(defun hic-constraint (sp notes starting-note ending-note)
    (let (notes-array max-min-array)
        (setf notes-array (gil::add-int-var-array sp (+ (- ending-note starting-note) 1) 0 127)); create an array to represent all the notes played with that chord
        (setf max-min-array (gil::add-int-var-array sp 2 0 127)); create a table where the first value is the max and the second is the min
        (loop :for j :from 0 :below (+ (- ending-note starting-note) 1) :do ;for each element of note-array, make it equal to the note it represents
            (gil::g-rel sp (nth j notes-array) gil::IRT_EQ (nth (+ starting-note j) notes)); notes-array[j] = notes[j + starting-notes]
        )
        (gil::g-lmax sp (first max-min-array) notes-array); max-val = max(notes-array)
        (gil::g-lmin sp (second max-min-array) notes-array); min-val = min(notes-array)
        (gil::g-linear sp '(1 -1) max-min-array gil::IRT_LQ 12); max(notes-array) - min(notes-array) <= 12
    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NOTE-ON-CHORDS constraint ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; <sp> is the space
; <notes> is the variable array on which the constraint is executed
; <input-rhythm> is the voice object of the rhythm for the melody we want to find
; <chords> is the voice object of the chords
; Ensures that the notes played at the same time as a chord respect rules 
; TODO ADD SEVENTH CHORDS, AUGMENTED, DIMINISHED,...
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
        (setf admissible-notes (get-admissible-notes chord-pitch mode inversion))
        (gil::g-dom sp (nth variable-id notes) admissible-notes); post the constraint that the domain of that variable is one of the admissible notes
        (gil::g-rel sp (nth variable-id notes) gil::IRT_LQ (+ (max (first chord-pitch)) 18)); post the constraint that the interval between the note of the melody and the highest note of the chord is maximum 1 octave
    )
)

