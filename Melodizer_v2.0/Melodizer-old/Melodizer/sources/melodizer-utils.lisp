(in-package :mldz)

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

;converts the value of a note to its name
(defmethod note-value-to-name (note)
    (cond 
        ((eq note 60) "C")
        ((eq note 61) "C#")
        ((eq note 62) "D")
        ((eq note 63) "Eb")
        ((eq note 64) "E")
        ((eq note 65) "F")
        ((eq note 66) "F#")
        ((eq note 67) "G")
        ((eq note 68) "Ab")
        ((eq note 69) "A")
        ((eq note 70) "Bb")
        ((eq note 71) "B")
    )
)

;converts the name of a note to its value
(defmethod name-to-note-value (name)
    (cond 
        ((string-equal name "C") 60)
        ((string-equal name "C#") 61)
        ((string-equal name "D") 62)
        ((string-equal name "Eb") 63)
        ((string-equal name "E") 64)
        ((string-equal name "F") 65)
        ((string-equal name "F#") 66)
        ((string-equal name "G") 67)
        ((string-equal name "Ab") 68)
        ((string-equal name "A") 69)
        ((string-equal name "Bb") 70)
        ((string-equal name "B") 71)
    )
)

; finds the smallest element of a list
(defun min-list (L)
    (cond
        ((null (car L)) nil); the list is empty -> return nil
        ((null (cdr L)) (car L)); the list has 1 element -> return it
        (T 
            (let ((head (car L)); default behavior
                 (tailMin (min-list (cdr L))))
                (if (< head tailMin) head tailMin)
            )
        )
    )
)

; finds the biggest element of a list
(defun max-list (L)
    (cond
        ((null (car L)) nil); the list is empty -> return nil
        ((null (cdr L)) (car L)); the list has 1 element -> return it
        (T 
            (let ((head (car L)); default behavior
                 (tailMax (max-list (cdr L))))
                (if (> head tailMax) head tailMax)
            )
        )
    )
)


; finds the biggest element in a list of lists
(defun max-list-list (L)
    (cond
        ((null (car L)) nil); the list is empty -> return nil
        ((null (cdr L)) (max-list (car L))); the list has 1 element -> return it
        (T 
            (let ((head (max-list (car L))); default behavior
                 (tailMax (max-list-list (cdr L))))
                (if (> head tailMax) head tailMax)
            )
        )
    )
)

; function to update the list of solutions in a pop-up menu without having to close and re-open the window
; TODO find a more efficient way to do this
(defun update-pop-up (self my-panel data position size output)
  (om::om-add-subviews my-panel
    (om::om-make-dialog-item 
      'om::om-pop-up-dialog-item 
      position ;(om::om-make-point 5 130)
      size ;(om::om-make-point 320 20) 
      "list of solutions"
      :range (loop for item in (make-data-sol data) collect (car item))
      ;:value (mode (object self)); change so it goes to the newest added solution? 
      :di-action #'(lambda (m)
                    (cond 
                        ((string-equal output "output-solution")
                            (setf (output-solution (om::object self)) (nth (om::om-get-selected-item-index m) data)); set the output solution to the currently selected solution
                            (let ((indx (om::om-get-selected-item-index m)))
                                (om::openeditorframe ; open the editor of the selected solution
                                    (om::omNG-make-new-instance 
                                        (nth indx data)
                                        (format nil "melody ~D" (1+ indx)); name of the window
                                    )
                                )
                            )
                        )
                        ((string-equal output "output-motif")
                            (setf (output-motif (om::object self)) (nth (om::om-get-selected-item-index m) data))
                            (let ((indx (om::om-get-selected-item-index m)))
                                (om::openeditorframe
                                    (om::omNG-make-new-instance
                                        (output-motif (om::object self))
                                        (format nil "motif ~D" (1+ indx)); name of the window
                                    )
                                )
                            )
                        )
                        ((string-equal output "output-phrase")
                            (setf (output-phrase (om::object self)) (nth (om::om-get-selected-item-index m) data))
                            (let ((indx (om::om-get-selected-item-index m)))
                                (om::openeditorframe
                                    (om::omNG-make-new-instance
                                        (output-phrase (om::object self))
                                        (format nil "phrase ~D" (1+ indx)); name of the window
                                    )
                                )
                            )
                        )
                        ((string-equal output "output-period")
                            (setf (output-period (om::object self)) (nth (om::om-get-selected-item-index m) data))
                            (let ((indx (om::om-get-selected-item-index m)))
                                (om::openeditorframe
                                    (om::omNG-make-new-instance
                                        (output-period (om::object self))
                                        (format nil "period ~D" (1+ indx))
                                    )
                                )
                            )
                        )
                    )
      )
    )
  )
)

;function to get the starting times (in ms) of the notes
; from karim haddad (OM)
(defmethod voice-onsets ((self voice))
  "on passe de voice a chord-seq juste pour avoir les onsets"
    (let ((obj (om::objfromobjs self (make-instance 'om::chord-seq))))
        (butlast (om::lonset obj))
    )
)

;function to get the duration (in ms) of the notes
(defmethod voice-durs ((self voice))
  "on passe de voice a chord-seq juste pour avoir les onsets"
    (let ((obj (om::objfromobjs self (make-instance 'om::chord-seq))))
        (om::ldur obj)
    )
)

; returns the list of intervals defining a given mode
(defun get-scale (mode)
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
    )
)

; function to get all of a given note (e.g. C)
(defun get-all-notes (note)
    (let ((acc '()) (backup note))
        (om::while (<= note 127) :do
            (setq acc (cons note acc)); add it to the list
            (incf note 12)
        )
        (setf note (- backup 12))
        (om::while (>= note 0) :do
            (setq acc (cons note acc)); add it to the list
            (decf note 12)
        )
        acc
    )
)

; function to get all notes playable on top of a given chord CHECK WHAT NOTES CAN BE PLAYED FOR OTHER CASES THAN M/m
(defun get-admissible-notes (chords mode inversion)
    (let ((return-list '()))
        (cond
            ((string-equal mode "major"); on top of a major chord, you can play either of the notes from the chord though the preferred order is 1-5-3
                (setf return-list (reduce #'cons
                    (get-all-notes (first chords))
                    :initial-value return-list
                    :from-end t
                ))
                (setf return-list (reduce #'cons
                    (get-all-notes (second chords))
                    :initial-value return-list
                    :from-end t
                ))
                (setf return-list (reduce #'cons
                    (get-all-notes (third chords))
                    :initial-value return-list
                    :from-end t
                ))
            )
            ((string-equal mode "minor"); on top of a minor chord, you can play either of the notes from the chord though the preferred order is 1-5-3
                (setf return-list (reduce #'cons
                    (get-all-notes (first chords))
                    :initial-value return-list
                    :from-end t
                ))
                (setf return-list (reduce #'cons
                    (get-all-notes (second chords))
                    :initial-value return-list
                    :from-end t
                ))
                (setf return-list (reduce #'cons
                    (get-all-notes (third chords))
                    :initial-value return-list
                    :from-end t
                ))
            )
            ((string-equal mode "diminished"); only the third can be played on top of diminished chords
                (cond
                    ((= inversion 0)
                        (setf return-list (reduce #'cons
                            (get-all-notes (second chords))
                            :initial-value return-list
                            :from-end t
                        ))
                    )
                    ((= inversion 1)
                        (setf return-list (reduce #'cons
                            (get-all-notes (first chords))
                            :initial-value return-list
                            :from-end t
                        ))
                    )
                    ((= inversion 2)
                        (setf return-list (reduce #'cons
                            (get-all-notes (third chords))
                            :initial-value return-list
                            :from-end t
                        ))
                    )
                )
            )
        )
    )   
)

; function to get the mode of the chord (major, minor, diminished,...) and the inversion (0 = classical form, 1 = first inversion, 2 = second inversion)
(defun get-mode-and-inversion (intervals)
    (let ((major-intervals (list (list 4 3) (list 3 5) (list 5 4))); possible intervals in midi for major chords
        (minor-intervals (list (list 3 4) (list 4 5) (list 5 3))) ; possible intervals in midi for minor chords
        (diminished-intervals (list (list 3 3) (list 3 6) (list 6 3)))); possible intervals in midi for diminished chords
        (cond 
            ((position intervals major-intervals :test #'equal); if the chord is major
                (list "major" (position intervals major-intervals :test #'equal))
            )
            ((position intervals minor-intervals :test #'equal); if the chord is minor
                (list "minor" (position intervals minor-intervals :test #'equal))
            )
            ((position intervals diminished-intervals :test #'equal); if the chord is diminished
                (list "diminished" (position intervals diminished-intervals :test #'equal))
            )
        )
    )
)

;makes a list (name voice-instance) from a list of voices: 
;(from Karim Haddad)
(defun make-data-sol (liste)
  (loop for l in liste
        for i from 1 to (length liste)
        collect (list (format nil "solution ~D: ~A"  i l) l)))
    

; taken from rhythm box
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

; <input-chords> is the voice objects for the chords
; <quant> NOT USED YET (FORCED TO 500) smallest possible note length
; Return a list in which each element i represent a note starting at a time i*quant
; -1 means no note starting at that time, a chord object means multiple note starting
(defun create-push (input-chords)
    (let ((note-starting-times (voice-onsets input-chords))
          (quant 500)
          (push-list (list))
          (chords (to-pitch-list (om::chords input-chords))) ; get chords list
         )
         (setf note-starting-times (mapcar (lambda (n) (/ n quant)) note-starting-times)) ; dividing note-starting-times by quant
         (loop :for j :from 0 :below (+ (max-list note-starting-times) 1) :by 1 :do 
            (if (= j (car note-starting-times)); if j == note-starting-times[0] 
                (progn
                    (setq push-list (nconc push-list (list (car chords))))
                    (setf chords (cdr chords))
                    (setf note-starting-times (cdr note-starting-times))) ;add chords[0] to push and prune qt[0] and pchords[0]
                (setq push-list (nconc push-list (list -1)))) ; else add -1 to push
        )
        ; (print push-list)
    )   
)


; <input-chords> is the voice objects for the chords
; <quant> NOT USED YET (FORCED TO 500) smallest possible note length
; Return a list in which each element i represent a note stopping at a time i*quant
; -1 means no note stop at that time, a chord object means multiple note starting
(defun create-pull (input-chords)
    (let ((note-starting-times (voice-onsets input-chords)) ; note-starting-times = start time of each chord
          (note-dur-times (voice-durs input-chords)) ; note-dur-times = duration of each note
          (note-stopping-times (list))
          (quant 500)
          (pull-list (list))
          (pitch (to-pitch-list (om::chords input-chords))) ; get chords list
         )
         ; (print pitch)
         (setf note-starting-times (mapcar (lambda (n) (/ n quant)) note-starting-times)) ; dividing note-starting-times by quant
         ; (print note-starting-times)
         (setf note-dur-times (mapcar (lambda (n) (mapcar (lambda (m) (/ m quant)) n)) note-dur-times)) ; dividing note-dur-times by quant
         ; (print note-dur-times)
         (loop :for j :from 0 :below (length note-starting-times) :by 1 :do 
             (setq note-stopping-times (nconc note-stopping-times (list (mapcar (lambda (n) (+ n (nth j note-starting-times))) (nth j note-dur-times))))) ; Adding note-starting-times to note-dur-times to get note-stopping-times
        )
        ; (print note-stopping-times)
        ; (print (max-list-list note-stopping-times))
        (loop :for j :from 0 :below (+ (max-list-list note-stopping-times) 1) :by 1 :do 
              (setq pull-list (nconc pull-list (list -1))))
        (loop for l in note-stopping-times 
              for k in pitch do
            (loop for i in l 
                  for j in k do
                  (if (typep (nth i pull-list) 'list)
                      (setf (nth i pull-list) (nconc (nth i pull-list) (list j)))
                      (setf (nth i pull-list) (list j)))                 
             )
        )
        ; (print pull-list) 
    )   
)

(defun adapt-scale (major-natural)
    (let ((major-modified (list (first major-natural))))
         (loop :for i :from 1 :below (length major-natural) :by 1 :do
            (setq major-modified (nconc major-modified (list (+ (nth i major-natural) (nth (- i 1) major-modified)))))  
         ) 
    (return-from adapt-scale major-modified)
    )    
)

(defun build-scaleset (major-natural)
    (let ((major-modified (adapt-scale major-natural)))
        (loop :for octave :from 0 :below 11 :by 1 append
            (let ((offset 0)
                (scaleset (list)))
                (setq scaleset (nconc scaleset (mapcar (lambda (n) (+ n (* octave 12))) major-modified)))
            )
        )
    )
)


; <chords> a list of chord object
; Return the list of pitch contained in chords in midi format
(defun to-pitch-list (chords)
     (loop :for n :from 0 :below (length chords) :by 1 collect (to-midi (om::lmidic (nth n chords))))
)


; Getting a list of chords and a rhythm tree from the playing list of intvar
(defun build-score (sol playing)
    (let ((pitch (list))
          (tree (list '? (list (list (list 4 4) (list 1 1 1 1 1 1 1 1)) (list (list 4 4) (list 1 1 1 1 1 1 1 1)) (list (list 4 4) (list 1 1 1 1 1 1 1 1)) (list (list 4 4) (list 1 1 1 1 1 1 1 1)))))
          )
    (print (gil::g-value-size sol (first playing)))
    (print "avant g-value")
    (setq pitch (nconc pitch (mapcar (lambda (n) (to-midicent (gil::g-values sol n))) playing)))
    (print "apres g-value")
    (print pitch)
    (list pitch tree)
    )
)

; this is not used but kept in case it is needed
; shuffles a list
; from https://gist.github.com/shortsightedsid/62d0ee21bfca53d9b69e
(defun list-shuffler (input-list &optional accumulator)
  "Shuffle a list using tail call recursion."
  (if (eq input-list nil)
      accumulator
      (progn
	(rotatef (car input-list) 
		 (nth (random (length input-list)) input-list))
	(list-shuffler (cdr input-list) 
				 (append accumulator (list (car input-list)))))))