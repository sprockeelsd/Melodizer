(in-package :fuxcp)

;::::::::::::::::::::::::::::::::::::::::::::::::;;;;;;;;;;;;;
;; Most of this code comes from the Melodizer2.0 libraries. ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; create a list from min to max by step
(defun range (max &key (min 0) (step 1))
   (loop for n from min below max by step
      collect n))

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
(defun get-scale (&optional (mode "ionian (major)"))
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
        ((string-equal mode "borrowed")
            (list 5 4 2 1)
        )
    )
)

(defun get-chord (quality)
    (cond
        ((string-equal quality "Major")
            (list 4 3 5)
        )
        ((string-equal quality "Minor")
            (list 3 4 5)
        )
        ((string-equal quality "Augmented")
            (list 4 4 4)
        )
        ((string-equal quality "Diminished")
            (list 3 3 6)
        )
        ((string-equal quality "Major 7")
            (list 4 3 4 1)
        )
        ((string-equal quality "Minor 7")
            (list 3 4 3 2)
        )
        ((string-equal quality "Dominant 7" )
            (list 4 3 3 2)
        )
        ((string-equal quality "Minor 7 flat 5")
            (list 3 3 4 2)
        )
        ((string-equal quality "Diminished 7")
            (list 3 3 3 3)
        )
        ((string-equal quality "Minor-major 7")
            (list 3 4 4 1)
        )

        ; TODO manage chords
        ((string-equal quality "Major 9")
            (list 3 4 5)
        )
        ((string-equal quality "Minor 9")
            (list 4 3 5)
        )
        ((string-equal quality "9 Augmented 5")
            (list 3 4 5)
        )
        ((string-equal quality "9 flatted 5")
            (list 3 4 5)
        )
        ((string-equal quality "7 flat 9")
            (list 4 3 5)
        )
        ((string-equal quality "Augmented 9")
            (list 3 4 5)
        )
        ((string-equal quality "Minor 11")
            (list 3 4 5)
        )
        ((string-equal quality "Major 11")
            (list 4 3 5)
        )
        ((string-equal quality "Dominant 11")
            (list 3 4 5)
        )
        ((string-equal quality "Dominant # 11")
            (list 4 3 5)
        )
        ((string-equal quality "Major # 11")
            (list 3 4 5)
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

; Create push and pull list from a voice object
(defun create-push-pull (input-chords quant)
    (let (temp
         (next 0)
         (push (list))
         (pull (list '-1))
         (playing (list))
         (tree (om::tree input-chords))
         (pitch (to-pitch-list (om::chords input-chords))))
         (setq tree (second tree))
         (loop :for i :from 0 :below (length tree) :by 1 :do
            (setq temp (read-tree (make-list quant :initial-element -1) (make-list quant :initial-element -1) (make-list quant :initial-element -1) (second (first (second (nth i tree)))) pitch 0 quant next))
            (setq push (append push (first temp)))
            (setq pull (append pull (second temp)))
            (setq playing (append playing (third temp)))
            (setf next (fourth temp))
         )
         (list push pull playing))
)

; <tree> is the rhythm tree to read
; <pitch> is the ordered list of pitch
; <pos> is the next position in push to add values
; <length> is the current duration of a note to add
; <next> is the index in pitch of the next notes we will add
;recursive function to read a rhythm tree and create push and pull
(defun read-tree (push pull playing tree pitch pos length next)
    (progn
        (setf length (/ length (length tree)))
        (loop :for i :from 0 :below (length tree) :by 1 :do
            (if (typep (nth i tree) 'list)
                (let (temp)
                    (setq temp (read-tree push pull playing (second (nth i tree)) pitch pos length next))
                    (setq push (first temp))
                    (setq pull (second temp))
                    (setq playing (third temp))
                    (setf next (fourth temp))
                    (setf pos (fifth temp))
                )
                (progn
                    (setf (nth pos push) (nth next pitch))
                    (loop :for j :from pos :below (+ pos (* length (nth i tree))) :by 1 :do
                         (setf (nth j playing) (nth next pitch))
                    )
                    (setf pos (+ pos (* length (nth i tree))))
                    (setf (nth (- pos 1) pull) (nth next pitch))
                    (setf next (+ next 1))
                )
            )
        )
        (list push pull playing next pos)
    )
)

; <input-chords> is the voice objects for the chords
; <quantOrig> quantification used by melodizer
; Return a list in which each element i represent a note starting at a time i*quant
; -1 means no note starting at that time, a chord object means multiple note starting
(defun create-push (input-chords quantOrig)
    (let ((note-starting-times (voice-onsets input-chords))
          (quant (/ (second (first (om::tempo input-chords))) (/ quantOrig 16)))
          (tree (om::tree input-chords))
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
         (setf note-starting-times (mapcar (lambda (n) (/ n quant)) note-starting-times)) ; dividing note-starting-times by quant
         (setf note-dur-times (mapcar (lambda (n) (mapcar (lambda (m) (/ m quant)) n)) note-dur-times)) ; dividing note-dur-times by quant
         (loop :for j :from 0 :below (length note-starting-times) :by 1 :do
             (setq note-stopping-times (nconc note-stopping-times (list (mapcar (lambda (n) (+ n (nth j note-starting-times))) (nth j note-dur-times))))) ; Adding note-starting-times to note-dur-times to get note-stopping-times
        )
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
    )
)

; reformat a scale to be a canvas of pitch and not intervals
(defun adapt-scale (scale)
    (let ((major-modified (list (first scale))))
         (loop :for i :from 1 :below (length scale) :by 1 :do
            (setq major-modified (nconc major-modified (list (+ (nth i scale) (nth (- i 1) major-modified)))))
         )
    (return-from adapt-scale major-modified)
    )
)

; build the list of acceptable pitch based on the scale and a key offset
(defun build-scaleset (scale offset)
    (let ((major-modified (adapt-scale scale))
          (scaleset (list)))
        (loop :for octave :from -1 :below 11 :by 1 append
              (setq scaleset (nconc scaleset (mapcar (lambda (n) (+ (+ n (* octave 12)) offset)) major-modified)))
        )
        (setq scaleset (remove-if 'minusp scaleset))
        ;; tibo: remove notes higher than 127
        (setq scaleset (remove 127 scaleset :test #'<))
    )
)

; build the list of acceptable pitch based on the scale and a key offset
(defun build-notesets (chord offset)
    (let ((chord-modified (adapt-scale chord))
          (notesets (list)))
        (loop :for i :from 0 :below (length chord-modified) :by 1 :do
            (setq noteset (list))
            (loop :for octave :from -1 :below 11 :by 1 append
                  (setq noteset (nconc noteset (list (+ (+ (nth i chord-modified) (* octave 12)) offset))))
            )
            (setq noteset (remove-if 'minusp noteset))
            (setq notesets (nconc notesets (list noteset)))
        )
        notesets
    )
)



; <chords> a list of chord object
; Return the list of pitch contained in chords in midi format
(defun to-pitch-list (chords)
     (loop :for n :from 0 :below (length chords) :by 1 collect (to-midi (om::lmidic (nth n chords))))
)


; Getting a list of chords and a rhythm tree from the playing list of intvar
(defun build-voice (sol push pull bars quant tempo)
    (let ((p-push (list))
          (p-pull (list))
          (chords (list))
          (tree (list))
          (ties (list))
          (prev 0)
          )

    (setq p-pull (nconc p-pull (mapcar (lambda (n) (to-midicent (gil::g-values sol n))) pull)))
    (setq p-push (nconc p-push (mapcar (lambda (n) (to-midicent (gil::g-values sol n))) push)))

    (setq count 1)
    (loop :for b :from 0 :below bars :by 1 :do
        (if (not (nth (* b quant) p-push))
            (setq rest 1)
            (setq rest 0)
        )
        (setq rhythm (list))
        (loop :for q :from 0 :below quant :by 1 :do
            (setq i (+ (* b quant) q))
            (cond
                ((nth i p-push)
                     ; if rhythm impulse
                     (progn
                        (setq durations (list))
                        (loop :for m :in (nth i p-push) :do
                            (setq j (+ i 1))
                            (loop
                                (if (nth j p-pull)
                                    (if (find m (nth j p-pull))
                                        (progn
                                            (setq dur (* (floor 60000 (* tempo quant)) (- j i)))
                                            (setq durations (nconc durations (list dur)))

                                            (return)
                                        )
                                    )
                                )
                                (incf j)
                            )
                        )
                        (setq chord (make-instance 'chord :LMidic (nth i p-push) :Ldur durations))
                        (setq chords (nconc chords (list chord)))
                        (cond
                            ((= rest 1)
                                (progn
                                    (setq rhythm (nconc rhythm (list (* -1 count))))
                                    (setq rest 0)))
                            ((/= q 0)
                                (setq rhythm (nconc rhythm (list count))))
                        )
                        (setq count 1))
                )
                ; else
                (t (setq count (+ count 1)))
            )
        )
        (if (= rest 1)
            (setq rhythm (nconc rhythm '(list (* -1 count))))
            (setq rhythm (nconc rhythm (list count)))
        )
        (setq count 0)
        (setq rhythm (list '(4 4) rhythm))

        (setq tree (nconc tree (list rhythm)))
    )
    (setq tree (list '? tree))

    (list chords tree)
    )
)

(defun build-chord-seq (sol push pull bars quant tempo)
    (let ((p-push (list))
          (p-pull (list))
          (chords (list))
          (durations (list))
          (onsets (list)))

        (setq p-pull (nconc p-pull (mapcar (lambda (n) (to-midicent (gil::g-values sol n))) pull)))
        (setq p-push (nconc p-push (mapcar (lambda (n) (to-midicent (gil::g-values sol n))) push)))

        (loop :for i :from 0 :below (+ (* bars quant) 1) :do
            (if (nth i p-push)
                (progn
                    (setq onset (* (/ 60000 (* tempo (/ quant 4))) i))
                    (setq duration (list))
                    (loop :for m :in (nth i p-push) :do
                        (setq j (+ i 1))
                        (loop
                            (if (nth j p-pull)
                                (if (find m (nth j p-pull))
                                    (progn
                                        (setq dur (* (/ 60000 (* tempo (/ quant 4))) (- j i)))
                                        (setq duration (nconc duration (list dur)))

                                        (return)
                                    )
                                )
                            )
                            (incf j)
                        )
                    )
                    (setq chords (nconc chords (list (nth i p-push))))
                    (setq durations (nconc durations (list duration)))
                    (setq onsets (nconc onsets (list onset)))
                )
            )
        )

        (list chords onsets durations)
    )
)

;return T if the two list have the same elements (order doesn't matter)
(defun compare (l1 l2)
  (and (subsetp l1 l2) (subsetp l2 l1)))

; return the quant value based on the index selected
(defun get-quant (str)
  (cond ((string= str "1 bar") 1)
    ((string= str "1/2 bar") 2)
    ((string= str "1 beat") 4)
    ((string= str "1/2 beat") 8)
    ((string= str "1/4 beat") 16)
    ((string= str "1/8 beat") 32)
    ((string= str "1/3 bar") 3)
    ((string= str "1/6 bar") 6)
    ((string= str "1/3 beat") 12)
    ((string= str "1/6 beat") 24)
    ((string= str "1/12 beat") 48)
    ((not str) 192))
)

; return the quant value based on the index selected
(defun get-length (str)
  (cond ((string= str "1 bar") 192)
    ((string= str "1/2 bar") 96)
    ((string= str "1 beat") 48)
    ((string= str "1/2 beat") 24)
    ((string= str "1/4 beat") 12)
    ((string= str "1/8 beat") 6)
    ((string= str "1/3 bar") 64)
    ((string= str "1/6 bar") 32)
    ((string= str "1/3 beat") 16)
    ((string= str "1/6 beat") 8)
    ((string= str "1/12 beat") 4)
    ((not str) 1))
)

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