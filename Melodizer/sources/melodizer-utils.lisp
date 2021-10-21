(in-package :mldz)

; converts a list of MIDI values to MIDIcent
(defun to-midicent (l)
  (if (null l) nil
    (cons (* 100 (first l)) (to-midicent (rest l)))))

; takes a rhythm tree as argument and returns the number of events in it (doesn't work with dotted notes for now)
(defmethod get-events-from-rtree (rtree)
    (let ((l (first (rest rtree))) ; get the first element of the list
        (nb 0)); the number of events
        (dolist (bar l); for each bar
            (dolist (elem (second bar)); count each event in the bar
                (if (typep elem 'list); if the element of the bar is a list
                    (dolist (event (second elem))
                        ;(print event)
                        (setq nb (+ nb 1))
                    )
                    (setq nb (+ nb 1)); if it is just a number
                )
            )
        )
        ;(print nb)
        nb
    )
)

;converts the Value of a note to its name
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
    

; taken from rhythm box (add link)
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

(defun get-bars (tree)
    "Get the bars of a tree (? bars)"
    (cadr tree)
)

(defun get-signature (bar)
    "Get the signature (N D) of a bar ((N D) ps)."
    (car bar)
)

(defmethod get-N ((x voice))
    "Get the N part of the signature of the tree of the input voice as if
    it was a long single bar."
    (apply '+ (mapcar #'(lambda(z) (first (get-signature z))) (get-bars (tree x))))
)

(defmethod get-D ((x voice))
    "Get the D part of the signature of the tree of the input voice."
    (second (get-signature (first (get-bars (tree x)))))
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