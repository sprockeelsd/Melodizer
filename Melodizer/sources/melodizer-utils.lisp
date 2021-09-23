(in-package :om)

; converts a list of MIDI values to MIDIcent
(defun to-midicent (l)
  (if (null l) nil
    (cons (* 100 (first l)) (to-midicent (rest l)))))

; takes a rhythm tree as argument and returns the number of events in it
(defmethod get-events-from-rtree (rtree)
    (let ((l (first (rest rtree))) ; get the first element of the list
        (nb 0)); the number of events
        (dolist (bar l); for each bar
            ;(print "bar")
            ;(print bar)
            (dolist (elem (second bar)); count each event in the bar
                ;(print "elem")
                ;(print elem)
                (if (typep elem 'list); if the element of the bar is a list
                        (dolist (event (second elem))
                            (print event)
                            (setq nb (+ nb 1))
                        )
                    (setq nb (+ nb 1)); if it is just a number
                )
            )
        )
        (print nb)
        nb
    )
)
    

; taken from rhythm box (add link)

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