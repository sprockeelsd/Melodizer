(in-package :om)

; converts a list of MIDI values to MIDIcent
(defun to-midicent (l)
  (if (null l) nil
    (cons (* 100 (first l)) (to-midicent (rest l)))))

; shuffles a list
(defun list-shuffler (input-list &optional accumulator)
  "Shuffle a list using tail call recursion."
  (if (eq input-list nil)
      accumulator
      (progn
	(rotatef (car input-list) 
		 (nth (random (length input-list)) input-list))
	(list-shuffler (cdr input-list) 
				 (append accumulator (list (car input-list)))))))