(in-package :om)


(defun get-proportions (bar)
    "Get the rhythmic proportions of a bar ((N D) ps)."
    (cadr bar)
)

(defun get-bars (tree)
    "Get the bars of a tree (? bars)"
    (cadr tree)
)

(defun convert(drt)
    "Convert a musical note duration to a tick duration."
    (and drt (/ (denominator drt) (numerator drt)))
)

(defun create-coeffs (n x)
    "Return a list of n elements equal to x."
    (cond ((<= n 0) nil)
          (t (cons x (create-coeffs (- n 1) x))))
)

(defun or* (x y)
    "Return x*y if neither are nil."
    (and x y (* x y))
)

(defun or/ (x y)
    "Return x/y if neither are nil."
    (and x y (/ x y))
)

;;;;;;;;;;;;;;;;;
; GET SIGNATURE ;
;;;;;;;;;;;;;;;;;

(defun get-signature (bar)
    "Get the signature (N D) of a bar ((N D) ps)."
    (car bar)
)

(defmethod get-N ((x voice))
    "Get the N part of the signature of the tree of the input voice as if
    it was a long single bar."
    (apply '+ (mapcar #'(lambda(z) (first (get-signature z))) (get-bars (tree x))))
)

(defmethod get-N ((x list))
    "Get the N part of the signature of the input tree as if it was a long 
    single bar."
    (apply '+ (mapcar #'(lambda(z) (first (get-signature z))) (get-bars x)))
)

(defmethod get-D ((x voice))
    "Get the D part of the signature of the tree of the input voice."
    (second (get-signature (first (get-bars (tree x)))))
)

(defmethod get-D ((x list))
    "Get the D part of the signature of the tree of the input voice."
    (second (get-signature (first (get-bars x))))
)

;;;;;;;;;;;;;;;;;;;;;
; SHORTEST DURATION ;
;;;;;;;;;;;;;;;;;;;;;

(defun smallest-in-bar (bar)
    "Return the shortest note duration of the bar."
    (let ((sign (get-signature bar))
          (ps (mapcar #'abs (get-proportions bar))))
        (* (/ (second sign) (first sign)) (apply '+ ps)))
)

(defun smallest (x)
    "Return the shortest note duration of the input tree."
    (apply 'max (loop for b in (get-bars x) collect
        (smallest-in-bar b)))
)

(defmethod shortest ((x voice))
    "Return the shortest note duration of the tree of the input voice."
    (smallest (tree x))
)

(defmethod shortest ((x list))
    "Return the shortest note duration of the input tree."
    (smallest x) 
)


;;;;;;;;;;;;;;;;;;;;;;;;;;
; POS IN TICKS FROM TREE ;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod tree2posticks ((tree list) s)
"Convert the tree to a list of event positions in ticks.

WARNING: when using this method, position are considered starting from 1 to be able to
represent the fact the first element can be a rest."
    (let ((ratios (tree2ratio tree)))
        (loop for i from 0 below (length ratios) collect
            (let ((x 0))
                (loop for j from 0 below i do
                    (setq x (+ x (* s (abs (nth j ratios))))))
                (if (> (nth i ratios) 0) (+ x 1) (- (+ x 1)))))
))

(defmethod tree2posticks ((tree voice) s) (tree2posticks (tree tree) s))

(defun only-pulse-ticks (ticks)
"Filter a list of positions in ticks to keep only the positions of pulses."
    (cond ((null ticks) nil)
          ((< (car ticks) 0) (only-pulse-ticks (cdr ticks)))
          (t (cons (- (car ticks) 1) (only-pulse-ticks (cdr ticks)))))
)

(defun only-rest-ticks (ticks)
"Filter a list of positions in ticks to keep only the positions of rests."
    (cond ((null ticks) nil)
          ((> (car ticks) 0) (only-pulse-ticks (cdr ticks)))
          (t (cons (- (abs (car ticks)) 1) (only-pulse-ticks (cdr ticks)))))
)

;;;;;;;;;;;;;;;;;;;;;;;
; BUILD TREE FROM SOL ;
;;;;;;;;;;;;;;;;;;;;;;;

(defun 0-erase (l)
"Filter a list to keep only the non-zero elements."
    (cond 
        ((null l) nil)
        ((= 0 (car l)) (0-erase (cdr l)))
        (t (cons (car l) (0-erase (cdr l))))
    )
)

(defun build-rtree (tree drt s)
"Build a rhythm tree from a list of durations in ticks."
    (let ((signs (mapcar #'get-signature (get-bars tree)))
           drts)

        (setq drts (mapcar #'(lambda (x) (/ x s)) (0-erase drt)))
        (reducetree (mktree drts signs)))
)
