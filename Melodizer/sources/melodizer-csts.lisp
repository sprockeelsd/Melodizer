(in-package :om)

;;;;;;;;;;;;;;;;;;;;;;
; SAMPLE CONSTRAINTS ;
;;;;;;;;;;;;;;;;;;;;;;

; Constraints are defined as instance of the class <constraint>, which is a container
; for a function that post the gecode constraints and its parameters.
; 
; To create a constraint, one must create an OM method (i.e. via defmethod!) that returns
; the constraint instance. 
; For readability reasons, the following constraints always return a function called "post-<cst>"
; that calls the <cst> by rearranging the args.


; Constraint REL-PULSE
; The specified positions must contain a note of duration with the specified relation 
; to the specified value (e.g. equal to 1/4 or greater than 1/16).


(defun all-different-notes (sp notes)
    (gil::g-distinct sp notes)
)

;this is from rhythm-box, previous constraints are for melodizer

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


(defun rel-pulse (lp rel value sp pos drt Dn s nmax)
    (let ((dp (* value s))
          pulses)

        (setq pulses (mapcar #'(lambda (x) (* x dp)) lp))
        (loop for x in pulses do
            (gil::g-count sp pos x gil::IRT_GQ 1)
            (loop for i from 0 below nmax do
                (let  ;b1: if the pos is equal to x...
                     ((b1 (gil::add-bool-var-expr sp (nth i pos) gil::IRT_EQ x))
                      ;b2: ... then the drt is related to dp
                      (b2 (gil::add-bool-var-expr sp (nth i drt) (rel-to-gil rel) dp))
                      ;b3: ... and the drt is greater than 0 (i.e. is a pulse)
                      (b3 (gil::add-bool-var-expr sp (nth i drt) gil::IRT_GR 0)))
                (gil::g-op sp b1 gil::BOT_IMP b2 1)
                (gil::g-op sp b1 gil::BOT_IMP b3 1))
        ))
    )
)

(defun post-rel-pulse (sp pos drt drt* N Dn s l duration nmin nmax input args-list)
    (rel-pulse (car args-list) (second args-list) (third args-list) sp pos drt Dn s nmax)
)

(defmethod! cst-rel-pulse (pos &optional (rel '=) (drt 1/4))
    :initvals '((0) = 1/4)
    :indoc '("a list of positions" "a relation operator (e.g =)" "a note duration (e.g. 1/4)")
    :icon 262
    :doc "
Constrains the output rhythm sequence to include a note of duration related to the drt.

- The relation is defined by the relation operator argument and is =, =/=, <, =<, > or >= .
- The drt is fraction (1/4 corresponds to a quarter note).
"
    (make-instance 'constraint
        :cstf #'post-rel-pulse
        :args (list pos rel drt))
)

; Constraint KEEP-PULSE
; The output rhythm must include at least the pulses contained in the input rhythm.

(defun keep-pulses (sp pos drt s nmax input)
    (let ((pulses (only-pulse-ticks (tree2posticks input s))))
        (loop for p in pulses do
            (gil::g-count sp pos p gil::IRT_GQ 1)
            (loop for i from 0 below nmax do
                (let ((b1 (gil::add-bool-var-expr sp (nth i pos) gil::IRT_EQ p))
                      (b2 (gil::add-bool-var-expr sp (nth i drt) gil::IRT_GR 0)))
                    (gil::g-op sp b1 gil::BOT_IMP b2 1))
            ))
))

(defun post-keep-pulses (sp pos drt drt* N Dn s l duration nmin nmax input args-list)
    (keep-pulses sp pos drt s nmax input)
)

(defmethod! cst-keep-pulses ()
    :icon 262
    :doc "
Constrains the output rhythm sequence to include a pulse where the input tree or voice does.
"
    (make-instance 'constraint
        :cstf #'post-keep-pulses
        :args nil)
)

; Constraint AT-MOST-PULSES
; The output rhythm contains only pulses from the input
(defun at-most-pulses (sp pos s duration nmax input)
    (let ((pulses (only-pulse-ticks (tree2posticks input s))))
        (loop for i from 0 below nmax do
            (gil::g-dom sp (nth i pos) (append pulses (list duration)))
        )
))

(defun post-at-most-pulses (sp pos drt drt* N Dn s l duration nmin nmax input args-list)
    (at-most-pulses sp pos s duration nmax input)
)

(defmethod! cst-at-most-pulses ()
    :icon 262
    :doc "
Constrains the output rhythm sequence to not include pulse where there is no pulse
in the input.
"
    (make-instance 'constraint
        :cstf #'post-at-most-pulses
        :args nil)
)

; Constraint AT-MOST-NB-NOTES
; The output rhythm has at most the same number of notes as the input.

(defun at-most-nb-notes (sp drt nmax input)
    (let ((drt>0 (gil::add-int-var-array sp nmax 0 1)))
        (loop for i from 0 below nmax do
            (let ((b1 (gil::add-bool-var-expr sp (nth i drt) gil::IRT_GR 0))
                  (b2 (gil::add-bool-var-expr sp (nth i drt>0) gil::IRT_EQ 1)))
                (gil::g-op sp b1 gil::BOT_EQV b2 1)))
    (gil::g-count sp drt>0 1 gil::IRT_LQ (n-pulses input)))
)

(defun post-at-most-nb-notes (sp pos drt drt* N Dn s l duration nmin nmax input args-list)
    (at-most-nb-notes sp drt nmax input)
)

(defmethod! cst-at-most-nb-notes ()
    :icon 262
    :doc "
Constrains the output rhythm sequence to have at most the same number of pulses as the input.
"
    (make-instance 'constraint
        :cstf #'post-at-most-nb-notes
        :args nil)
)

; Constraint AT-LEAST-NB-NOTES
; The output rhythm has at least the same number of notes as the input.

(defun at-least-nb-notes (sp drt nmax input)
    (let ((drt>0 (gil::add-int-var-array sp nmax 0 1)))
        (loop for i from 0 below nmax do
            (let ((b1 (gil::add-bool-var-expr sp (nth i drt) gil::IRT_GR 0))
                  (b2 (gil::add-bool-var-expr sp (nth i drt>0) gil::IRT_EQ 1)))
                (gil::g-op sp b1 gil::BOT_EQV b2 1)))
    (gil::g-count sp drt>0 1 gil::IRT_GQ (n-pulses input)))
)

(defun post-at-least-nb-notes (sp pos drt drt* N Dn s l duration nmin nmax input args-list)
    (at-least-nb-notes sp drt nmax input)
)

(defmethod! cst-at-least-nb-notes ()
    :icon 262
    :doc "
Constrains the output rhythm sequence to have at least the same number of pulses as the input.
"
    (make-instance 'constraint
        :cstf #'post-at-least-nb-notes
        :args nil)
)

; Constraint KEEP-NB-PULSES
; The output rhythm must have the same number of pulses as the input

(defun keep-nb-pulses (sp drt nmax input)
    (let ((note? (gil::add-int-var-array sp nmax 0 1)))
        (loop for i below nmax do
            (let ((b1 (gil::add-bool-var-expr sp (nth i drt) gil::IRT_GR 0))
                  (b2 (gil::add-bool-var-expr sp (nth i note?) gil::IRT_EQ 1)))
                (gil::g-op sp b1 gil::BOT_EQV b2 1)))
        (gil::g-count sp note? 1 gil::IRT_EQ (n-pulses input))
    )
)

(defun post-keep-nb-pulses (sp pos drt drt* N Dn s l duration nmin nmax input args-list)
    (keep-nb-pulses sp drt nmax input)
)

(defmethod! cst-keep-nb-pulses ()
    :icon 262
    :doc "
Constrains the output rhythm sequence to have the same number of pulses as the input.
"
    (make-instance 'constraint
        :cstf #'post-keep-nb-pulses
        :args nil)
)

; Constraint KEEP-NOTE-DRTS
; The notes in the output sequence have only durations taken from the intput.

(defmethod get-ratios ((input list)) (tree2ratio input))
(defmethod get-ratios ((input voice)) (get-ratios (tree input)))

(defun keep-note-drts (sp drt s duration nmax input)
    (let ((neg (loop for d from (- duration) to 0 collect d))
          (pos (remove-if-not #'(lambda (x) (> x 0)) (mapcar #'(lambda (x) (* s x)) (get-ratios input)))))
        
        (loop for i from 0 below nmax do 
            (gil::g-dom sp (nth i drt) (append neg pos)))
    )    
)

(defun post-keep-note-drts (sp pos drt drt* N Dn s l duration nmin nmax input args-list)
    (keep-note-drts sp drt s duration nmax input)
)

(defmethod! cst-keep-note-drts ()
    :icon 262
    :doc "
Constrains the notes in the output rhythm sequence to have only duration from the input.
"
    (make-instance 'constraint
        :cstf #'post-keep-note-drts
        :args nil)
)

; Constraint AT-LEAST-NOTES
; The output sequence contains at least the notes (i.e. events with positive duration) of the input. Notes from
; the input occur at least the same number of times in the output as in the input, but others can also occur.

(defun distinct (l &optional (acc nil))
    (cond
        ((null l) acc)
        ((member (car l) acc) (distinct (cdr l) acc))
        (t (distinct (cdr l) (append acc (list (car l)))))
    )
)

(defun group-drts (input)
    "Create a list of pairs (x drt) where x is the number of occurrences of drt in the input"
    (let ((drt (remove-if-not #'(lambda (x) (> x 0)) (get-ratios input))))
        (mapcar #'(lambda (x) (list x (count x drt))) (distinct drt)))
)

(defun at-least-notes (sp drt s nmax input)
    (let ((ticks (mapcar #'(lambda (x) (list (* s (first x)) (second x))) (group-drts input))))
    
    (loop for p in ticks do
        (gil::g-count sp drt (first p) gil::IRT_GQ (second p))))
)

(defun post-at-least-notes (sp pos drt drt* N Dn s l duration nmin nmax input args-list)
    (at-least-notes sp drt s nmax input)
)

(defmethod! cst-at-least-notes ()
    :icon 262
    :doc "
Constrains the output sequence to contain at least the note durations from the input. There can also be other notes with
other durations.
"
    (make-instance 'constraint
        :cstf #'post-at-least-notes
        :args nil)
)

; Constraint INSERT-PATTERN
; Constrains the output rhythm to include a user-defined pattern at each select pulse of the input.
; The pattern is adapted in length according to the duration of the note at the pulse.

(defun insert-pattern (p* l sp pos drt s nmax input) ;TODO: also insert input RESTS !!!!
    "p is the list of ratios defining the pattern to insert
     l is a list bools: l[i] <=> p is inserted at pulse i"
    (let ((ps (only-pulse-ticks (tree2posticks input s))) ;positions of pulses in ticks
          (ds (mapcar #'(lambda (x) (* s x)) (remove-if-not #'(lambda (x) (> x 0)) (get-ratios input)))) ;durations of pulses in ticks
          (p (mapcar #'(lambda (x) (* s x)) p*)) ;pattern in ticks
          (np (length p*))
          dp minp)
        (setq dp (apply '+ (mapcar #'abs p)))
        (setq minp (apply 'min (mapcar #'abs p)))
        (setq ns (length ps))

        (loop for i from 0 below (length ps) do
            (gil::g-count sp pos (nth i ps) gil::IRT_GQ 1)
            (let ((r (/ (nth i ds) dp)))
                (if (and (nth i l) (>= (* minp r) 1))
                    (loop for j from 0 below (- nmax np) do
                        (let ((b1 (gil::add-bool-var-expr sp (nth j pos) gil::IRT_EQ (nth i ps))))
                            (loop for k from 0 below np do
                                (let ((b2 (gil::add-bool-var-expr sp (nth (+ j k) drt) gil::IRT_EQ (floor (* r (nth k p))))))
                                    (gil::g-op sp b1 gil::BOT_IMP b2 1)))))
                  ;else
                 (loop for j from 0 below (- nmax dp) do
                        (let ((b1 (gil::add-bool-var-expr sp (nth j pos) gil::IRT_EQ (nth i ps)))
                                (b2 (gil::add-bool-var-expr sp (nth j drt) gil::IRT_EQ (nth i ds))))
                             (gil::g-op sp b1 gil::BOT_IMP b2 1)))))))
)

(defun xor (a b) (or (and a (not b)) (and (not a) b))) 

(defmethod pulses2bools ((l null) p)
    (loop for i from 0 below (length p) collect t))

(defmethod pulses2bools ((l list) p)
    (let ((l* (mapcar 'abs l)))
    (loop for i from 0 below (length p) collect (xor (member (+ i 1) l*) (< (first l) 0)))))

(defmethod pulses2bools ((l fixnum) p) (pulses2bools (list l) p))

(defun post-insert-pattern (sp pos drt drt* N Dn s l duration nmin nmax input args-list)
    (insert-pattern (first args-list) (pulses2bools (second args-list) (get-pulse-places input)) 
                    sp pos drt s nmax input)
)

(defmethod! cst-insert-pattern ((pattern list) &optional (pulses nil))
    :initvals (list '(1/8 1/8) nil)
    :indoc '("a pattern described by a list of ratios (e.g. (1/16 1/8 1/16)" 
             "a list of integers, the indices of the pulses")
    :icon 262
    :doc "
Constrains the output rhythm sequence to include the pattern <pattern> at each pulse specified in <pulses>. The 
duration of the pattern in dependant of the duration of the note at the pulses. For example, inserting 
p = (1/16 1/8 1/16) (i.e. the duration of p is 1/4) at a pulse where the note lasts 1/4, results in replacing
1/4 by (1/16 1/8 1/16). If note lasted 1/8, it would have been replaced by (1/32 1/16 1/32), and by (1/8 1/4 1/8)
if it lasted 1/2. If the result would include a note shorter than the shortest duration authorized in the space,
then the pattern is not inserted at this pulse.

- <pattern> must be a non null list expressed in ratios.
- <pulses> must be a list of integers, indices of the pulses in the input rhythm. 1 is the first pulse of the 
input rhythm, 2 the second pulse, and do on. The pattern is inserted at pulses denoted in <pulses>. <pulses> may
also be a list of negative integers, where -1 is the first pulse of the input rhythm. In that case, the pattern 
is not inserted  at pulses denoted in <pulses>. If <pulses> is nil, then the pattern is inserted at every pulse.
"
    (make-instance 'constraint
        :cstf #'post-insert-pattern
        :args (list pattern pulses))
)

