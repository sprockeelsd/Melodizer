(in-package :om)
;taken from rhythm box
(defclass constraint ()
    ((cst-function :initform nil :initarg :cstf :accessor cstf) 
    ;a constraint function that always have a space as 1st arg and a list as 2nd arg
    ;should this function have return values, they HAVE TO under the form of a list 
    ;of var-ref objects.

     (args :initform nil :initarg :args :accessor args)) 
    ;the list args to be the 2nd argument of the cst-function
)

(defun post-constraint (sp cst pos drt drt* N Dn s l duration nmin nmax input)
    "Call the constraint function with the rhythm-box data and the constraint arglist."
    (funcall (cstf cst) sp pos drt drt* N Dn s l duration nmin nmax input (args cst))
)

; <csts> is a list of <constraint> object
; <input> is either a voice object or a rhythm tree
; <s> is a fraction - the shortest authorized event duration
; <l> is a fraction - the longest authorized event duration
; <hd> is a float between 0 and 1 that indicates the maximum rhythmic density
(defmethod! melodizer (csts input &optional (s nil) (l nil) (hd 1.0))
    :initvals (list nil (make-instance 'voice) nil nil 1.0) 
    :indoc '("a list of constraints" "a voice or a rhythm tree" "the shortest desired note duration e.g. 1/16"
             "the longest desired note duration e.g. 1/2" "a float, the ratio of maximum density")
    :icon 921
    :doc "
Create a search space for the musical CSP and post general constraints that define general rhythm CSP.
"
    (let ((sp (gil::new-space))
          (N (get-N input))
          (Dn (get-D input))
          (s* (or (convert s) (shortest input)))
          duration nmin nmax csp pos drt drt* se)

        ;1: create the variables (depending on the input and options)
        (setq duration (* s* (/ N Dn)))
        (setq nmin (ceiling (or (or/ N (or* Dn l)) 1))) ;nmin = N/Dl or 1
        (setq nmax (round (* hd duration)))

        (setq drt (gil::add-int-var-array sp nmax (- duration) (or (or* l s*) duration)))
        (setq drt* (gil::add-int-var-array sp nmax 0 duration))
        (setq pos (gil::add-int-var-array sp nmax 0 duration))

        ;2: post the constraints
        ;general constraints
        
        ;2.1: drt*[i] = |drt[i]|
        (loop for i from 0 below nmax do
            (gil::g-abs sp (nth i drt) (nth i drt*)))

        ;2.2: sum(drt*) = duration
        (gil::g-linear sp (create-coeffs nmax 1) drt* gil::IRT_EQ duration) 

        ;2.3: count(drt, 0) <= nmax-nmin
        (gil::g-count sp drt 0 gil::IRT_LQ (- nmax nmin)) 

        ;2.4: pos[i] = pos[i-1]+drt[i-1]
        (loop for i from 1 below nmax do 
            (gil::g-linear sp '(1 1) (list (nth (- i 1) pos) (nth (- i 1) drt*)) gil::IRT_EQ (nth i pos)))
        ;2.4': pos[0] = 0
        (gil::g-rel sp (nth 0 pos) gil::IRT_EQ 0)

        ;pos[i] = duration => drt[i] = 0
        (loop for i from 0 below nmax do 
            (let ((b1 (gil::add-bool-var-expr sp (nth i pos) gil::IRT_EQ duration))
                   (b2 (gil::add-bool-var-expr sp (nth i drt) gil::IRT_EQ 0)))
                (gil::g-op sp b1 gil::BOT_IMP b2 1)))
        
        
        ;additional constraints
        (if (typep csts 'constraint)
            (post-constraint sp csts pos drt drt* N Dn s* l duration nmin nmax input)
            (loop for cst in csts do
                (post-constraint sp cst pos drt drt* N Dn s* l duration nmin nmax input)
        ))

        ;branching
        (gil::g-branch sp pos 0 0)
        (gil::g-branch sp drt 0 0)

        ;search engine
        (setq se (gil::search-engine sp nil))

        ;return
        (list se drt input s*)
))

(defmethod! search-next (l)
    :initvals (list nil) 
    :indoc '("a rhythm-space")
    :icon 330
    :doc "
Get the next solution for the rhythm csp described in the input rhythm-space.
"
    (let ((se (first l))
         (drt* (second l))
         (input (third l))
         (s* (nth 3 l))
         sol drt rtree)
        
        ;Get the values of the solution and build the tree
        (setq sol (gil::search-next se))
        (if (null sol) (error "No solution or no more solution."))
        (setq drt (gil::g-values sol drt*))
        (if (typep input 'voice)
            (setq rtree (build-rtree (tree input) drt s*))
            (setq rtree (build-rtree input drt s*)))

        ;return a voice object based on the input
        (if (typep input 'voice)
            (make-instance 'voice
                :tree rtree
                :chords (chords input)
                :tempo (tempo input)
                :legato (legato input)
                :ties (ties input))
            (make-instance 'voice
                :tree rtree
                :chords '((7200)))
    )
))