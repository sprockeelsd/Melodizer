;;;===============================
;;; Object to find a desired solution among the possible solutions
;;;===============================
(in-package :om)

;change the input from a voice to 2 inputs : the tree and the pitches?
(om::defclass! melody-finder () 
  ;attributes
  ((input-chords :accessor input-chords :initarg :input-chords :initform (make-instance 'voice) :documentation "the input chords on top of which the melody will be played")
    (search-engine :accessor search-engine :initarg :search-engine :initform nil :documentation "search engine for the CSP")
    (solution :accessor solution :initarg :solution :initform nil :documentation "solution of the CSP in the form of a voice object")
    (cspsol :accessor cspsol :initarg :cspsol :initform nil :documentation "solution of the CSP")
    (slot2 :accessor slot2 :initarg :slot2 :initform nil :documentation "slot 2") ; to do some testing
    ;(slot2 :accessor slot2 :initarg :slot2 :initform nil :documentation "slot 2")
  )
  (:icon 1)
  (:doc "This class implements melody-finder.
        It takes as input a voice object that is a possible solution for the CSP
        and allows to search among the solutions.")
)


    
;;; OBJECT EDITOR 
(defclass my-editor (om::editorview) ())

(defmethod om::class-has-editor-p ((self melody-finder)) t)
(defmethod om::get-editor-class ((self melody-finder)) 'my-editor)

(defmethod om::om-draw-contents ((view my-editor))
  (let* ((object (om::object view)))
    (om::om-with-focused-view 
      view
      ;;; DRAW SOMETHING ?
    )
  )
)


(defmethod initialize-instance ((self my-editor) &rest args)

  ; To access the melody-finder object, (object self)
  
  ;;; do what needs to be done by default
  (call-next-method)
  
  (om::om-add-subviews 
    self
    ; button to start or restart the search, not sure if I will keep it here
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 100 10) ; position
      (om::om-make-point 80 20) ; size
      "Start"
      :di-action #'(lambda (b) 
                    #| (dolist (e (chords (input-chords (object self))))
                      (print (lmidic e))
                    ) |#
                    (let init 
                      (setq init (voicemelodizer (input-chords (object self)))); get the search engine and the first solution of the CSP
                      ; update the fields of the object to their new value
                      (setf (search-engine (object self)) (first init))
                      (setf (cspsol (object self)) (second init))
                      (print "csp constructed")
                    )
                  )
      ;:di-action #'(lambda (b)
      ;             (print "Restarting the search with the new constraints"))
    )
    ; button to find the next solution
    (om::om-make-dialog-item 
      'om::om-button
      (om::om-make-point 10 10) ; position
      (om::om-make-point 80 20) ; size
      "Next"
      :di-action #'(lambda (b)
                    (print "Searching for the next solution")
                    (let sol 
                      (setf (solution (object self)) (search-next-voice (list (search-engine (object self)) (cspsol (object self)))))
                    )
                  )
                    
                   ;(search-next-voice (list (search-engine (object self) (cspsol (object self)))))
                   ;do something (search-next from melodizer-csp to modify so it works here)
    )
  )
  ; return the editor:
  self
)

(defmethod voicemelodizer ( input &optional (key 60.0) (mode 0.0))
    :initvals (list (make-instance 'voice) 60.0 0.0)
    :indoc '("a voice object" 
            "the key" 
            "the mode"
            )
    :icon 921
    :doc "TODO : add documentation once the code does something interesting"
    (let ((sp (gil::new-space))
        pitch dfs)

        ; first, create the variables
        (setq pitch (gil::add-int-var-array sp 8 60 72))

        ; then, post the constraints
        (in-tonality sp pitch 60 0)

        ;(all-different-notes sp pitch)

        ;(interval-between-adjacent-notes sp pitch)
        
        ; branching
        ; in order branching
        (gil::g-branch sp pitch 0 0)
        ;random branching
        ;(gil::g-branch-random sp pitch 1 2)

        ; search engine
        (setq se (gil::search-engine sp nil))

        ; return
        (list se pitch)
    )
)

(defmethod search-next-voice (l)
    :initvals (list nil) 
    :indoc '("a musical-space")
    :icon 330
    :doc "
Get the next solution for the csp described in the input musical-space.
"
    (let ((se (first l))
         (pitch* (second l))
         sol pitches)
        
        ;Get the values of the solution

        (setq sol (gil::search-next se))
        (if (null sol) (error "No solution or no more solution."))
        ;(print gil::g-values pitch*)
        (setq pitches (to-midicent (gil::g-values sol pitch*)))
        (print "pitches" )
        (print pitches)

        ;return a voice object
        (make-instance 'voice
            :tree (mktree (list 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4) (list 4 4))
            :chords pitches
            :tempo 72
        )
        ;(list (mktree (list 1/4 1/4 1/4 1/4 1/4 1/4 1/4 1/4) (list 4 4)) pitches 72)
    )
)

;(defmethod get-slot-2 ((self melody-finder)) 
;  (slot2 self)
;)




    







