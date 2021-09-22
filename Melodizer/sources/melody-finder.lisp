;;;===============================
;;; Object to find a desired solution among the possible solutions
;;;===============================
(in-package :om)

;change the input from a voice to 2 inputs : the tree and the pitches?
(om::defclass! melody-finder () 
  ;attributes
  ((input-chords :accessor input-chords :initarg :input-chords :initform (make-instance 'voice) :documentation "the input chords on top of which the melody will be played")
    (input-rhythm :accessor input-rhythm :initarg :input-rhythm :initform nil :documentation "rhythm of the melody to be found")
    (search-engine :accessor search-engine :initarg :search-engine :initform nil :documentation "search engine for the CSP")
    (cspsol :accessor cspsol :initarg :cspsol :initform nil :documentation "solution of the CSP")
    (solution :accessor solution :initarg :solution :initform nil :documentation "solution of the CSP in the form of a voice object")
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
                      (setq init (voicemelodizer (input-chords (object self)) (input-rhythm (object self)))); get the search engine and the first solution of the CSP
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
                      (setf (solution (object self)) (search-next-voice (list (search-engine (object self)) (cspsol (object self))) (input-rhythm (object self))))
                    )
                  )
                    
                   ;(search-next-voice (list (search-engine (object self) (cspsol (object self)))))
                   ;do something (search-next from melodizer-csp to modify so it works here)
    )
  )
  ; return the editor:
  self
)



